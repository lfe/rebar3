%%%% LFE source formatter — Arc A3 (S3: comment/trivia emission, blank lines, edges).
%%%% Pipeline: r3lfe_format_lexer -> r3lfe_format_cst -> iolist.
%%%%
%%%% Output is LF-only (\n). CRLF input is normalised to LF because A1 lexes
%%%% \r into whitespace tokens, which the CST drops. Empty files produce empty
%%%% output (no trailing \n; there are no forms to end). All other output ends
%%%% with exactly one \n. Comment order is preserved.
-module(r3lfe_formatter).

-export([format/1]).

-define(WIDTH, 80).  %% column limit (§2.1)

%%====================================================================
%% Exported API
%%====================================================================

%% Dialyzer infers a concrete nested-list type for the iolist return; suppress
%% the underspecs warning since iolist() is the correct public abstraction.
-dialyzer({no_underspecs, format/1}).
-spec format(binary() | string()) -> {ok, iolist()} | {error, term()}.
format(Input) ->
    case r3lfe_format_lexer:tokens(Input) of
        {ok, Tokens} ->
            case r3lfe_format_cst:parse(Tokens) of
                {ok, Doc} -> {ok, render_document(Doc)};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% Internal: document-level layout
%%====================================================================

-spec render_document(r3lfe_format_cst:cst_document()) -> iolist().
render_document(Doc) ->
    Nodes    = r3lfe_format_cst:document_children(Doc),
    DangItems = r3lfe_format_cst:document_dangling(Doc),
    Parts    = render_toplevel(Nodes, true, []),
    DangIO   = emit_toplevel_dangling(DangItems),
    lists:reverse([DangIO | Parts]).

%% render_toplevel: emit each top-level node with its leading trivia and final \n.
-spec render_toplevel([r3lfe_format_cst:cst_node()], boolean(), iolist()) -> iolist().
render_toplevel([], _IsFirst, Acc) ->
    Acc;
render_toplevel([Node | Rest], IsFirst, Acc) ->
    LeadIO = emit_leading_trivia(r3lfe_format_cst:leading(Node), "", IsFirst),
    {NodeIO, NodeCol} = print_node(Node, 0),
    {TrailIO, _Col}   = emit_trailing(r3lfe_format_cst:trailing(Node), NodeCol),
    Part = [LeadIO, NodeIO, TrailIO, "\n"],
    render_toplevel(Rest, false, [Part | Acc]).

%%====================================================================
%% Internal: main printer — flat vs broken decision
%%====================================================================

-type width() :: non_neg_integer() | infinity.

%% print_node: print a node starting at column Col.
%% Returns {IO, NewCol} where NewCol is the column after the last printed char.
%% Flat if: no multi-line token, fits in WIDTH, and no trivia that would be
%% lost in flat mode (dangling on this node, or any trivia on any descendant).
%% The node's own leading/trailing are always emitted by the parent context and
%% do NOT prevent flat rendering.
-spec print_node(r3lfe_format_cst:cst_node(), non_neg_integer()) ->
          {iolist(), non_neg_integer()}.
print_node(Node, Col) ->
    W = flat_width(Node),
    Fits = W =/= infinity
           andalso Col + W =< ?WIDTH
           andalso not has_internal_trivia(Node),
    case Fits of
        true  -> {flat_render(Node), Col + W};
        false -> print_broken(Node, Col)
    end.

%%====================================================================
%% Internal: broken printing
%%====================================================================

%% print_broken: broken form for containers, prefixed, and multi-line leaves.
-spec print_broken(r3lfe_format_cst:cst_node(), non_neg_integer()) ->
          {iolist(), non_neg_integer()}.
print_broken(Node, Col) ->
    case r3lfe_format_cst:type(Node) of
        T when T =:= list; T =:= tuple; T =:= map; T =:= binary; T =:= eval ->
            print_broken_container(Node, Col);
        prefixed ->
            PfxText = r3lfe_format_lexer:text(r3lfe_format_cst:prefix(Node)),
            [Inner]  = r3lfe_format_cst:children(Node),
            {InnerIO, InnerCol} = print_node(Inner, Col + length(PfxText)),
            {[PfxText, InnerIO], InnerCol};
        _ ->
            %% Leaf with multi-line token (tqstring/tqbstring): emit verbatim.
            Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Node)),
            {Text, col_after_text(Text, Col)}
    end.

%% print_broken_container: generic +2 hanging rule (§3.2) with trivia emission.
%%
%% When the head child has a leading comment the opener stands alone on its
%% line and ALL children (including the head) are emitted via print_rest_loop
%% at Indent.  This keeps the comment inside the parens so it re-attaches to
%% the head on reparse — required for idempotency (fix1).
%%
%%   HEAD-COMMENT path:
%%     <OPEN>
%%     <C+2>[;; comment]<child0>[trailing]
%%     <C+2>[child1-leading]<child1>[trailing]  …
%%     [dangling / C]<CLOSE>
%%
%%   NORMAL path (head has no leading comment):
%%     <OPEN><child0>[trailing]
%%     <C+2>[child1-leading]<child1>[trailing]  …
%%     [dangling / C]<CLOSE>
-spec print_broken_container(r3lfe_format_cst:cst_node(), non_neg_integer()) ->
          {iolist(), non_neg_integer()}.
print_broken_container(Node, C) ->
    Open      = r3lfe_format_lexer:text(r3lfe_format_cst:open(Node)),
    Close     = r3lfe_format_lexer:text(r3lfe_format_cst:close(Node)),
    Children  = r3lfe_format_cst:children(Node),
    Dangling  = r3lfe_format_cst:dangling(Node),
    Indent    = C + 2,
    IndentStr = lists:duplicate(Indent, $\s),
    CIndStr   = lists:duplicate(C, $\s),
    CloseLen  = length(Close),
    case Children of
        [] ->
            %% Empty container
            case Dangling of
                [] ->
                    {[Open, Close], C + length(Open) + CloseLen};
                _ ->
                    DangIO = emit_dangling(Dangling, IndentStr),
                    {[Open, DangIO, "\n", CIndStr, Close], C + CloseLen}
            end;
        [Head | RestChildren] ->
            case head_has_leading_comment(Head) of
                true ->
                    %% Opener alone on its line; all children at Indent (fix1).
                    {AllIO, LastCol} = print_rest_loop([Head | RestChildren],
                                                       Indent, IndentStr, true),
                    {CloseIO, CloseCol} = close_section(Dangling, LastCol, Indent,
                                                        IndentStr, C, CIndStr, Close),
                    {[Open, AllIO, CloseIO], CloseCol};
                false ->
                    %% Head on opener line; head-leading blanks are dropped.
                    HeadLeadIO = emit_head_leading(
                                   r3lfe_format_cst:leading(Head), CIndStr),
                    {HeadIO, HeadCol}  = print_node(Head, C + length(Open)),
                    {HeadTrailIO, HTC} = emit_trailing(
                                           r3lfe_format_cst:trailing(Head), HeadCol),
                    case RestChildren of
                        [] ->
                            {CloseIO, CloseCol} = close_section(Dangling, HTC, Indent,
                                                                 IndentStr, C, CIndStr,
                                                                 Close),
                            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, CloseIO], CloseCol};
                        _ ->
                            {RestIO, LastCol} = print_rest_loop(RestChildren, Indent,
                                                                 IndentStr, true),
                            {CloseIO, CloseCol} = close_section(Dangling, LastCol, Indent,
                                                                 IndentStr, C, CIndStr,
                                                                 Close),
                            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, RestIO, CloseIO],
                             CloseCol}
                    end
            end
    end.

%% head_has_leading_comment: true iff the head child's leading contains a comment.
%% Blanks alone do not trigger the head-comment path.
-spec head_has_leading_comment(r3lfe_format_cst:cst_node()) -> boolean().
head_has_leading_comment(Node) ->
    lists:any(fun({comment, _}) -> true; (_) -> false end,
              r3lfe_format_cst:leading(Node)).

%% close_section: emit dangling then close, or just close hugging last child.
-spec close_section([r3lfe_format_cst:trivia()], non_neg_integer(),
                    non_neg_integer(), string(), non_neg_integer(), string(), string()) ->
          {iolist(), non_neg_integer()}.
close_section([], LastCol, _Indent, _IndStr, _C, _CIndStr, Close) ->
    {Close, LastCol + length(Close)};
close_section(Dangling, _LastCol, _Indent, IndStr, C, CIndStr, Close) ->
    DangIO = emit_dangling(Dangling, IndStr),
    {[DangIO, "\n", CIndStr, Close], C + length(Close)}.

%% print_rest_loop: emit children [c1..cN] each preceded by \n+Indent.
%% IsFirst=true suppresses the leading blank of the first rest child.
-spec print_rest_loop([r3lfe_format_cst:cst_node()], non_neg_integer(),
                      string(), boolean()) ->
          {iolist(), non_neg_integer()}.
print_rest_loop([Child | Rest], Indent, IndentStr, IsFirst) ->
    LeadIO = emit_child_leading(r3lfe_format_cst:leading(Child), IndentStr, IsFirst),
    {ChildIO, ChildCol}  = print_node(Child, Indent),
    {TrailIO, TrailCol}  = emit_trailing(r3lfe_format_cst:trailing(Child), ChildCol),
    case Rest of
        [] ->
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO], TrailCol};
        _ ->
            {RestIO, LastCol} = print_rest_loop(Rest, Indent, IndentStr, false),
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO | RestIO], LastCol}
    end.

%%====================================================================
%% Internal: flat rendering (used when node passes flat check)
%%====================================================================

-spec flat_render(r3lfe_format_cst:cst_node()) -> iolist().
flat_render(Node) ->
    case r3lfe_format_cst:type(Node) of
        T when T =:= symbol; T =:= number; T =:= string; T =:= char ->
            r3lfe_format_lexer:text(r3lfe_format_cst:open(Node));
        T when T =:= list; T =:= tuple; T =:= map; T =:= binary; T =:= eval ->
            Open  = r3lfe_format_lexer:text(r3lfe_format_cst:open(Node)),
            Close = r3lfe_format_lexer:text(r3lfe_format_cst:close(Node)),
            case r3lfe_format_cst:children(Node) of
                []       -> [Open, Close];
                Children ->
                    [Open, lists:join(" ", [flat_render(C) || C <- Children]), Close]
            end;
        prefixed ->
            PfxText = r3lfe_format_lexer:text(r3lfe_format_cst:prefix(Node)),
            [Inner]  = r3lfe_format_cst:children(Node),
            [PfxText | flat_render(Inner)]
    end.

%%====================================================================
%% Internal: flat-width calculation
%%====================================================================

-spec flat_width(r3lfe_format_cst:cst_node()) -> width().
flat_width(Node) ->
    case r3lfe_format_cst:type(Node) of
        T when T =:= symbol; T =:= number; T =:= string; T =:= char ->
            Tok = r3lfe_format_cst:open(Node),
            case r3lfe_format_lexer:kind(Tok) of
                K when K =:= tqstring; K =:= tqbstring -> infinity;
                _                                       ->
                    length(r3lfe_format_lexer:text(Tok))
            end;
        T when T =:= list; T =:= tuple; T =:= map; T =:= binary; T =:= eval ->
            OpenLen  = length(r3lfe_format_lexer:text(r3lfe_format_cst:open(Node))),
            CloseLen = length(r3lfe_format_lexer:text(r3lfe_format_cst:close(Node))),
            Children = r3lfe_format_cst:children(Node),
            case Children of
                [] -> OpenLen + CloseLen;
                _  ->
                    Widths = [flat_width(C) || C <- Children],
                    Spaces = length(Children) - 1,
                    add_widths(OpenLen + CloseLen + Spaces, sum_widths(Widths, 0))
            end;
        prefixed ->
            PfxLen  = length(r3lfe_format_lexer:text(r3lfe_format_cst:prefix(Node))),
            [Inner] = r3lfe_format_cst:children(Node),
            add_widths(PfxLen, flat_width(Inner))
    end.

-spec sum_widths([width()], non_neg_integer()) -> width().
sum_widths([], Acc)            -> Acc;
sum_widths([infinity | _], _)  -> infinity;
sum_widths([W | Rest], Acc)    -> sum_widths(Rest, Acc + W).

-spec add_widths(non_neg_integer(), width()) -> width().
add_widths(_, infinity) -> infinity;
add_widths(A, B)        -> A + B.

%%====================================================================
%% Internal: trivia emission helpers
%%====================================================================

%% emit_leading_trivia: emit leading trivia items at IndentStr.
%% DropFirstBlank=true suppresses the first blank (doc-start / after opener).
-spec emit_leading_trivia([r3lfe_format_cst:trivia()], string(), boolean()) -> iolist().
emit_leading_trivia([], _IndStr, _DropFirstBlank) ->
    [];
emit_leading_trivia([blank | Rest], IndStr, true) ->
    emit_leading_trivia(Rest, IndStr, false);
emit_leading_trivia([blank | Rest], IndStr, false) ->
    ["\n" | emit_leading_trivia(Rest, IndStr, false)];
emit_leading_trivia([{comment, Tok} | Rest], IndStr, _Drop) ->
    Text = r3lfe_format_lexer:text(Tok),
    [IndStr, Text, "\n" | emit_leading_trivia(Rest, IndStr, false)].

%% emit_head_leading: leading trivia for the head child, emitted before the opener.
%% Blanks are always dropped (head is on the opener line; no blank between leading
%% comments and the opener itself is unusual enough to discard in generic mode).
-spec emit_head_leading([r3lfe_format_cst:trivia()], string()) -> iolist().
emit_head_leading([], _CIndStr) ->
    [];
emit_head_leading([blank | Rest], CIndStr) ->
    emit_head_leading(Rest, CIndStr);
emit_head_leading([{comment, Tok} | Rest], CIndStr) ->
    Text = r3lfe_format_lexer:text(Tok),
    [CIndStr, Text, "\n" | emit_head_leading(Rest, CIndStr)].

%% emit_child_leading: leading trivia for a rest child at IndentStr.
%% IsFirst=true drops the first blank (no blank immediately after head line).
-spec emit_child_leading([r3lfe_format_cst:trivia()], string(), boolean()) -> iolist().
emit_child_leading(Leading, IndentStr, IsFirst) ->
    emit_leading_trivia(Leading, IndentStr, IsFirst).

%% emit_trailing: emit a trailing comment (if any) on the same line as the node.
-spec emit_trailing([r3lfe_format_cst:trivia()], non_neg_integer()) ->
          {iolist(), non_neg_integer()}.
emit_trailing([], Col) ->
    {[], Col};
emit_trailing([{comment, Tok}], Col) ->
    Text   = r3lfe_format_lexer:text(Tok),
    NewCol = col_after_text(Text, Col + 1),
    {[" ", Text], NewCol}.

%% emit_dangling: emit dangling trivia items, each on its own line at IndentStr.
%% The leading \n for each item is included (caller appends \nCIndStr+close after).
-spec emit_dangling([r3lfe_format_cst:trivia()], string()) -> iolist().
emit_dangling([], _IndStr) ->
    [];
emit_dangling([{comment, Tok} | Rest], IndStr) ->
    Text = r3lfe_format_lexer:text(Tok),
    ["\n", IndStr, Text | emit_dangling(Rest, IndStr)];
emit_dangling([blank | Rest], IndStr) ->
    ["\n" | emit_dangling(Rest, IndStr)].

%% emit_toplevel_dangling: trailing trivia after the last top-level form.
%% Blanks are dropped; comments are emitted at column 0.
-spec emit_toplevel_dangling([r3lfe_format_cst:trivia()]) -> iolist().
emit_toplevel_dangling([]) ->
    [];
emit_toplevel_dangling([blank | Rest]) ->
    emit_toplevel_dangling(Rest);
emit_toplevel_dangling([{comment, Tok} | Rest]) ->
    Text = r3lfe_format_lexer:text(Tok),
    [Text, "\n" | emit_toplevel_dangling(Rest)].

%%====================================================================
%% Internal: flat-eligibility helpers
%%====================================================================

%% has_internal_trivia: true if flat-rendering this node would silently drop trivia.
%% A node's own leading/trailing are emitted by the parent context (not by
%% flat_render), so they do not prevent flat mode. Only:
%%   • the node's own dangling (inside the container content)
%%   • any leading/trailing/dangling on any descendant
%% force breaking.
-spec has_internal_trivia(r3lfe_format_cst:cst_node()) -> boolean().
has_internal_trivia(Node) ->
    r3lfe_format_cst:dangling(Node) =/= []
    orelse lists:any(fun has_descendant_trivia/1, r3lfe_format_cst:children(Node)).

-spec has_descendant_trivia(r3lfe_format_cst:cst_node()) -> boolean().
has_descendant_trivia(Node) ->
    r3lfe_format_cst:leading(Node) =/= []
    orelse r3lfe_format_cst:trailing(Node) =/= []
    orelse r3lfe_format_cst:dangling(Node) =/= []
    orelse lists:any(fun has_descendant_trivia/1, r3lfe_format_cst:children(Node)).

%%====================================================================
%% Internal: column helpers
%%====================================================================

-spec col_after_text(string(), non_neg_integer()) -> non_neg_integer().
col_after_text([], Col)         -> Col;
col_after_text([$\n | Rest], _) -> col_after_text(Rest, 0);
col_after_text([_ | Rest], Col) -> col_after_text(Rest, Col + 1).
