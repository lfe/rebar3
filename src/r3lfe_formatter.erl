%%%% LFE source formatter — Arc A7·S2b-1: regime classification + InData threading.
%%%% Pipeline: r3lfe_format_lexer -> r3lfe_format_cst -> iolist.
%%%%
%%%% Output is LF-only (\n). CRLF input is normalised to LF because A1 lexes
%%%% \r into whitespace tokens, which the CST drops. Empty files produce empty
%%%% output (no trailing \n; there are no forms to end). All other output ends
%%%% with exactly one \n. Comment order is preserved.
-module(r3lfe_formatter).

-export([format/1]).

%% regime/2 exported for unit testing only.
-ifdef(TEST).
-export([regime/2]).
-endif.

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
%% Internal: regime classification (A7·S2b)
%%====================================================================

-type regime() :: canonical | break_preserving.

%% regime/2: decide per container node whether the formatter owns layout
%% (canonical) or preserves the author's break positions (break_preserving).
%%
%% Rules (in priority order):
%%   InData=true         → break_preserving  (inside a quote — data context)
%%   tuple / binary      → break_preserving  (data containers)
%%   map                 → canonical          (k/v pair alignment owned by formatter)
%%   list/eval with specform or defform head → canonical
%%   any other list/eval (plain call, unknown head, non-symbol head) → break_preserving
%%
%% Note: leaves and prefixed nodes do not take a regime; only containers do.
-spec regime(r3lfe_format_cst:cst_node(), boolean()) -> regime().
regime(_Node, true) ->
    break_preserving;
regime(Node, false) ->
    case r3lfe_format_cst:type(Node) of
        tuple  -> break_preserving;
        binary -> break_preserving;
        map    -> canonical;
        T when T =:= list; T =:= eval ->
            case r3lfe_format_cst:dot_token(Node) of
                undefined ->
                    case r3lfe_format_cst:children(Node) of
                        [Head | _] ->
                            case classify_head(Head) of
                                {specform, _} -> canonical;
                                defform       -> canonical;
                                _             -> break_preserving
                            end;
                        [] -> break_preserving
                    end;
                _ ->
                    break_preserving  %% dotted lists are never canonical specforms
            end;
        _ ->
            break_preserving
    end.

%%====================================================================
%% Internal: document-level layout
%%====================================================================

-spec render_document(r3lfe_format_cst:cst_document()) -> iolist().
render_document(Doc) ->
    Nodes     = r3lfe_format_cst:document_children(Doc),
    DangItems = r3lfe_format_cst:document_dangling(Doc),
    Parts     = render_toplevel(Nodes, true, [], false),
    DangIO    = emit_toplevel_dangling(DangItems),
    lists:reverse([DangIO | Parts]).

%% render_toplevel: emit each top-level node with its leading trivia and final \n.
-spec render_toplevel([r3lfe_format_cst:cst_node()], boolean(), iolist(),
                      boolean()) -> iolist().
render_toplevel([], _IsFirst, Acc, _InData) ->
    Acc;
render_toplevel([Node | Rest], IsFirst, Acc, InData) ->
    LeadIO = emit_leading_trivia(r3lfe_format_cst:leading(Node), "", IsFirst),
    {NodeIO, NodeCol} = print_node(Node, 0, InData),
    {TrailIO, _Col}   = emit_trailing(r3lfe_format_cst:trailing(Node), NodeCol),
    Part = [LeadIO, NodeIO, TrailIO, "\n"],
    render_toplevel(Rest, false, [Part | Acc], InData).

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
%% InData: true when inside a quote/quasiquote context (data, not code).
-spec print_node(r3lfe_format_cst:cst_node(), non_neg_integer(), boolean()) ->
          {iolist(), non_neg_integer()}.
print_node(Node, Col, InData) ->
    W = flat_width(Node),
    Fits = W =/= infinity
           andalso Col + W =< ?WIDTH
           andalso not has_internal_trivia(Node),
    case Fits of
        true  -> {flat_render(Node), Col + W};
        false -> print_broken(Node, Col, InData)
    end.

%%====================================================================
%% Internal: broken printing
%%====================================================================

%% print_broken: broken form for containers, prefixed, and multi-line leaves.
%% Transitions InData at quote/quasiquote (→ true) and unquote/-splicing (→ false).
-spec print_broken(r3lfe_format_cst:cst_node(), non_neg_integer(), boolean()) ->
          {iolist(), non_neg_integer()}.
print_broken(Node, Col, InData) ->
    case r3lfe_format_cst:type(Node) of
        T when T =:= list; T =:= tuple; T =:= map; T =:= binary; T =:= eval ->
            print_broken_container(Node, Col, InData);
        prefixed ->
            PfxText = r3lfe_format_lexer:text(r3lfe_format_cst:prefix(Node)),
            PfxKind = r3lfe_format_lexer:kind(r3lfe_format_cst:prefix(Node)),
            [Inner]  = r3lfe_format_cst:children(Node),
            InnerInData = case PfxKind of
                quote            -> true;
                quasiquote       -> true;
                unquote          -> false;
                unquote_splicing -> false;
                _                -> InData
            end,
            {InnerIO, InnerCol} = print_node(Inner, Col + length(PfxText), InnerInData),
            {[PfxText, InnerIO], InnerCol};
        _ ->
            %% Leaf with multi-line token (tqstring/tqbstring): emit verbatim.
            Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Node)),
            {Text, col_after_text(Text, Col)}
    end.

%% print_broken_container: branches on regime/2 (A7·S2b-2).
%%   canonical        → head-classified indentation (A4) + map pair alignment (S3a)
%%   break_preserving → author break positions preserved (A7·S2b)
%%
%% Dangling trivia always at C+2; close on its own line when dangling present
%% or last child has trailing comment. All A3 trivia rules unchanged.
-spec print_broken_container(r3lfe_format_cst:cst_node(), non_neg_integer(),
                             boolean()) ->
          {iolist(), non_neg_integer()}.
print_broken_container(Node, C, InData) ->
    Open      = r3lfe_format_lexer:text(r3lfe_format_cst:open(Node)),
    Close     = r3lfe_format_lexer:text(r3lfe_format_cst:close(Node)),
    Children  = r3lfe_format_cst:children(Node),
    Dangling  = r3lfe_format_cst:dangling(Node),
    Indent    = C + 2,
    IndentStr = lists:duplicate(Indent, $\s),
    CIndStr   = lists:duplicate(C, $\s),
    CloseLen  = length(Close),
    OpenLen   = length(Open),
    case Children of
        [] ->
            case Dangling of
                [] ->
                    {[Open, Close], C + OpenLen + CloseLen};
                _ ->
                    DangIO = emit_dangling(Dangling, IndentStr),
                    {[Open, DangIO, "\n", CIndStr, Close], C + CloseLen}
            end;
        [Head | RestChildren] ->
            case regime(Node, InData) of
                canonical ->
                    case head_has_leading_comment(Head) of
                        true ->
                            %% Opener alone; all children at Indent (fix1 idempotency).
                            {AllIO, LastCol, HasTrail} =
                                print_rest_loop([Head | RestChildren],
                                                Indent, IndentStr, true, InData),
                            {CloseIO, CloseCol} =
                                close_section(Dangling, HasTrail, LastCol,
                                              Indent, IndentStr, C, CIndStr, Close),
                            {[Open, AllIO, CloseIO], CloseCol};
                        false ->
                            case r3lfe_format_cst:type(Node) of
                                T when T =:= list; T =:= eval ->
                                    Class = classify_head(Head),
                                    print_classified(Class, Head, RestChildren, Dangling,
                                                     C, Open, OpenLen, Close, CloseLen,
                                                     Indent, IndentStr, CIndStr, InData);
                                map ->
                                    print_map_pairs(Head, RestChildren, Dangling,
                                                    C, Open, OpenLen, Close, CloseLen,
                                                    Indent, IndentStr, CIndStr, InData)
                            end
                    end;
                break_preserving ->
                    print_bp_container(Node, C, Open, OpenLen, Close, CloseLen,
                                       Head, RestChildren, Dangling,
                                       Indent, IndentStr, CIndStr, InData)
            end
    end.

%%====================================================================
%% Internal: break-preserving renderer (A7·S2b-2)
%%====================================================================

%% print_bp_container: render a break-preserving container.
%%
%% Flat path is already handled by print_node (tried before print_broken is called).
%% Here we are in the broken path.
%%
%% If Head has a leading comment: opener alone, all children (incl. Head) via
%% print_rest_loop at Indent — comment safety (same as canonical fix1 path).
%%
%% Otherwise:
%%   - If nl_before(Head)=true: head on new line at C+2, AlignCol=C+2.
%%   - Else: head on opener line at C+OpenLen.
%%     AlignCol = column of first argument:
%%       - If first arg has nl_before or head has trailing comment → C+2 (hanging).
%%       - Else → HTC+1 (align under first arg).
%%
%% Subsequent children via bp_rest_loop: new line if nl_before OR overflow OR
%% has leading comment; otherwise space-separated on current line.
%% Close hugs last child unless dangling or last-child trailing comment.
-spec print_bp_container(r3lfe_format_cst:cst_node(),
                         non_neg_integer(), string(), non_neg_integer(),
                         string(), non_neg_integer(),
                         r3lfe_format_cst:cst_node(), [r3lfe_format_cst:cst_node()],
                         [r3lfe_format_cst:trivia()],
                         non_neg_integer(), string(), string(), boolean()) ->
          {iolist(), non_neg_integer()}.
print_bp_container(Node, C, Open, _OpenLen, Close, _CloseLen,
                   Head, RestChildren, Dangling,
                   Indent, IndentStr, CIndStr, InData) ->
    DotTok = r3lfe_format_cst:dot_token(Node),
    {RestBody, MaybeTail} = split_dot_tail(DotTok, RestChildren),
    IsCondHead = (r3lfe_format_cst:type(Head) =:= symbol)
        andalso (r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)) =:= "cond"),
    case head_has_leading_comment(Head) of
        true ->
            case InData of
                false ->
                    %% Code list: opener-alone, all children one-per-line at Indent (unchanged).
                    {AllIO, LastCol, HasTrail} =
                        print_rest_loop([Head | RestBody], Indent, IndentStr, true, InData),
                    {DotIO, DotCol, DotHasTrail} =
                        apply_dot_suffix(MaybeTail, LastCol, HasTrail),
                    {CloseIO, CloseCol} =
                        close_section(Dangling, DotHasTrail, DotCol,
                                      Indent, IndentStr, C, CIndStr, Close),
                    {[Open, AllIO, DotIO, CloseIO], CloseCol};
                true ->
                    %% Data list (§3.9): first head comment on opener line; rest +
                    %% elements at AlignCol = C+len(Open).
                    AlignCol  = C + length(Open),
                    AlignStr  = lists:duplicate(AlignCol, $\s),
                    HeadLeading = r3lfe_format_cst:leading(Head),
                    Comments  = [r3lfe_format_lexer:text(Tok)
                                 || {comment, Tok} <- HeadLeading],
                    HeadLeadIO =
                        case Comments of
                            [] ->
                                [];
                            [First | More] ->
                                MoreIO = [[AlignStr, T, "\n"] || T <- More],
                                [First, "\n" | MoreIO]
                        end,
                    {HeadIO, HeadCol}  = print_node(Head, AlignCol, InData),
                    {HeadTrailIO, HTC} = emit_trailing(
                                           r3lfe_format_cst:trailing(Head), HeadCol),
                    {RestIO, BodyLastCol, BodyHasTrail} =
                        bp_rest_loop(RestBody, AlignCol, AlignStr, HTC, InData),
                    {DotIO, DotCol, DotHasTrail} =
                        apply_dot_suffix(MaybeTail, BodyLastCol, BodyHasTrail),
                    {CloseIO, CloseCol} =
                        close_section(Dangling, DotHasTrail, DotCol,
                                      AlignCol, AlignStr, C, CIndStr, Close),
                    {[Open, HeadLeadIO, AlignStr, HeadIO, HeadTrailIO,
                      RestIO, DotIO, CloseIO], CloseCol}
            end;
        false ->
            HeadLeadIO = emit_head_leading(r3lfe_format_cst:leading(Head), CIndStr),
            HeadCol    = C + length(Open),
            case r3lfe_format_cst:nl_before(Head) of
                true ->
                    %% Head on new line at C+2; all args also at C+2.
                    HangStr = IndentStr,
                    {HeadIO, HCol}       = print_node(Head, Indent, InData),
                    {HeadTrailIO, HTC}   = emit_trailing(
                                             r3lfe_format_cst:trailing(Head), HCol),
                    {RestIO, BodyLastCol, BodyHasTrail} =
                        case IsCondHead of
                            true  -> bp_clause_rest_loop(RestBody, Indent, HangStr, HTC, InData);
                            false -> bp_rest_loop(RestBody, Indent, HangStr, HTC, InData)
                        end,
                    {DotIO, DotCol, DotHasTrail} =
                        apply_dot_suffix(MaybeTail, BodyLastCol, BodyHasTrail),
                    {CloseIO, CloseCol}  =
                        close_section(Dangling, DotHasTrail, DotCol,
                                      Indent, HangStr, C, CIndStr, Close),
                    {[HeadLeadIO, Open, "\n", HangStr, HeadIO, HeadTrailIO,
                      RestIO, DotIO, CloseIO], CloseCol};
                false ->
                    {HeadIO, HCol}       = print_node(Head, HeadCol, InData),
                    {HeadTrailIO, HTC}   = emit_trailing(
                                             r3lfe_format_cst:trailing(Head), HCol),
                    HeadHasTrail = r3lfe_format_cst:trailing(Head) =/= [],
                    case RestBody of
                        [] ->
                            {DotIO, DotCol, DotHasTrail} =
                                apply_dot_suffix(MaybeTail, HTC, HeadHasTrail),
                            {CloseIO, CloseCol} =
                                close_section(Dangling, DotHasTrail, DotCol,
                                              Indent, IndentStr, C, CIndStr, Close),
                            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, DotIO, CloseIO], CloseCol};
                        [FirstArg | OtherArgs] ->
                            %% AlignCol = column where first arg lands.
                            %% Hanging (C+2) when: head has trailing comment, first arg
                            %% has nl_before, OR first arg would overflow the current line.
                            %% "Overflow" here uses >= so a token at exactly col 80
                            %% triggers wrapping (col 80 = 81st char on the line, over limit).
                            FirstArgNL = r3lfe_format_cst:nl_before(FirstArg),
                            FirstArgW  = flat_width(FirstArg),
                            FirstArgOverflows =
                                FirstArgW =:= infinity
                                orelse HTC + 1 + FirstArgW >= ?WIDTH,
                            {AlignCol, AlignStr} =
                                case HeadHasTrail orelse FirstArgNL orelse FirstArgOverflows of
                                    true  -> {Indent, IndentStr};
                                    false -> {HTC + 1, lists:duplicate(HTC + 1, $\s)}
                                end,
                            IsMultiline = r3lfe_format_cst:multiline(Node),
                            {RestIO, BodyLastCol, BodyHasTrail} =
                                case {IsCondHead, IsMultiline orelse OtherArgs =:= []} of
                                    {true, true} ->
                                        bp_clause_rest_loop(RestBody, AlignCol, AlignStr, HTC, InData);
                                    {true, false} ->
                                        {FirstIO, _, _} =
                                            bp_clause_rest_loop([FirstArg], AlignCol, AlignStr, HTC, InData),
                                        {OtherIO, OtherLastCol, OtherHasTrail} =
                                            print_clause_loop(OtherArgs, AlignCol, AlignStr, false, InData),
                                        {[FirstIO, OtherIO], OtherLastCol, OtherHasTrail};
                                    {false, true} ->
                                        bp_rest_loop(RestBody, AlignCol, AlignStr, HTC, InData);
                                    {false, false} ->
                                        {FirstIO, _, _} =
                                            bp_rest_loop([FirstArg], AlignCol, AlignStr, HTC, InData),
                                        {OtherIO, OtherLastCol, OtherHasTrail} =
                                            print_rest_loop(OtherArgs, AlignCol, AlignStr, false, InData),
                                        {[FirstIO, OtherIO], OtherLastCol, OtherHasTrail}
                                end,
                            {DotIO, DotCol, DotHasTrail} =
                                apply_dot_suffix(MaybeTail, BodyLastCol, BodyHasTrail),
                            {CloseIO, CloseCol} =
                                close_section(Dangling, DotHasTrail, DotCol,
                                              AlignCol, AlignStr, C, CIndStr, Close),
                            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, RestIO, DotIO, CloseIO],
                             CloseCol}
                    end
            end
    end.

%% bp_rest_loop: render children preserving nl_before break positions.
%% Each child starts a new line iff:
%%   • nl_before(Child) is true (author broke here), OR
%%   • it has a leading comment (comment safety), OR
%%   • it would overflow column 80 on the current line.
%% Otherwise it is appended space-separated on the current line.
-spec bp_rest_loop([r3lfe_format_cst:cst_node()], non_neg_integer(), string(),
                   non_neg_integer(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
bp_rest_loop([], _AlignCol, _AlignStr, CurCol, _InData) ->
    {[], CurCol, false};
bp_rest_loop([Child | Rest], AlignCol, AlignStr, CurCol, InData) ->
    W         = flat_width(Child),
    NlBefore  = r3lfe_format_cst:nl_before(Child),
    HasLead   = has_comment_leading(r3lfe_format_cst:leading(Child)),
    Overflow  = W =:= infinity orelse CurCol + 1 + W >= ?WIDTH,
    NewLine   = NlBefore orelse HasLead orelse Overflow,
    {StartCol, Prefix} =
        case NewLine of
            true  -> {AlignCol, ["\n",
                                 emit_child_leading(
                                   r3lfe_format_cst:leading(Child), AlignStr, false),
                                 AlignStr]};
            false -> {CurCol + 1, " "}
        end,
    {ChildIO, ChildCol} = print_node(Child, StartCol, InData),
    {TrailIO, TrailCol} = emit_trailing(r3lfe_format_cst:trailing(Child), ChildCol),
    case Rest of
        [] ->
            HasTrail = r3lfe_format_cst:trailing(Child) =/= [],
            {[Prefix, ChildIO, TrailIO], TrailCol, HasTrail};
        _ ->
            {RestIO, LastCol, HasTrail} =
                bp_rest_loop(Rest, AlignCol, AlignStr, TrailCol, InData),
            {[Prefix, ChildIO, TrailIO | RestIO], LastCol, HasTrail}
    end.

%% bp_clause_rest_loop: like bp_rest_loop but uses render_clause for each child.
%% Used for cond clauses (preserves nl_before positioning while applying the
%% trivial/non-trivial clause rule to each clause's internal rendering).
-spec bp_clause_rest_loop([r3lfe_format_cst:cst_node()], non_neg_integer(), string(),
                          non_neg_integer(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
bp_clause_rest_loop([], _AlignCol, _AlignStr, CurCol, _InData) ->
    {[], CurCol, false};
bp_clause_rest_loop([Clause | Rest], AlignCol, AlignStr, CurCol, InData) ->
    W        = flat_width(Clause),
    NlBefore = r3lfe_format_cst:nl_before(Clause),
    HasLead  = has_comment_leading(r3lfe_format_cst:leading(Clause)),
    Overflow = W =:= infinity orelse CurCol + 1 + W >= ?WIDTH,
    NewLine  = NlBefore orelse HasLead orelse Overflow,
    {StartCol, Prefix} =
        case NewLine of
            true  -> {AlignCol, ["\n",
                                 emit_child_leading(
                                   r3lfe_format_cst:leading(Clause), AlignStr, false),
                                 AlignStr]};
            false -> {CurCol + 1, " "}
        end,
    {ClauseIO, ClauseCol} = render_clause(Clause, StartCol, InData),
    {TrailIO, TrailCol}   = emit_trailing(r3lfe_format_cst:trailing(Clause), ClauseCol),
    case Rest of
        [] ->
            HasTrail = r3lfe_format_cst:trailing(Clause) =/= [],
            {[Prefix, ClauseIO, TrailIO], TrailCol, HasTrail};
        _ ->
            {RestIO, LastCol, HasTrail} =
                bp_clause_rest_loop(Rest, AlignCol, AlignStr, TrailCol, InData),
            {[Prefix, ClauseIO, TrailIO | RestIO], LastCol, HasTrail}
    end.

%%====================================================================
%% Internal: cons-dot helpers (A7·S1)
%%====================================================================

%% split_dot_tail/2: for a dotted list, separate body children from the tail.
split_dot_tail(undefined, Children)  -> {Children, none};
split_dot_tail(_, [])                -> {[], none};
split_dot_tail(DotTok, Children) ->
    {lists:droplast(Children), {DotTok, lists:last(Children)}}.

%% apply_dot_suffix/3: append " . tail" IO after the body, returning updated col.
apply_dot_suffix(none, Col, HasTrail) ->
    {[], Col, HasTrail};
apply_dot_suffix({DotTok, TailNode}, Col, _HasTrail) ->
    DotText = r3lfe_format_lexer:text(DotTok),
    TailIO  = flat_render(TailNode),
    TailW   = case flat_width(TailNode) of infinity -> 0; W -> W end,
    TailCol = Col + 3 + TailW,
    TailHasTrail = r3lfe_format_cst:trailing(TailNode) =/= [],
    {[" ", DotText, " ", TailIO], TailCol, TailHasTrail}.

%%====================================================================
%% Internal: map key-value pair rendering (A4·S3a)
%%====================================================================

%% print_map_pairs: render map children as key-value pairs (style guide §6).
%%   First pair on the opener line: #m(k1 v1
%%   Subsequent pairs aligned at C+OpenLen:
%%      k2 v2
%%      k3 v3)
%% If any direct map child carries a leading or trailing comment, fall back
%% to element-per-line (identical to list_head rendering) so no comment
%% swallows a paired value.
-spec print_map_pairs(r3lfe_format_cst:cst_node(), [r3lfe_format_cst:cst_node()],
                      [r3lfe_format_cst:trivia()],
                      non_neg_integer(), string(), non_neg_integer(),
                      string(), non_neg_integer(),
                      non_neg_integer(), string(), string(), boolean()) ->
          {iolist(), non_neg_integer()}.
print_map_pairs(Head, RestChildren, Dangling,
                C, Open, OpenLen, Close, CloseLen,
                Indent, IndentStr, CIndStr, InData) ->
    AllChildren = [Head | RestChildren],
    AnyTrivia = lists:any(
        fun(Child) ->
            r3lfe_format_cst:leading(Child) =/= []
            orelse r3lfe_format_cst:trailing(Child) =/= []
        end, AllChildren),
    case AnyTrivia of
        true ->
            %% Fall back: element-per-line (reuse list_head; safe with trivia).
            print_classified(list_head, Head, RestChildren, Dangling,
                             C, Open, OpenLen, Close, CloseLen,
                             Indent, IndentStr, CIndStr, InData);
        false ->
            AlignCol = C + OpenLen,
            AlignStr = lists:duplicate(AlignCol, $\s),
            {PairsIO, LastCol, HasTrail} =
                print_map_pairs_list(AllChildren, AlignCol, AlignStr, InData),
            {CloseIO, CloseCol} = close_section(Dangling, HasTrail, LastCol,
                                                Indent, IndentStr, C, CIndStr, Close),
            {[Open, PairsIO, CloseIO], CloseCol}
    end.

%% print_map_pairs_list: render the full list of map children starting at
%% AlignCol (first pair on the opener line, no leading newline).
-spec print_map_pairs_list([r3lfe_format_cst:cst_node()],
                            non_neg_integer(), string(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
print_map_pairs_list([K, V], AlignCol, _AlignStr, InData) ->
    {KIO, KCol} = print_node(K, AlignCol, InData),
    {VIO, VCol} = print_node(V, KCol + 1, InData),
    VTrail = r3lfe_format_cst:trailing(V) =/= [],
    {[KIO, " ", VIO], VCol, VTrail};
print_map_pairs_list([K, V | Rest], AlignCol, AlignStr, InData) ->
    {KIO, KCol} = print_node(K, AlignCol, InData),
    {VIO, _VCol} = print_node(V, KCol + 1, InData),
    {RestIO, LastCol, HasTrail} = print_map_pairs_rest(Rest, AlignCol, AlignStr, InData),
    {[KIO, " ", VIO | RestIO], LastCol, HasTrail};
print_map_pairs_list([K], AlignCol, _AlignStr, InData) ->
    %% Odd last element (malformed map): emit alone.
    {KIO, KCol} = print_node(K, AlignCol, InData),
    KTrail = r3lfe_format_cst:trailing(K) =/= [],
    {[KIO], KCol, KTrail}.

%% print_map_pairs_rest: emit remaining k-v pairs each preceded by \n+AlignStr.
-spec print_map_pairs_rest([r3lfe_format_cst:cst_node()],
                            non_neg_integer(), string(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
print_map_pairs_rest([K, V], AlignCol, AlignStr, InData) ->
    {KIO, KCol} = print_node(K, AlignCol, InData),
    {VIO, VCol} = print_node(V, KCol + 1, InData),
    VTrail = r3lfe_format_cst:trailing(V) =/= [],
    {["\n", AlignStr, KIO, " ", VIO], VCol, VTrail};
print_map_pairs_rest([K, V | Rest], AlignCol, AlignStr, InData) ->
    {KIO, KCol} = print_node(K, AlignCol, InData),
    {VIO, _VCol} = print_node(V, KCol + 1, InData),
    {RestIO, LastCol, HasTrail} = print_map_pairs_rest(Rest, AlignCol, AlignStr, InData),
    {["\n", AlignStr, KIO, " ", VIO | RestIO], LastCol, HasTrail};
print_map_pairs_rest([K], AlignCol, AlignStr, InData) ->
    %% Odd last element.
    {KIO, KCol} = print_node(K, AlignCol, InData),
    KTrail = r3lfe_format_cst:trailing(K) =/= [],
    {["\n", AlignStr, KIO], KCol, KTrail}.

%% is_when_form: true if Node is a list whose first child is the symbol "when".
-spec is_when_form(r3lfe_format_cst:cst_node()) -> boolean().
is_when_form(Node) ->
    r3lfe_format_cst:type(Node) =:= list
    andalso case r3lfe_format_cst:children(Node) of
                [WHead | _] ->
                    r3lfe_format_cst:type(WHead) =:= symbol
                    andalso r3lfe_format_lexer:text(r3lfe_format_cst:open(WHead)) =:= "when";
                [] -> false
            end.

%% head_has_leading_comment: true iff the node's leading contains a comment.
-spec head_has_leading_comment(r3lfe_format_cst:cst_node()) -> boolean().
head_has_leading_comment(Node) ->
    lists:any(fun({comment, _}) -> true; (_) -> false end,
              r3lfe_format_cst:leading(Node)).

%% any_dist_has_comment: true if the distinguished args have an unsafe comment.
%% Safe: trailing comment on the LAST distinguished arg (ends head line; body
%% goes below at +2).  Unsafe: leading comment on ANY arg, or trailing comment
%% on a NON-LAST arg (would swallow the next distinguished arg on the same line).
-spec any_dist_has_comment([r3lfe_format_cst:cst_node()]) -> boolean().
any_dist_has_comment([]) -> false;
any_dist_has_comment([D]) ->
    %% Last item: trailing comment is safe; only leading triggers fallback.
    head_has_leading_comment(D);
any_dist_has_comment([D | Rest]) ->
    head_has_leading_comment(D)
    orelse r3lfe_format_cst:trailing(D) =/= []
    orelse any_dist_has_comment(Rest).

%% must_break: true when flat rendering must be suppressed regardless of width.
%%   • defform-headed lists (defun/defmacro with args, defmodule, defrecord, …)
%%   • maps: key-value pairs always on separate lines
%%   • list headed by let/let*/case/cond
%% Scope note: flet/fletrec/letrec-function and other let-family forms are NOT
%% forced — they retain flat-if-fits.  Extend this list when adjudicated.
-spec must_break(r3lfe_format_cst:cst_node()) -> boolean().
must_break(Node) ->
    case r3lfe_format_cst:type(Node) of
        map  -> true;
        list ->
            r3lfe_format_cst:dot_token(Node) =:= undefined
            andalso (is_force_break_defform(Node)
                     orelse is_always_break_head(Node)
                     orelse is_lambda_multi_body(Node));
        _    -> false
    end.

%% is_always_break_head: true for list nodes headed by a form that must always
%% break (let/let*/case/cond/if/progn/receive/try/maybe/match-lambda).
-spec is_always_break_head(r3lfe_format_cst:cst_node()) -> boolean().
is_always_break_head(Node) ->
    case r3lfe_format_cst:children(Node) of
        [Head | _] ->
            case r3lfe_format_cst:type(Head) of
                symbol ->
                    Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
                    Text =:= "let"     orelse Text =:= "let*"
                    orelse Text =:= "case"    orelse Text =:= "cond"
                    orelse Text =:= "if"      orelse Text =:= "progn"
                    orelse Text =:= "receive" orelse Text =:= "try"
                    orelse Text =:= "maybe"   orelse Text =:= "match-lambda";
                _ -> false
            end;
        [] -> false
    end.

%% is_let_head: true when the head symbol is let or let*.
-spec is_let_head(r3lfe_format_cst:cst_node()) -> boolean().
is_let_head(Head) ->
    case r3lfe_format_cst:type(Head) of
        symbol ->
            Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
            Text =:= "let" orelse Text =:= "let*";
        _ -> false
    end.

%% is_flet_head: true when the head symbol is flet, flet*, or fletrec.
-spec is_flet_head(r3lfe_format_cst:cst_node()) -> boolean().
is_flet_head(Head) ->
    case r3lfe_format_cst:type(Head) of
        symbol ->
            Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
            Text =:= "flet" orelse Text =:= "flet*" orelse Text =:= "fletrec";
        _ -> false
    end.

%% local_fn_n: N for rendering a single flet/fletrec binding as a defun-like form.
%% Binding children = [name | rest].  N=1 when rest has an arglist as its first
%% element (signature form: name + arglist on head line); N=0 otherwise (match-
%% clause form: name on head line, clauses at +2).
-spec local_fn_n(r3lfe_format_cst:cst_node()) -> non_neg_integer().
local_fn_n(Binding) ->
    case r3lfe_format_cst:children(Binding) of
        [_Name, Arg2 | _] ->
            case is_arglist(Arg2) of
                true  -> 1;
                false -> 0
            end;
        _ -> 0
    end.

%% is_lambda_multi_body: true for (lambda arglist body1 body2 …) with >1 body form.
%% Children = [lambda-sym, arglist | body…]; body count > 1 forces a break so the
%% implicit progn is always written one-form-per-line (formatting-rules §3.2).
-spec is_lambda_multi_body(r3lfe_format_cst:cst_node()) -> boolean().
is_lambda_multi_body(Node) ->
    case r3lfe_format_cst:children(Node) of
        [Head, _Arglist | Body] ->
            length(Body) > 1
            andalso r3lfe_format_cst:type(Head) =:= symbol
            andalso r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)) =:= "lambda";
        _ -> false
    end.

%% is_clause_specform_head: true for specforms whose body children are clauses.
%% try case/catch sections are intentionally deferred to A7·S4.
-spec is_clause_specform_head(r3lfe_format_cst:cst_node(), non_neg_integer()) -> boolean().
is_clause_specform_head(Head, N) ->
    case r3lfe_format_cst:type(Head) of
        symbol ->
            Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
            Text =:= "case" orelse (Text =:= "match-lambda" andalso N =:= 0);
        _ ->
            false
    end.

%% is_defun_match_head: true for defun/defmacro routed through dynamic N=1.
-spec is_defun_match_head(r3lfe_format_cst:cst_node(), non_neg_integer()) -> boolean().
is_defun_match_head(Head, 1) ->
    case r3lfe_format_cst:type(Head) of
        symbol ->
            Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
            Text =:= "defun" orelse Text =:= "defmacro";
        _ ->
            false
    end;
is_defun_match_head(_Head, _N) ->
    false.

-spec is_receive_head(r3lfe_format_cst:cst_node()) -> boolean().
is_receive_head(Head) ->
    case r3lfe_format_cst:type(Head) of
        symbol ->
            r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)) =:= "receive";
        _ ->
            false
    end.

-spec is_try_head(r3lfe_format_cst:cst_node()) -> boolean().
is_try_head(Head) ->
    case r3lfe_format_cst:type(Head) of
        symbol ->
            r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)) =:= "try";
        _ ->
            false
    end.

-spec is_after_section(r3lfe_format_cst:cst_node()) -> boolean().
is_after_section(Node) ->
    r3lfe_format_cst:type(Node) =:= list
    andalso case r3lfe_format_cst:children(Node) of
        [Head | _] ->
            r3lfe_format_cst:type(Head) =:= symbol
            andalso r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)) =:= "after";
        [] ->
            false
    end.

-spec all_clauses([r3lfe_format_cst:cst_node()]) -> boolean().
all_clauses(Children) ->
    lists:all(
        fun(Child) ->
            r3lfe_format_cst:type(Child) =:= list
            andalso r3lfe_format_cst:open(Child) =/= undefined
            andalso r3lfe_format_cst:close(Child) =/= undefined
        end, Children).

%%====================================================================
%% Internal: clause helpers (A7·S3b-1)
%%====================================================================

%% trivial_clause: a clause is trivial iff it has exactly two children
%% (pattern + a trivial datum) and carries no internal trivia. Trivial
%% clauses render flat; non-trivial clauses always break (pattern line +
%% body below via the list_head path). The clause's own trailing trivia
%% is handled by the parent loop and does not affect triviality.
-spec trivial_clause(r3lfe_format_cst:cst_node()) -> boolean().
trivial_clause(Node) ->
    r3lfe_format_cst:type(Node) =:= list
    andalso not has_clause_internal_trivia(Node)
    andalso case r3lfe_format_cst:children(Node) of
        [_Pattern, Datum] -> is_trivial_datum(Datum);
        _                 -> false
    end.

%% has_clause_internal_trivia: true when the clause itself has a leading comment
%% or dangling trivia, or any descendant has any trivia.
%% The clause's own trailing is excluded (handled externally).
-spec has_clause_internal_trivia(r3lfe_format_cst:cst_node()) -> boolean().
has_clause_internal_trivia(Node) ->
    has_comment_leading(r3lfe_format_cst:leading(Node))
    orelse r3lfe_format_cst:dangling(Node) =/= []
    orelse lists:any(fun has_descendant_trivia/1, r3lfe_format_cst:children(Node)).

%% is_trivial_datum: true for a leaf node (symbol/number/string/char) or a
%% prefixed node whose inner is such a leaf.
-spec is_trivial_datum(r3lfe_format_cst:cst_node()) -> boolean().
is_trivial_datum(Node) ->
    case r3lfe_format_cst:type(Node) of
        T when T =:= symbol; T =:= number; T =:= string; T =:= char -> true;
        prefixed ->
            case r3lfe_format_cst:children(Node) of
                [Inner] ->
                    case r3lfe_format_cst:type(Inner) of
                        T when T =:= symbol; T =:= number;
                               T =:= string; T =:= char -> true;
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.

%% render_clause: flat if trivial; list_head layout otherwise.
%% Directly dispatches to print_classified(list_head, …) to guarantee the
%% break regardless of what regime/2 would return for the clause's head.
-spec render_clause(r3lfe_format_cst:cst_node(), non_neg_integer(), boolean()) ->
          {iolist(), non_neg_integer()}.
render_clause(Clause, Col, InData) ->
    case trivial_clause(Clause) of
        true  -> {flat_render(Clause), Col + flat_width(Clause)};
        false ->
            case r3lfe_format_cst:children(Clause) of
                [] ->
                    print_broken(Clause, Col, InData);
                [Head | Rest] ->
                    Open     = r3lfe_format_lexer:text(r3lfe_format_cst:open(Clause)),
                    Close    = r3lfe_format_lexer:text(r3lfe_format_cst:close(Clause)),
                    OpenLen  = length(Open),
                    CloseLen = length(Close),
                    Dangling = r3lfe_format_cst:dangling(Clause),
                    Indent    = Col + 2,
                    IndentStr = lists:duplicate(Indent, $\s),
                    CIndStr   = lists:duplicate(Col, $\s),
                    print_classified(list_head, Head, Rest, Dangling,
                                     Col, Open, OpenLen, Close, CloseLen,
                                     Indent, IndentStr, CIndStr, InData)
            end
    end.

%%====================================================================
%% Internal: defform helpers (A4·S2)
%%====================================================================

%% is_arglist: true for () and (x y z) but NOT for ((pat) body) match clauses.
%% A list whose first child is itself a list is a match clause, not an arglist.
-spec is_arglist(r3lfe_format_cst:cst_node()) -> boolean().
is_arglist(Node) ->
    r3lfe_format_cst:type(Node) =:= list
    andalso case r3lfe_format_cst:children(Node) of
                []          -> true;
                [First | _] -> r3lfe_format_cst:type(First) =/= list
            end.

%% is_force_break_defform: true for defform-headed lists that must always break.
%% Only defun/defmacro with an empty arglist (the constant idiom) are excluded
%% and allowed to be flat-if-fits.
-spec is_force_break_defform(r3lfe_format_cst:cst_node()) -> boolean().
is_force_break_defform(Node) ->
    case r3lfe_format_cst:children(Node) of
        [Head | RestChildren] ->
            case classify_head(Head) of
                defform ->
                    HeadText = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
                    IsDefunMacro = HeadText =:= "defun" orelse HeadText =:= "defmacro",
                    case IsDefunMacro of
                        true  -> not has_empty_arglist(RestChildren);
                        false -> true   %% defmodule, defrecord, etc. always break
                    end;
                _ -> false
            end;
        _ -> false
    end.

%% has_empty_arglist: true when RestChildren is [Name, Arg2 | _] and Arg2 is
%% an arglist with no children (the empty-arglist / constant idiom).
-spec has_empty_arglist([r3lfe_format_cst:cst_node()]) -> boolean().
has_empty_arglist([_Name, Arg2 | _]) ->
    is_arglist(Arg2) andalso r3lfe_format_cst:children(Arg2) =:= [];
has_empty_arglist(_) ->
    false.

%% defform_n: compute the number of distinguished args for a breaking defform.
%%   defun/defmacro + non-empty arglist as Arg2 → N=2 (signature form)
%%   defun/defmacro + match-clause Arg2 (or missing Arg2) → N=1
%%   any other defform → N=1 (name on head line, rest at C+2)
-spec defform_n(r3lfe_format_cst:cst_node(), [r3lfe_format_cst:cst_node()]) ->
          pos_integer().
defform_n(Head, RestChildren) ->
    HeadText = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
    case HeadText =:= "defun" orelse HeadText =:= "defmacro" of
        true ->
            case RestChildren of
                [_Name, Arg2 | _] ->
                    case is_arglist(Arg2) of
                        true  -> 2;   %% (defun name (args) body…)
                        false -> 1    %% (defun name ((pat) body)…) match clauses
                    end;
                _ -> 1
            end;
        false ->
            1   %% defmodule, defrecord, defstruct, …
    end.

%%====================================================================
%% Internal: head classification (A4)
%%====================================================================

-type head_class() :: {specform, non_neg_integer()} | defform | funcall | list_head.

%% classify_head: determines indentation class for a breaking list's head.
%% Algorithm (order matters — table wins over def-prefix):
%%   1. Head not a symbol   → list_head
%%   2. Head in specform table → {specform, N}
%%   3. Head starts with "def" and length > 3 → defform
%%   4. else → funcall
-spec classify_head(r3lfe_format_cst:cst_node()) -> head_class().
classify_head(Head) ->
    case r3lfe_format_cst:type(Head) of
        symbol ->
            Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
            case maps:find(Text, specform_table()) of
                {ok, N} -> {specform, N};
                error   ->
                    case length(Text) > 3 andalso lists:prefix("def", Text) of
                        true  -> defform;
                        false -> funcall
                    end
            end;
        _ ->
            list_head
    end.

%% specform_table: verbatim from lfe-indent.el; maps symbol text → N distinguished args.
%% Intentional extensions beyond lfe-indent.el (per Duncan's ruling):
%%   "export" => 0, "import" => 0 — keyword-alone style, items at C+2, flat-if-fits.
%%   Other module-clause forms (behaviour, doc, …) could be added similarly.
%% Dialyzer infers a narrower key type ([1..255,...]) than string(); suppress.
-dialyzer({no_underspecs, specform_table/0}).
-spec specform_table() -> #{string() => non_neg_integer()}.
specform_table() ->
    #{
        ":"                 => 2,
        "after"             => 1,
        "bc"                => 1,
        "binary-comp"       => 1,
        "call"              => 2,
        "case"              => 1,
        "catch"             => 0,
        "define-function"   => 1,
        "define-macro"      => 1,
        "define-module"     => 1,
        "export"            => 0,
        "extend-module"     => 0,
        "import"            => 0,
        "do"                => 2,
        "else"              => 0,
        "eval-when-compile" => 0,
        "flet"              => 1,
        "flet*"             => 1,
        "fletrec"           => 1,
        "if"                => 1,
        "lambda"            => 1,
        "let"               => 1,
        "let*"              => 1,
        "let-function"      => 1,
        "letrec-function"   => 1,
        "let-macro"         => 1,
        "lc"                => 1,
        "list-comp"         => 1,
        "macrolet"          => 1,
        "match-lambda"      => 0,
        "match-spec"        => 0,
        "maybe"             => 0,
        "prog1"             => 1,
        "prog2"             => 2,
        "progn"             => 0,
        "receive"           => 0,
        "try"               => 0,
        "when"              => 0,
        "syntaxlet"         => 1,
        "defflavor"         => 3,
        "begin"             => 0,
        "let-syntax"        => 1,
        "syntax-rules"      => 0,
        "macro"             => 0
    }.

%%====================================================================
%% Internal: classified broken rendering
%%====================================================================

-spec print_classified(head_class(),
                       r3lfe_format_cst:cst_node(), [r3lfe_format_cst:cst_node()],
                       [r3lfe_format_cst:trivia()],
                       non_neg_integer(), string(), non_neg_integer(),
                       string(), non_neg_integer(),
                       non_neg_integer(), string(), string(), boolean()) ->
          {iolist(), non_neg_integer()}.

%% list_head: all elements aligned under the first at C+len(Open).
%%
%% Guard path (S3d): if RestChildren=[Guard|Body] where Guard is (when …)
%% and neither Pat (Head) nor Guard carries a comment, keep Pat+Guard on one
%% line and emit Body one per line at AlignCol.  Falls back to element-per-line
%% when either has a comment (comment safety) or Head has a trailing comment.
%% Atom-pattern clauses (Pat is a symbol → funcall class) never reach here.
print_classified(list_head, Head, RestChildren, Dangling,
                 C, Open, OpenLen, Close, _CloseLen,
                 Indent, IndentStr, CIndStr, InData) ->
    AlignCol = C + OpenLen,
    AlignStr = lists:duplicate(AlignCol, $\s),
    HeadLeadIO = emit_head_leading(r3lfe_format_cst:leading(Head), CIndStr),
    {HeadIO, HeadCol}  = print_node(Head, AlignCol, InData),
    {HeadTrailIO, HTC} = emit_trailing(r3lfe_format_cst:trailing(Head), HeadCol),
    HeadHasTrail = r3lfe_format_cst:trailing(Head) =/= [],
    UseGuard = case RestChildren of
        [G | _] ->
            is_when_form(G)
            andalso not HeadHasTrail
            andalso r3lfe_format_cst:leading(G) =:= []
            andalso r3lfe_format_cst:trailing(G) =:= [];
        _ -> false
    end,
    case {UseGuard, RestChildren} of
        {true, [Guard | Body]} ->
            %% Pat already printed; Guard on same line, Body below at AlignCol.
            {GuardIO, GuardCol} = print_node(Guard, HTC + 1, InData),
            case Body of
                [] ->
                    {CloseIO, CloseCol} = close_section(Dangling, false, GuardCol,
                                                        Indent, IndentStr, C, CIndStr, Close),
                    {[HeadLeadIO, Open, HeadIO, HeadTrailIO, " ", GuardIO, CloseIO], CloseCol};
                _ ->
                    {BodyIO, LastCol, HasTrail} = print_rest_loop(Body, AlignCol,
                                                                   AlignStr, true, InData),
                    {CloseIO, CloseCol} = close_section(Dangling, HasTrail, LastCol,
                                                        Indent, IndentStr, C, CIndStr, Close),
                    {[HeadLeadIO, Open, HeadIO, HeadTrailIO, " ", GuardIO, BodyIO, CloseIO], CloseCol}
            end;
        {false, []} ->
            {CloseIO, CloseCol} = close_section(Dangling, HeadHasTrail, HTC,
                                                Indent, IndentStr, C, CIndStr, Close),
            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, CloseIO], CloseCol};
        {false, _} ->
            {RestIO, LastCol, HasTrail} = print_rest_loop(RestChildren, AlignCol,
                                                           AlignStr, true, InData),
            {CloseIO, CloseCol} = close_section(Dangling, HasTrail, LastCol,
                                                Indent, IndentStr, C, CIndStr, Close),
            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, RestIO, CloseIO], CloseCol}
    end;

%% specform N: distinguished args 1..N on head line; body at C+2.
%% N=0: head alone, all args at C+2.
%% defform (provisional = specform 1, refined in S2).
%% Falls back to body layout when:
%%   • N=0 (always), OR
%%   • HeadHasTrail (fix2: a trailing comment on the head ends the line — no
%%     content may follow it on the head line), OR
%%   • any distinguished arg has a leading/trailing comment (fix1-b).
%% Body=[] branch passes HeadHasTrail to close_section so (progn ; c) and
%% similar still break the close onto its own line (fix2).
print_classified({specform, N}, Head, RestChildren, Dangling,
                 C, Open, OpenLen, Close, _CloseLen,
                 Indent, IndentStr, CIndStr, InData) ->
    HeadLeadIO = emit_head_leading(r3lfe_format_cst:leading(Head), CIndStr),
    {HeadIO, HeadCol}  = print_node(Head, C + OpenLen, InData),
    {HeadTrailIO, HTC} = emit_trailing(r3lfe_format_cst:trailing(Head), HeadCol),
    HeadHasTrail = r3lfe_format_cst:trailing(Head) =/= [],
    {DistIO, DistEndCol, Body} =
        case N =:= 0 orelse HeadHasTrail of
            true ->
                {[], HTC, RestChildren};
            false ->
                NSplit = min(N, length(RestChildren)),
                {DistPotential, BodyPotential} = lists:split(NSplit, RestChildren),
                case any_dist_has_comment(DistPotential) of
                    true  -> {[], HTC, RestChildren};  %% fall back: all to body
                    false ->
                        %% For let/let*: force-break binding list (one per line).
                        %% For flet/flet*/fletrec: force-break + defun-like per element.
                        {DIO, DCol} =
                            case is_let_head(Head) andalso DistPotential =/= [] of
                                true ->
                                    [BindList] = DistPotential,
                                    {BIO, BCol} = print_broken(BindList, HTC + 1, InData),
                                    {BTrailIO, BTC} = emit_trailing(
                                        r3lfe_format_cst:trailing(BindList), BCol),
                                    {[" ", BIO, BTrailIO], BTC};
                                false ->
                                    case is_flet_head(Head) andalso DistPotential =/= [] of
                                        true ->
                                            [BindList] = DistPotential,
                                            {BIO, BCol} = print_flet_bindlist(
                                                BindList, HTC + 1, InData),
                                            {BTrailIO, BTC} = emit_trailing(
                                                r3lfe_format_cst:trailing(BindList), BCol),
                                            {[" ", BIO, BTrailIO], BTC};
                                        false ->
                                            print_distinguished(DistPotential, HTC, InData)
                                    end
                            end,
                        {DIO, DCol, BodyPotential}
                end
        end,
    case Body of
        [] ->
            %% No body args. HeadHasTrail forces the close onto its own line
            %% (covers (progn ; c), (case ; c), and any head-trailing-no-args form).
            {CloseIO, CloseCol} = close_section(Dangling, HeadHasTrail, DistEndCol,
                                                Indent, IndentStr, C, CIndStr, Close),
            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, DistIO, CloseIO], CloseCol};
        _ ->
            IsCaseHead = is_clause_specform_head(Head, N),
            IsReceiveHead = is_receive_head(Head),
            IsTryHead = is_try_head(Head),
            IsDefunMatchHead =
                is_defun_match_head(Head, N) andalso DistIO =/= [] andalso all_clauses(Body),
            {BodyIO, LastCol, HasTrail} =
                case {IsTryHead, IsReceiveHead, IsCaseHead orelse IsDefunMatchHead} of
                    {true, _, _}  -> print_try_body_loop(Body, Indent, IndentStr, true, InData);
                    {_, true, _}  -> print_receive_body_loop(Body, Indent, IndentStr, true, InData);
                    {_, _, true}  -> print_clause_loop(Body, Indent, IndentStr, true, InData);
                    {_, _, false} -> print_rest_loop(Body, Indent, IndentStr, true, InData)
                end,
            {CloseIO, CloseCol} = close_section(Dangling, HasTrail, LastCol,
                                                Indent, IndentStr, C, CIndStr, Close),
            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, DistIO, BodyIO, CloseIO], CloseCol}
    end;

%% defform — dynamic N, delegating to specform (inherits full comment matrix).
%%
%%   defun/defmacro:
%%     N=2 when RestChildren=[_Name, Arg2|_] and is_arglist(Arg2)
%%            → signature form: (defun name (args)  body…)
%%     N=1 otherwise
%%            → match-clause form: (defun name  clauses…)
%%   any other defform → N=1 (name on head line, rest at C+2).
%%
%% Docstrings need no special case: the first body form (a string) lands at C+2.
print_classified(defform, Head, RestChildren, Dangling,
                 C, Open, OpenLen, Close, CloseLen,
                 Indent, IndentStr, CIndStr, InData) ->
    N = defform_n(Head, RestChildren),
    %% Rule (A7·S4a): def-forms are never alone on a line.  When N=2 and the
    %% distinguished args have a comment that would trigger the N=0 fallback
    %% (keyword alone), fall back to N=1 instead so keyword + name share the
    %% head line even when the arglist cannot.
    EffN = case N > 1 of
        false -> N;
        true  ->
            NSplit = min(N, length(RestChildren)),
            {DistPotential, _} = lists:split(NSplit, RestChildren),
            case any_dist_has_comment(DistPotential) of
                true  -> 1;
                false -> N
            end
    end,
    print_classified({specform, EffN}, Head, RestChildren, Dangling,
                     C, Open, OpenLen, Close, CloseLen,
                     Indent, IndentStr, CIndStr, InData);

%% funcall: a1 on head line; a2..aN aligned under a1's column.
%% Align column = C + len(Open) + len(flat(head)) + 1.
%% Falls back to body layout when head has a trailing comment (fix2: nothing may
%% follow it on the head line) or when a1 has a leading comment (fix1).
print_classified(funcall, Head, RestChildren, Dangling,
                 C, Open, OpenLen, Close, _CloseLen,
                 Indent, IndentStr, CIndStr, InData) ->
    HeadLeadIO = emit_head_leading(r3lfe_format_cst:leading(Head), CIndStr),
    {HeadIO, HeadCol}  = print_node(Head, C + OpenLen, InData),
    {HeadTrailIO, HTC} = emit_trailing(r3lfe_format_cst:trailing(Head), HeadCol),
    %% Head is always a symbol for funcall; use its text length for alignment.
    HeadTextLen = length(r3lfe_format_lexer:text(r3lfe_format_cst:open(Head))),
    AlignCol = C + OpenLen + HeadTextLen + 1,
    AlignStr = lists:duplicate(AlignCol, $\s),
    HeadHasTrail = r3lfe_format_cst:trailing(Head) =/= [],
    case RestChildren of
        [] ->
            {CloseIO, CloseCol} = close_section(Dangling, HeadHasTrail, HTC,
                                                Indent, IndentStr, C, CIndStr, Close),
            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, CloseIO], CloseCol};
        [A1 | RestArgs] ->
            case HeadHasTrail orelse head_has_leading_comment(A1) of
                true ->
                    %% Head trailing or a1 leading comment: all rest as body at C+2.
                    {AllIO, LastCol, HasTrail} = print_rest_loop(RestChildren, Indent,
                                                                  IndentStr, true, InData),
                    {CloseIO, CloseCol} = close_section(Dangling, HasTrail, LastCol,
                                                        Indent, IndentStr, C, CIndStr, Close),
                    {[HeadLeadIO, Open, HeadIO, HeadTrailIO, AllIO, CloseIO], CloseCol};
                false ->
                    {A1IO, A1Col}     = print_node(A1, HTC + 1, InData),
                    {A1TrailIO, A1TC} = emit_trailing(r3lfe_format_cst:trailing(A1), A1Col),
                    A1HasTrail = r3lfe_format_cst:trailing(A1) =/= [],
                    case RestArgs of
                        [] ->
                            {CloseIO, CloseCol} = close_section(Dangling, A1HasTrail, A1TC,
                                                                 Indent, IndentStr, C, CIndStr,
                                                                 Close),
                            {[HeadLeadIO, Open, HeadIO, HeadTrailIO,
                              " ", A1IO, A1TrailIO, CloseIO], CloseCol};
                        _ ->
                            {RestIO, LastCol, HasTrail} = print_rest_loop(RestArgs, AlignCol,
                                                                           AlignStr, true, InData),
                            {CloseIO, CloseCol} = close_section(Dangling, HasTrail, LastCol,
                                                                 Indent, IndentStr, C, CIndStr,
                                                                 Close),
                            {[HeadLeadIO, Open, HeadIO, HeadTrailIO,
                              " ", A1IO, A1TrailIO, RestIO, CloseIO], CloseCol}
                    end
            end
    end.

%% print_distinguished: print distinguished args space-separated on the head line.
%% Each arg's leading is emitted via emit_head_leading (blanks dropped).
-spec print_distinguished([r3lfe_format_cst:cst_node()], non_neg_integer(),
                          boolean()) ->
          {iolist(), non_neg_integer()}.
print_distinguished([], Col, _InData) ->
    {[], Col};
print_distinguished([D | Rest], Col, InData) ->
    DLeadIO = emit_head_leading(r3lfe_format_cst:leading(D), ""),
    {DIO, DCol}      = print_node(D, Col + 1, InData),
    {DTrailIO, DTC}  = emit_trailing(r3lfe_format_cst:trailing(D), DCol),
    {RestIO, LastCol} = print_distinguished(Rest, DTC, InData),
    {[" ", DLeadIO, DIO, DTrailIO | RestIO], LastCol}.

%% close_section: emit dangling then close, or close hugging last child.
%% Breaks close onto its own line at Indent (content indent) when:
%%   • Dangling is non-empty (existing rule), OR
%%   • LastHasTrail=true (last child had a trailing comment — fix1: a comment
%%     runs to end-of-line so the close must not follow it on the same line).
%% The close aligns with the preceding content/dangling lines (IndStr), never
%% de-indented to the form's open column C (A7·S4b).
-spec close_section([r3lfe_format_cst:trivia()], boolean(), non_neg_integer(),
                    non_neg_integer(), string(), non_neg_integer(), string(), string()) ->
          {iolist(), non_neg_integer()}.
close_section([], false, LastCol, _Indent, _IndStr, _C, _CIndStr, Close) ->
    {Close, LastCol + length(Close)};
close_section(Dangling, _HasTrail, _LastCol, Indent, IndStr, _C, _CIndStr, Close) ->
    DangIO = emit_dangling(Dangling, IndStr),
    {[DangIO, "\n", IndStr, Close], Indent + length(Close)}.

%% print_rest_loop: emit children [c1..cN] each preceded by \n+Indent.
%% Returns {IO, LastCol, LastHasTrailing} where LastHasTrailing is true when
%% the final child carried a trailing comment (used by close_section fix1).
%% IsFirst=true suppresses the leading blank of the first rest child.
-spec print_rest_loop([r3lfe_format_cst:cst_node()], non_neg_integer(),
                      string(), boolean(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
print_rest_loop([Child | Rest], Indent, IndentStr, IsFirst, InData) ->
    LeadIO = emit_child_leading(r3lfe_format_cst:leading(Child), IndentStr, IsFirst),
    {ChildIO, ChildCol}  = print_node(Child, Indent, InData),
    {TrailIO, TrailCol}  = emit_trailing(r3lfe_format_cst:trailing(Child), ChildCol),
    case Rest of
        [] ->
            HasTrail = r3lfe_format_cst:trailing(Child) =/= [],
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO], TrailCol, HasTrail};
        _ ->
            {RestIO, LastCol, HasTrail} = print_rest_loop(Rest, Indent, IndentStr,
                                                          false, InData),
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO | RestIO], LastCol, HasTrail}
    end.

%% print_local_fn_binding: render a single flet/fletrec binding (name args body…)
%% with forced {specform, N} classification (defun-like layout) rather than the
%% funcall/BP layout that a plain-symbol head would normally get.
-spec print_local_fn_binding(r3lfe_format_cst:cst_node(), non_neg_integer(),
                             boolean()) ->
          {iolist(), non_neg_integer()}.
print_local_fn_binding(Binding, C, InData) ->
    Open      = r3lfe_format_lexer:text(r3lfe_format_cst:open(Binding)),
    Close     = r3lfe_format_lexer:text(r3lfe_format_cst:close(Binding)),
    OpenLen   = length(Open),
    CloseLen  = length(Close),
    Dangling  = r3lfe_format_cst:dangling(Binding),
    Indent    = C + 2,
    IndentStr = lists:duplicate(Indent, $\s),
    CIndStr   = lists:duplicate(C, $\s),
    N = local_fn_n(Binding),
    case r3lfe_format_cst:children(Binding) of
        [Head | RestChildren] ->
            case N =:= 0 andalso RestChildren =/= [] andalso all_clauses(RestChildren) of
                true ->
                    %% Match-clause local fn: name on head line, clauses via
                    %% render_clause at +2 (mirrors match-lambda / defun N=1 path).
                    HeadLeadIO = emit_head_leading(r3lfe_format_cst:leading(Head), CIndStr),
                    {HeadIO, HeadCol}  = print_node(Head, C + OpenLen, InData),
                    {HeadTrailIO, _}   = emit_trailing(r3lfe_format_cst:trailing(Head), HeadCol),
                    HeadHasTrail = r3lfe_format_cst:trailing(Head) =/= [],
                    {BodyIO, LastCol, HasTrail} =
                        print_clause_loop(RestChildren, Indent, IndentStr, true, InData),
                    {CloseIO, CloseCol} =
                        close_section(Dangling, HasTrail orelse HeadHasTrail, LastCol,
                                      Indent, IndentStr, C, CIndStr, Close),
                    {[HeadLeadIO, Open, HeadIO, HeadTrailIO, BodyIO, CloseIO], CloseCol};
                false ->
                    print_classified({specform, N}, Head, RestChildren, Dangling,
                                     C, Open, OpenLen, Close, CloseLen,
                                     Indent, IndentStr, CIndStr, InData)
            end;
        [] ->
            {[Open, Close], C + OpenLen + CloseLen}
    end.

%% print_flet_bindlist: render the binding list of an flet/fletrec form as a
%% force-broken container, with each binding rendered via print_local_fn_binding
%% (defun-like) rather than the generic BP/funcall path.
%% Geometry mirrors print_broken_container for the non-map canonical path, but
%% the element renderer is swapped.
-spec print_flet_bindlist(r3lfe_format_cst:cst_node(), non_neg_integer(),
                          boolean()) ->
          {iolist(), non_neg_integer()}.
print_flet_bindlist(BindList, C, InData) ->
    Open      = r3lfe_format_lexer:text(r3lfe_format_cst:open(BindList)),
    Close     = r3lfe_format_lexer:text(r3lfe_format_cst:close(BindList)),
    OpenLen   = length(Open),
    Bindings  = r3lfe_format_cst:children(BindList),
    Dangling  = r3lfe_format_cst:dangling(BindList),
    AlignCol  = C + OpenLen,
    AlignStr  = lists:duplicate(AlignCol, $\s),
    CIndStr   = lists:duplicate(C, $\s),
    case Bindings of
        [] ->
            {[Open, Close], C + OpenLen + length(Close)};
        [First | Rest] ->
            FirstLead = r3lfe_format_cst:leading(First),
            FirstPrefixIO =
                case has_comment_leading(FirstLead) of
                    true  -> ["\n", emit_child_leading(FirstLead, AlignStr, false), AlignStr];
                    false -> []
                end,
            {FirstIO, FirstCol}     = print_local_fn_binding(First, AlignCol, InData),
            {FirstTrailIO, FirstTC} = emit_trailing(
                                        r3lfe_format_cst:trailing(First), FirstCol),
            HasFirstTrail = r3lfe_format_cst:trailing(First) =/= [],
            {RestIO, LastCol, HasTrail} =
                print_local_fn_bindings_loop(Rest, AlignCol, AlignStr,
                                             FirstTC, HasFirstTrail, InData),
            {CloseIO, CloseCol} =
                close_section(Dangling, HasTrail, LastCol,
                              AlignCol, AlignStr, C, CIndStr, Close),
            {[Open, FirstPrefixIO, FirstIO, FirstTrailIO, RestIO, CloseIO], CloseCol}
    end.

%% print_local_fn_bindings_loop: render the 2nd-onward flet bindings, one per
%% line at AlignStr, each via print_local_fn_binding.
-spec print_local_fn_bindings_loop([r3lfe_format_cst:cst_node()],
                                   non_neg_integer(), string(),
                                   non_neg_integer(), boolean(),
                                   boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
print_local_fn_bindings_loop([], _Indent, _IndStr, LastCol, HasTrail, _InData) ->
    {[], LastCol, HasTrail};
print_local_fn_bindings_loop([B | Rest], Indent, IndStr, _PrevCol, _PrevTrail, InData) ->
    LeadIO  = emit_child_leading(r3lfe_format_cst:leading(B), IndStr, false),
    {BIO, BCol}    = print_local_fn_binding(B, Indent, InData),
    {TrailIO, BTC} = emit_trailing(r3lfe_format_cst:trailing(B), BCol),
    HasTrail = r3lfe_format_cst:trailing(B) =/= [],
    {RestIO, LastCol, LastHasTrail} =
        print_local_fn_bindings_loop(Rest, Indent, IndStr, BTC, HasTrail, InData),
    {["\n", LeadIO, IndStr, BIO, TrailIO | RestIO], LastCol, LastHasTrail}.

%% print_clause_loop: like print_rest_loop but uses render_clause for each child.
%% Used for case body clauses and for remaining cond clauses.
-spec print_clause_loop([r3lfe_format_cst:cst_node()], non_neg_integer(),
                        string(), boolean(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
print_clause_loop([Clause | Rest], Indent, IndentStr, IsFirst, InData) ->
    LeadIO = emit_child_leading(r3lfe_format_cst:leading(Clause), IndentStr, IsFirst),
    {ClauseIO, ClauseCol} = render_clause(Clause, Indent, InData),
    {TrailIO, TrailCol}   = emit_trailing(r3lfe_format_cst:trailing(Clause), ClauseCol),
    case Rest of
        [] ->
            HasTrail = r3lfe_format_cst:trailing(Clause) =/= [],
            {["\n", LeadIO, IndentStr, ClauseIO, TrailIO], TrailCol, HasTrail};
        _ ->
            {RestIO, LastCol, HasTrail} = print_clause_loop(Rest, Indent, IndentStr,
                                                            false, InData),
            {["\n", LeadIO, IndentStr, ClauseIO, TrailIO | RestIO], LastCol, HasTrail}
    end.

%% print_receive_body_loop: receive pattern clauses use render_clause, but the
%% (after timeout body...) section is not a clause and keeps generic rendering.
-spec print_receive_body_loop([r3lfe_format_cst:cst_node()], non_neg_integer(),
                              string(), boolean(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
print_receive_body_loop([Child | Rest], Indent, IndentStr, IsFirst, InData) ->
    LeadIO = emit_child_leading(r3lfe_format_cst:leading(Child), IndentStr, IsFirst),
    {ChildIO, ChildCol} =
        case is_after_section(Child) of
            true  -> print_node(Child, Indent, InData);
            false -> render_clause(Child, Indent, InData)
        end,
    {TrailIO, TrailCol} = emit_trailing(r3lfe_format_cst:trailing(Child), ChildCol),
    case Rest of
        [] ->
            HasTrail = r3lfe_format_cst:trailing(Child) =/= [],
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO], TrailCol, HasTrail};
        _ ->
            {RestIO, LastCol, HasTrail} = print_receive_body_loop(Rest, Indent,
                                                                  IndentStr, false, InData),
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO | RestIO], LastCol, HasTrail}
    end.

%% print_try_body_loop: first child is the try body expr (print_node); subsequent
%% children are case/catch/after sections rendered via print_try_section.
-spec print_try_body_loop([r3lfe_format_cst:cst_node()], non_neg_integer(),
                          string(), boolean(), boolean()) ->
          {iolist(), non_neg_integer(), boolean()}.
print_try_body_loop([Child | Rest], Indent, IndentStr, IsFirst, InData) ->
    LeadIO = emit_child_leading(r3lfe_format_cst:leading(Child), IndentStr, IsFirst),
    {ChildIO, ChildCol} =
        case IsFirst of
            true  -> print_node(Child, Indent, InData);
            false -> print_try_section(Child, Indent, InData)
        end,
    {TrailIO, TrailCol} = emit_trailing(r3lfe_format_cst:trailing(Child), ChildCol),
    case Rest of
        [] ->
            HasTrail = r3lfe_format_cst:trailing(Child) =/= [],
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO], TrailCol, HasTrail};
        _ ->
            {RestIO, LastCol, HasTrail} = print_try_body_loop(Rest, Indent, IndentStr,
                                                              false, InData),
            {["\n", LeadIO, IndentStr, ChildIO, TrailIO | RestIO], LastCol, HasTrail}
    end.

%% print_try_section: render a (case/catch/after …) section with the keyword alone
%% on the section line and contents at +2 below (case/catch via print_clause_loop;
%% after via print_rest_loop). Reachable only from print_try_body_loop.
-spec print_try_section(r3lfe_format_cst:cst_node(), non_neg_integer(), boolean()) ->
          {iolist(), non_neg_integer()}.
print_try_section(Section, C, InData) ->
    case r3lfe_format_cst:type(Section) =:= list of
        false ->
            print_node(Section, C, InData);
        true ->
            case r3lfe_format_cst:children(Section) of
                [] ->
                    Open  = r3lfe_format_lexer:text(r3lfe_format_cst:open(Section)),
                    Close = r3lfe_format_lexer:text(r3lfe_format_cst:close(Section)),
                    {[Open, Close], C + length(Open) + length(Close)};
                [SectionHead | Contents] ->
                    case r3lfe_format_cst:type(SectionHead) =:= symbol of
                        false ->
                            print_node(Section, C, InData);
                        true ->
                            Open      = r3lfe_format_lexer:text(r3lfe_format_cst:open(Section)),
                            Close     = r3lfe_format_lexer:text(r3lfe_format_cst:close(Section)),
                            OpenLen   = length(Open),
                            Dangling  = r3lfe_format_cst:dangling(Section),
                            Indent    = C + 2,
                            IndentStr = lists:duplicate(Indent, $\s),
                            CIndStr   = lists:duplicate(C, $\s),
                            HeadLeadIO = emit_head_leading(
                                           r3lfe_format_cst:leading(SectionHead), CIndStr),
                            {HeadIO, HeadCol}  =
                                print_node(SectionHead, C + OpenLen, InData),
                            {HeadTrailIO, _}   =
                                emit_trailing(r3lfe_format_cst:trailing(SectionHead), HeadCol),
                            HeadHasTrail = r3lfe_format_cst:trailing(SectionHead) =/= [],
                            {BodyIO, LastCol, HasTrail} =
                                case Contents of
                                    [] ->
                                        {[], HeadCol, false};
                                    _ ->
                                        case is_after_section(Section) of
                                            true ->
                                                print_rest_loop(Contents, Indent, IndentStr,
                                                                true, InData);
                                            false ->
                                                print_clause_loop(Contents, Indent, IndentStr,
                                                                  true, InData)
                                        end
                                end,
                            {CloseIO, CloseCol} =
                                close_section(Dangling, HasTrail orelse HeadHasTrail, LastCol,
                                              Indent, IndentStr, C, CIndStr, Close),
                            {[HeadLeadIO, Open, HeadIO, HeadTrailIO, BodyIO, CloseIO], CloseCol}
                    end
            end
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
                [] -> [Open, Close];
                Children ->
                    case r3lfe_format_cst:dot_token(Node) of
                        undefined ->
                            [Open, lists:join(" ", [flat_render(C) || C <- Children]), Close];
                        DotTok ->
                            AllButLast = lists:droplast(Children),
                            Tail = lists:last(Children),
                            DotText = r3lfe_format_lexer:text(DotTok),
                            PreRendered = lists:join(" ", [flat_render(C) || C <- AllButLast]),
                            [Open, PreRendered, " ", DotText, " ", flat_render(Tail), Close]
                    end
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
            %% must_break: defforms, maps, and let/let*/case/cond lists always break.
            case must_break(Node) of
                true -> infinity;
                false ->
                    OpenLen  = length(r3lfe_format_lexer:text(r3lfe_format_cst:open(Node))),
                    CloseLen = length(r3lfe_format_lexer:text(r3lfe_format_cst:close(Node))),
                    Children = r3lfe_format_cst:children(Node),
                    DotTok   = r3lfe_format_cst:dot_token(Node),
                    case Children of
                        [] -> OpenLen + CloseLen;
                        _  ->
                            Widths = [flat_width(C) || C <- Children],
                            %% Dotted: " . " before tail adds 2 extra chars vs plain " ".
                            Spaces = case DotTok of
                                undefined -> length(Children) - 1;
                                _         -> length(Children) + 1
                            end,
                            add_widths(OpenLen + CloseLen + Spaces, sum_widths(Widths, 0))
                    end
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
    has_comment_leading(r3lfe_format_cst:leading(Node))
    orelse r3lfe_format_cst:trailing(Node) =/= []
    orelse r3lfe_format_cst:dangling(Node) =/= []
    orelse lists:any(fun has_descendant_trivia/1, r3lfe_format_cst:children(Node)).

%% Blank-only leading does not prevent flat rendering: blanks are always dropped
%% or collapsed in broken mode, so they never carry observable information.
%% Only a leading comment forces broken layout (otherwise it would be silently lost).
-spec has_comment_leading([r3lfe_format_cst:trivia()]) -> boolean().
has_comment_leading([])               -> false;
has_comment_leading([blank | Rest])   -> has_comment_leading(Rest);
has_comment_leading([{comment,_}|_])  -> true.

%%====================================================================
%% Internal: column helpers
%%====================================================================

-spec col_after_text(string(), non_neg_integer()) -> non_neg_integer().
col_after_text([], Col)         -> Col;
col_after_text([$\n | Rest], _) -> col_after_text(Rest, 0);
col_after_text([_ | Rest], Col) -> col_after_text(Rest, Col + 1).
