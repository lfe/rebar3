%%%% CST parser: flat lossless token stream -> tree with comment/trivia attachment.
%%%% See docs/design/022-lfe-format/arc2-cst/cc-prompt.md.
%%%%
%%%% Note: Erlang reserves node/0 and document/0 as built-in or common names,
%%%% so the exported opaque types are cst_node/0 and cst_document/0.
-module(r3lfe_format_cst).

-export([parse/1,
         significant_tokens/1, comments/1,
         type/1, open/1, close/1, prefix/1, children/1,
         leading/1, trailing/1, dangling/1,
         nl_before/1, multiline/1, dot_token/1,
         document_children/1, document_dangling/1]).

-export_type([cst_node/0, cst_document/0, node_type/0, trivia/0]).

-type token() :: r3lfe_format_lexer:token().

-type trivia() :: {comment, token()} | blank.

-type node_type() :: list | tuple | map | binary | eval
                   | symbol | number | string | char
                   | prefixed.

-record(node, {
    type      :: node_type(),
    open      :: token() | undefined,
    close     :: token() | undefined,
    prefix    :: token() | undefined,
    children  :: [#node{}],
    leading   :: [trivia()],
    trailing  :: [trivia()],
    dangling  :: [trivia()],
    nl_before = false :: boolean(),
    dot_token = undefined :: token() | undefined
}).

-record(document, {
    children :: [#node{}],
    dangling :: [trivia()]
}).

-opaque cst_node()     :: #node{}.
-opaque cst_document() :: #document{}.

%%====================================================================
%% Exported API
%%====================================================================

-spec parse([token()]) -> {ok, cst_document()} | {error, term()}.
parse(Tokens) ->
    case parse_seq_loop(Tokens, eof, [], false, []) of
        {ok, Nodes, none, Dangling, []} ->
            {ok, #document{children = Nodes, dangling = Dangling}};
        {ok, _Nodes, {_, _}, _Dangling, _} ->
            {error, {unexpected_dot, 0}};
        {ok, _Nodes, _DotInfo, _Dangling, [Tok | _]} ->
            {error, {unbalanced, eof, r3lfe_format_lexer:line(Tok)}};
        {error, _} = Err ->
            Err
    end.

-spec significant_tokens(cst_document()) -> [token()].
significant_tokens(#document{children = Nodes}) ->
    lists:flatmap(fun node_significant_tokens/1, Nodes).

-spec comments(cst_document()) -> [token()].
comments(#document{children = Nodes, dangling = Dangling}) ->
    lists:flatmap(fun node_comments/1, Nodes) ++ trivia_comments(Dangling).

-spec type(cst_node()) -> node_type().
type(#node{type = T}) -> T.

-spec open(cst_node()) -> token() | undefined.
open(#node{open = O}) -> O.

-spec close(cst_node()) -> token() | undefined.
close(#node{close = C}) -> C.

-spec prefix(cst_node()) -> token() | undefined.
prefix(#node{prefix = P}) -> P.

-spec children(cst_node()) -> [cst_node()].
children(#node{children = Ch}) -> Ch.

-spec leading(cst_node()) -> [trivia()].
leading(#node{leading = L}) -> L.

-spec trailing(cst_node()) -> [trivia()].
trailing(#node{trailing = T}) -> T.

-spec dangling(cst_node()) -> [trivia()].
dangling(#node{dangling = D}) -> D.

%% nl_before/1: true if a newline preceded this node among its siblings in the
%% source (i.e. the author started a new line here). Single \n is sufficient.
-spec nl_before(cst_node()) -> boolean().
nl_before(#node{nl_before = NL}) -> NL.

%% dot_token/1: the cons-dot token for an improper list, or undefined.
%% When present, the last child of the node is the cons-tail.
-spec dot_token(cst_node()) -> token() | undefined.
dot_token(#node{dot_token = D}) -> D.

%% multiline/1: true if any direct child of the node/document has nl_before=true.
-spec multiline(cst_node() | cst_document()) -> boolean().
multiline(#document{children = Children}) ->
    lists:any(fun(N) -> nl_before(N) end, Children);
multiline(#node{children = Children}) ->
    lists:any(fun(N) -> nl_before(N) end, Children).

-spec document_children(cst_document()) -> [cst_node()].
document_children(#document{children = Ch}) -> Ch.

-spec document_dangling(cst_document()) -> [trivia()].
document_dangling(#document{dangling = D}) -> D.

%%====================================================================
%% Internal: main parse loop
%%====================================================================

%% parse_seq_loop: parse tokens until matching CloserKind (rparen|rbracket|eof)
%% or EOF. Returns {ok, Nodes, Dangling, Rest} where Rest starts with the
%% (unconsumed) closer token, or [] at EOF. Pending holds leading trivia
%% accumulated for the next node, in source order. NlBefore tracks whether at
%% least one newline has been seen since the last node (or start of container),
%% and is stamped onto the next node then reset to false.
%%
%% Pending uses ++ for append: it is bounded by file-level trivia density
%% (never accumulates proportionally to input size) so O(n) per append is fine.
parse_seq_loop([], _CloserKind, Pending, _NlBefore, Nodes) ->
    {ok, lists:reverse(Nodes), none, Pending, []};
parse_seq_loop([Tok | Rest], CloserKind, Pending, NlBefore, Nodes) ->
    Kind = r3lfe_format_lexer:kind(Tok),
    case Kind of
        whitespace ->
            parse_seq_loop(Rest, CloserKind, Pending, NlBefore, Nodes);
        newline ->
            {Pending2, Rest2} = consume_newlines_inner(Rest, Pending, 1),
            parse_seq_loop(Rest2, CloserKind, Pending2, true, Nodes);
        line_comment ->
            parse_seq_loop(Rest, CloserKind, Pending ++ [{comment, Tok}], NlBefore, Nodes);
        block_comment ->
            parse_seq_loop(Rest, CloserKind, Pending ++ [{comment, Tok}], NlBefore, Nodes);
        rparen ->
            case CloserKind of
                rparen -> {ok, lists:reverse(Nodes), none, Pending, [Tok | Rest]};
                _      -> {error, {unbalanced, CloserKind, r3lfe_format_lexer:line(Tok)}}
            end;
        rbracket ->
            case CloserKind of
                rbracket -> {ok, lists:reverse(Nodes), none, Pending, [Tok | Rest]};
                _        -> {error, {unbalanced, CloserKind, r3lfe_format_lexer:line(Tok)}}
            end;
        dot when CloserKind =/= eof ->
            %% Cons-dot inside a container: the next node is the improper tail.
            case parse_one_node(Rest, [], false) of
                {ok, TailNode, Rest2} ->
                    {ok, lists:reverse([TailNode | Nodes]), {Tok, TailNode}, Pending, Rest2};
                {error, _} = Err -> Err
            end;
        dot ->
            {error, {unexpected_dot, r3lfe_format_lexer:line(Tok)}};
        K when K =:= lparen; K =:= lbracket; K =:= tuple_open;
               K =:= map_open; K =:= binary_open; K =:= eval_open ->
            case parse_container(Tok, K, Rest, Pending, NlBefore) of
                {ok, CNode, Rest2} ->
                    {CNode2, Rest3} = try_attach_trailing(CNode, Rest2),
                    parse_seq_loop(Rest3, CloserKind, [], false, [CNode2 | Nodes]);
                {error, _} = Err -> Err
            end;
        K when K =:= quote; K =:= quasiquote; K =:= unquote;
               K =:= unquote_splicing; K =:= fun_ref ->
            case parse_one_node(Rest, [], false) of
                {ok, Inner, Rest2} ->
                    PNode = #node{type = prefixed, prefix = Tok,
                                  open = undefined, close = undefined,
                                  children = [Inner], leading = Pending,
                                  trailing = [], dangling = [],
                                  nl_before = NlBefore},
                    {PNode2, Rest3} = try_attach_trailing(PNode, Rest2),
                    parse_seq_loop(Rest3, CloserKind, [], false, [PNode2 | Nodes]);
                {error, _} = Err -> Err
            end;
        _ ->
            %% Leaf: symbol, qsymbol, number, char, string, bstring, tqstring, tqbstring
            LNode = #node{type = leaf_type(Kind), open = Tok,
                          close = undefined, prefix = undefined,
                          children = [], leading = Pending,
                          trailing = [], dangling = [],
                          nl_before = NlBefore},
            {LNode2, Rest2} = try_attach_trailing(LNode, Rest),
            parse_seq_loop(Rest2, CloserKind, [], false, [LNode2 | Nodes])
    end.

%%====================================================================
%% Internal: parse exactly one node (for prefix inner-form parsing)
%%====================================================================

%% parse_one_node: skip trivia (accumulating into Pending), then parse one node.
%% Trivia between a prefix and its target attaches to the inner node's leading.
%% NlBefore tracks whether a newline occurred between the prefix token and the
%% inner form (e.g. '\n(foo) has NlBefore=true on the inner list node).
parse_one_node([], _Pending, _NlBefore) ->
    {error, {missing_inner_node, 0}};
parse_one_node([Tok | Rest], Pending, NlBefore) ->
    Kind = r3lfe_format_lexer:kind(Tok),
    case Kind of
        whitespace ->
            parse_one_node(Rest, Pending, NlBefore);
        newline ->
            {Pending2, Rest2} = consume_newlines_inner(Rest, Pending, 1),
            parse_one_node(Rest2, Pending2, true);
        K when K =:= line_comment; K =:= block_comment ->
            parse_one_node(Rest, Pending ++ [{comment, Tok}], NlBefore);
        K when K =:= lparen; K =:= lbracket; K =:= tuple_open;
               K =:= map_open; K =:= binary_open; K =:= eval_open ->
            case parse_container(Tok, K, Rest, Pending, NlBefore) of
                {ok, CNode, Rest2} ->
                    {CNode2, Rest3} = try_attach_trailing(CNode, Rest2),
                    {ok, CNode2, Rest3};
                {error, _} = Err -> Err
            end;
        K when K =:= quote; K =:= quasiquote; K =:= unquote;
               K =:= unquote_splicing; K =:= fun_ref ->
            case parse_one_node(Rest, [], false) of
                {ok, Inner, Rest2} ->
                    PNode = #node{type = prefixed, prefix = Tok,
                                  open = undefined, close = undefined,
                                  children = [Inner], leading = Pending,
                                  trailing = [], dangling = [],
                                  nl_before = NlBefore},
                    {PNode2, Rest3} = try_attach_trailing(PNode, Rest2),
                    {ok, PNode2, Rest3};
                {error, _} = Err -> Err
            end;
        K when K =:= rparen; K =:= rbracket ->
            {error, {missing_inner_node, r3lfe_format_lexer:line(Tok)}};
        _ ->
            LNode = #node{type = leaf_type(Kind), open = Tok,
                          close = undefined, prefix = undefined,
                          children = [], leading = Pending,
                          trailing = [], dangling = [],
                          nl_before = NlBefore},
            {LNode2, Rest2} = try_attach_trailing(LNode, Rest),
            {ok, LNode2, Rest2}
    end.

%%====================================================================
%% Internal: container parsing
%%====================================================================

parse_container(OpenerTok, OpenerKind, Rest, Leading, NlBefore) ->
    {ContainerType, CloserKind} = container_type(OpenerKind),
    case parse_seq_loop(Rest, CloserKind, [], false, []) of
        {ok, Children, DotInfo, Dangling, [CloserTok | Rest2]} ->
            DotTok = case DotInfo of
                none          -> undefined;
                {DotTok1, _}  -> DotTok1
            end,
            CNode = #node{type = ContainerType,
                          open = OpenerTok, close = CloserTok,
                          prefix = undefined, children = Children,
                          dangling = Dangling, leading = Leading,
                          trailing = [], nl_before = NlBefore,
                          dot_token = DotTok},
            {ok, CNode, Rest2};
        {ok, _Children, _DotInfo, _Dangling, []} ->
            {error, {unbalanced, CloserKind, r3lfe_format_lexer:line(OpenerTok)}};
        {error, _} = Err ->
            Err
    end.

container_type(lparen)      -> {list,   rparen};
container_type(lbracket)    -> {list,   rbracket};
container_type(tuple_open)  -> {tuple,  rparen};
container_type(map_open)    -> {map,    rparen};
container_type(binary_open) -> {binary, rparen};
container_type(eval_open)   -> {eval,   rparen}.

%%====================================================================
%% Internal: trailing-comment attachment (§4)
%%====================================================================

%% try_attach_trailing: skip whitespace (not newlines); if next is a comment,
%% attach it as the node's single trailing trivia and consume it.
try_attach_trailing(ANode, [Tok | Rest]) ->
    case r3lfe_format_lexer:kind(Tok) of
        whitespace ->
            try_attach_trailing(ANode, Rest);
        K when K =:= line_comment; K =:= block_comment ->
            {ANode#node{trailing = [{comment, Tok}]}, Rest};
        _ ->
            {ANode, [Tok | Rest]}
    end;
try_attach_trailing(ANode, []) ->
    {ANode, []}.

%%====================================================================
%% Internal: blank-line detection
%%====================================================================

%% consume_newlines_inner: called after consuming one newline (Count=1).
%% Consumes further newlines and interleaved whitespace. Appends a single
%% `blank` to Pending if total newline count >= 2.
consume_newlines_inner([Tok | Rest], Pending, Count) ->
    case r3lfe_format_lexer:kind(Tok) of
        newline    -> consume_newlines_inner(Rest, Pending, Count + 1);
        whitespace -> consume_newlines_inner(Rest, Pending, Count);
        _          -> {append_blank_if(Count >= 2, Pending), [Tok | Rest]}
    end;
consume_newlines_inner([], Pending, Count) ->
    {append_blank_if(Count >= 2, Pending), []}.

append_blank_if(true,  P) -> P ++ [blank];
append_blank_if(false, P) -> P.

%%====================================================================
%% Internal: helpers
%%====================================================================

leaf_type(symbol)    -> symbol;
leaf_type(qsymbol)   -> symbol;
leaf_type(number)    -> number;
leaf_type(char)      -> char;
leaf_type(string)    -> string;
leaf_type(bstring)   -> string;
leaf_type(tqstring)  -> string;
leaf_type(tqbstring) -> string.

%%====================================================================
%% Internal: oracle helpers
%%====================================================================

%% node_significant_tokens: in-order significant tokens for one node.
node_significant_tokens(#node{type = prefixed, prefix = Pfx,
                               children = Children}) ->
    [Pfx | lists:flatmap(fun node_significant_tokens/1, Children)];
node_significant_tokens(#node{type = T, open = Open, close = Close,
                               dot_token = DotTok, children = Children})
  when T =:= list; T =:= tuple; T =:= map; T =:= binary; T =:= eval ->
    case DotTok of
        undefined ->
            [Open | lists:flatmap(fun node_significant_tokens/1, Children)] ++ [Close];
        _ when Children =/= [] ->
            AllButLast = lists:droplast(Children),
            Tail = lists:last(Children),
            [Open]
            ++ lists:flatmap(fun node_significant_tokens/1, AllButLast)
            ++ [DotTok]
            ++ node_significant_tokens(Tail)
            ++ [Close]
    end;
node_significant_tokens(#node{open = Open}) ->
    [Open].

%% node_comments: comment tokens in source order
%% (leading → children → dangling → trailing).
node_comments(#node{leading = L, children = Ch, dangling = D, trailing = T}) ->
    trivia_comments(L)
    ++ lists:flatmap(fun node_comments/1, Ch)
    ++ trivia_comments(D)
    ++ trivia_comments(T).

trivia_comments(Trivia) ->
    [Tok || {comment, Tok} <- Trivia].
