-module(r3lfe_format_cst_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% Corpus / oracle tests
-export([
    oracle_token_preservation/1,
    oracle_comment_preservation/1,
    oracle_ast_equivalence/1
]).

%% Attachment unit tests
-export([
    attach_trailing_comment/1,
    attach_leading_comment/1,
    attach_blank_line/1,
    attach_dangling/1,
    attach_prefixed_quote/1,
    attach_prefixed_unquote_splicing/1,
    attach_container_map/1,
    attach_container_tuple/1,
    attach_container_binary/1,
    attach_container_lbracket/1
]).

%% nl_before tests (A7·S2a)
-export([
    nl_before_flat_all_false/1,
    nl_before_single_newline/1,
    nl_before_make_op_grouping/1,
    nl_before_top_level_forms/1,
    nl_before_nested_container/1,
    nl_before_quoted_list/1
]).

%% Error tests
-export([
    error_stray_rparen/1,
    error_eof_before_rparen/1,
    error_bracket_mismatch/1,
    error_missing_prefix_target/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [{group, corpus}, {group, attachment}, {group, nl_before}, {group, errors}].

groups() ->
    [
        {corpus, [], [
            oracle_token_preservation,
            oracle_comment_preservation,
            oracle_ast_equivalence
        ]},
        {attachment, [], [
            attach_trailing_comment,
            attach_leading_comment,
            attach_blank_line,
            attach_dangling,
            attach_prefixed_quote,
            attach_prefixed_unquote_splicing,
            attach_container_map,
            attach_container_tuple,
            attach_container_binary,
            attach_container_lbracket
        ]},
        {nl_before, [], [
            nl_before_flat_all_false,
            nl_before_single_newline,
            nl_before_make_op_grouping,
            nl_before_top_level_forms,
            nl_before_nested_container,
            nl_before_quoted_list
        ]},
        {errors, [], [
            error_stray_rparen,
            error_eof_before_rparen,
            error_bracket_mismatch,
            error_missing_prefix_target
        ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Corpus / oracle tests
%%====================================================================

oracle_token_preservation(_Config) ->
    Corpus = corpus_binaries(),
    ct:log("Oracle 1 (token-preservation) over ~p inputs", [length(Corpus)]),
    lists:foreach(fun assert_token_preservation/1, Corpus).

oracle_comment_preservation(_Config) ->
    Corpus = corpus_binaries(),
    ct:log("Oracle 2 (comment-preservation) over ~p inputs", [length(Corpus)]),
    lists:foreach(fun assert_comment_preservation/1, Corpus).

oracle_ast_equivalence(_Config) ->
    %% Exclude #.( (read-eval) and #' (fun_ref: lexer splits #'name/arity into
    %% two tokens; joining with a space produces "#' name/arity" which lfe_scan
    %% rejects — both are documented deviations from naive join-with-spaces).
    Corpus = [B || B <- corpus_binaries(),
                   binary:match(B, <<"#.(">>) =:= nomatch,
                   binary:match(B, <<"#'">>) =:= nomatch],
    ct:log("Oracle 3 (AST-equivalence) over ~p inputs", [length(Corpus)]),
    lists:foreach(fun assert_ast_equivalence/1, Corpus).

%%====================================================================
%% Attachment unit tests (§7)
%%====================================================================

attach_trailing_comment(_Config) ->
    %% (foo) ; bar  =>  list node with trailing [{comment, "; bar"}]
    Src = <<"(foo) ; bar">>,
    {ok, Doc} = parse_src(Src),
    [ListNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(list, r3lfe_format_cst:type(ListNode)),
    ?assertEqual([], r3lfe_format_cst:leading(ListNode)),
    ?assertMatch([{comment, _}], r3lfe_format_cst:trailing(ListNode)),
    [{comment, CTok}] = r3lfe_format_cst:trailing(ListNode),
    ?assertEqual("; bar", r3lfe_format_lexer:text(CTok)),
    %% inner symbol foo has empty trailing
    [FooNode] = r3lfe_format_cst:children(ListNode),
    ?assertEqual([], r3lfe_format_cst:trailing(FooNode)).

attach_leading_comment(_Config) ->
    %% ";;; section\n(defun f () 'ok)"  =>  list node with leading [{comment, ...}]
    Src = <<";;; section\n(defun f () 'ok)">>,
    {ok, Doc} = parse_src(Src),
    [ListNode] = r3lfe_format_cst:document_children(Doc),
    ?assertMatch([{comment, _}], r3lfe_format_cst:leading(ListNode)),
    [{comment, CTok}] = r3lfe_format_cst:leading(ListNode),
    ?assertEqual(";;; section", r3lfe_format_lexer:text(CTok)).

attach_blank_line(_Config) ->
    %% Two forms separated by a blank line: second form's leading has blank.
    Src = <<"foo\n\nbar">>,
    {ok, Doc} = parse_src(Src),
    [_FooNode, BarNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual([blank], r3lfe_format_cst:leading(BarNode)).

attach_dangling(_Config) ->
    %% (a\n  ;; c\n  )  =>  comment is list's dangling, not trailing on a
    Src = <<"(a\n  ;; c\n  )">>,
    {ok, Doc} = parse_src(Src),
    [ListNode] = r3lfe_format_cst:document_children(Doc),
    [ANode] = r3lfe_format_cst:children(ListNode),
    ?assertEqual([], r3lfe_format_cst:trailing(ANode)),
    ?assertMatch([{comment, _}], r3lfe_format_cst:dangling(ListNode)),
    [{comment, CTok}] = r3lfe_format_cst:dangling(ListNode),
    ?assertEqual(";; c", r3lfe_format_lexer:text(CTok)).

attach_prefixed_quote(_Config) ->
    %% 'foo  =>  prefixed node; prefix kind=quote; one symbol child
    {ok, Doc} = parse_src(<<"'foo">>),
    [PNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(prefixed, r3lfe_format_cst:type(PNode)),
    ?assertEqual(quote, r3lfe_format_lexer:kind(r3lfe_format_cst:prefix(PNode))),
    [Inner] = r3lfe_format_cst:children(PNode),
    ?assertEqual(symbol, r3lfe_format_cst:type(Inner)),
    ?assertEqual("foo", r3lfe_format_lexer:text(r3lfe_format_cst:open(Inner))).

attach_prefixed_unquote_splicing(_Config) ->
    %% ,@x  =>  prefixed; prefix kind=unquote_splicing
    {ok, Doc} = parse_src(<<",@x">>),
    [PNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(prefixed, r3lfe_format_cst:type(PNode)),
    ?assertEqual(unquote_splicing,
                 r3lfe_format_lexer:kind(r3lfe_format_cst:prefix(PNode))).

attach_container_map(_Config) ->
    %% #m(k v)  =>  map node
    {ok, Doc} = parse_src(<<"#m(k v)">>),
    [MNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(map, r3lfe_format_cst:type(MNode)),
    ?assertEqual(map_open,
                 r3lfe_format_lexer:kind(r3lfe_format_cst:open(MNode))).

attach_container_tuple(_Config) ->
    %% #(a b)  =>  tuple node
    {ok, Doc} = parse_src(<<"#(a b)">>),
    [TNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(tuple, r3lfe_format_cst:type(TNode)).

attach_container_binary(_Config) ->
    %% #b(1 2)  =>  binary node
    {ok, Doc} = parse_src(<<"#b(1 2)">>),
    [BNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(binary, r3lfe_format_cst:type(BNode)).

attach_container_lbracket(_Config) ->
    %% [a b]  =>  list node whose open kind is lbracket
    {ok, Doc} = parse_src(<<"[a b]">>),
    [LNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(list, r3lfe_format_cst:type(LNode)),
    ?assertEqual(lbracket,
                 r3lfe_format_lexer:kind(r3lfe_format_cst:open(LNode))).

%%====================================================================
%% nl_before tests (A7·S2a)
%%====================================================================

nl_before_flat_all_false(_Config) ->
    %% (a b c) — flat, no newlines — all children nl_before=false.
    {ok, Doc} = parse_src(<<"(a b c)">>),
    [ListNode] = r3lfe_format_cst:document_children(Doc),
    [A, B, C] = r3lfe_format_cst:children(ListNode),
    ?assertNot(r3lfe_format_cst:nl_before(A)),
    ?assertNot(r3lfe_format_cst:nl_before(B)),
    ?assertNot(r3lfe_format_cst:nl_before(C)),
    %% container itself: first top-level form, no preceding newline
    ?assertNot(r3lfe_format_cst:nl_before(ListNode)),
    %% multiline/1: no child has nl_before=true
    ?assertNot(r3lfe_format_cst:multiline(ListNode)).

nl_before_single_newline(_Config) ->
    %% (a b\n  c) — single newline before c — c is nl_before=true.
    {ok, Doc} = parse_src(<<"(a b\n  c)">>),
    [ListNode] = r3lfe_format_cst:document_children(Doc),
    [A, B, C] = r3lfe_format_cst:children(ListNode),
    ?assertNot(r3lfe_format_cst:nl_before(A)),
    ?assertNot(r3lfe_format_cst:nl_before(B)),
    ?assert(r3lfe_format_cst:nl_before(C)),
    %% multiline/1: at least one child has nl_before=true
    ?assert(r3lfe_format_cst:multiline(ListNode)).

nl_before_make_op_grouping(_Config) ->
    %% (make-op action 'x\n  preconds '(...)\n  add-list '(...))
    %% Pairs that start a new line: preconds and add-list are nl_before=true;
    %% the quoted forms after them on the same line are nl_before=false.
    Src = <<"(make-op action 'x\n  preconds '(a b)\n  add-list '(c d))">>,
    {ok, Doc} = parse_src(Src),
    [ListNode] = r3lfe_format_cst:document_children(Doc),
    [MakeOp, Action, QuoteX, Preconds, QuoteAB, AddList, QuoteCD]
        = r3lfe_format_cst:children(ListNode),
    ?assertNot(r3lfe_format_cst:nl_before(MakeOp)),
    ?assertNot(r3lfe_format_cst:nl_before(Action)),
    ?assertNot(r3lfe_format_cst:nl_before(QuoteX)),
    ?assert(r3lfe_format_cst:nl_before(Preconds)),
    ?assertNot(r3lfe_format_cst:nl_before(QuoteAB)),
    ?assert(r3lfe_format_cst:nl_before(AddList)),
    ?assertNot(r3lfe_format_cst:nl_before(QuoteCD)).

nl_before_top_level_forms(_Config) ->
    %% Two top-level forms separated by a single newline: second gets nl_before=true.
    {ok, Doc} = parse_src(<<"(foo)\n(bar)">>),
    [FooNode, BarNode] = r3lfe_format_cst:document_children(Doc),
    ?assertNot(r3lfe_format_cst:nl_before(FooNode)),
    ?assert(r3lfe_format_cst:nl_before(BarNode)),
    %% document-level multiline
    ?assert(r3lfe_format_cst:multiline(Doc)).

nl_before_nested_container(_Config) ->
    %% (outer\n  (a b)\n  (c\n    d))
    %% outer list children: outer-sym, (a b), (c\n    d).
    %% (a b) and (c\n    d) are nl_before=true (newline before each).
    %% inside (c\n    d): c is nl_before=false, d is nl_before=true.
    Src = <<"(outer\n  (a b)\n  (c\n    d))">>,
    {ok, Doc} = parse_src(Src),
    [OuterNode] = r3lfe_format_cst:document_children(Doc),
    [_OuterSym, AB, CD] = r3lfe_format_cst:children(OuterNode),
    ?assert(r3lfe_format_cst:nl_before(AB)),
    ?assert(r3lfe_format_cst:nl_before(CD)),
    [C, D] = r3lfe_format_cst:children(CD),
    ?assertNot(r3lfe_format_cst:nl_before(C)),
    ?assert(r3lfe_format_cst:nl_before(D)).

nl_before_quoted_list(_Config) ->
    %% '(a\n  b) — the prefixed node itself is nl_before=false (first top-level).
    %% The inner list's children: a is false, b is true.
    {ok, Doc} = parse_src(<<"'(a\n  b)">>),
    [PrefixedNode] = r3lfe_format_cst:document_children(Doc),
    ?assertEqual(prefixed, r3lfe_format_cst:type(PrefixedNode)),
    ?assertNot(r3lfe_format_cst:nl_before(PrefixedNode)),
    [InnerList] = r3lfe_format_cst:children(PrefixedNode),
    [A, B] = r3lfe_format_cst:children(InnerList),
    ?assertNot(r3lfe_format_cst:nl_before(A)),
    ?assert(r3lfe_format_cst:nl_before(B)).

%%====================================================================
%% Error tests
%%====================================================================

error_stray_rparen(_Config) ->
    ?assertMatch({error, {unbalanced, eof, _}},
                 parse_src(<<"foo)">>)).

error_eof_before_rparen(_Config) ->
    ?assertMatch({error, {unbalanced, rparen, _}},
                 parse_src(<<"(foo">>)).

error_bracket_mismatch(_Config) ->
    ?assertMatch({error, {unbalanced, rbracket, _}},
                 parse_src(<<"[foo)">>)).

error_missing_prefix_target(_Config) ->
    ?assertMatch({error, {missing_inner_node, _}},
                 parse_src(<<"'">>)).

%%====================================================================
%% Helpers
%%====================================================================

parse_src(Bin) ->
    {ok, Toks} = r3lfe_format_lexer:tokens(Bin),
    r3lfe_format_cst:parse(Toks).

%% assert_token_preservation: Oracle 1.
assert_token_preservation(Bin) ->
    {ok, LexToks} = r3lfe_format_lexer:tokens(Bin),
    SigFromLex = [{r3lfe_format_lexer:kind(T), r3lfe_format_lexer:text(T)}
                  || T <- LexToks, not is_trivia(r3lfe_format_lexer:kind(T))],
    {ok, Doc} = r3lfe_format_cst:parse(LexToks),
    SigFromCst = [{r3lfe_format_lexer:kind(T), r3lfe_format_lexer:text(T)}
                  || T <- r3lfe_format_cst:significant_tokens(Doc)],
    ?assertEqual(SigFromLex, SigFromCst,
                 io_lib:format("token-preservation failed for ~200p", [Bin])).

%% assert_comment_preservation: Oracle 2.
assert_comment_preservation(Bin) ->
    {ok, LexToks} = r3lfe_format_lexer:tokens(Bin),
    CommentsFromLex = [r3lfe_format_lexer:text(T)
                       || T <- LexToks,
                          K <- [r3lfe_format_lexer:kind(T)],
                          K =:= line_comment orelse K =:= block_comment],
    {ok, Doc} = r3lfe_format_cst:parse(LexToks),
    CommentsFromCst = [r3lfe_format_lexer:text(T)
                       || T <- r3lfe_format_cst:comments(Doc)],
    ?assertEqual(CommentsFromLex, CommentsFromCst,
                 io_lib:format("comment-preservation failed for ~200p", [Bin])).

%% assert_ast_equivalence: Oracle 3.
%% Joins significant-token texts with spaces, then compares lfe_io:read_string
%% results for both the original source and the reconstructed text.
assert_ast_equivalence(Bin) ->
    {ok, LexToks} = r3lfe_format_lexer:tokens(Bin),
    {ok, Doc} = r3lfe_format_cst:parse(LexToks),
    SigToks = r3lfe_format_cst:significant_tokens(Doc),
    SigText = lists:flatten(
                lists:join(" ", [r3lfe_format_lexer:text(T) || T <- SigToks])),
    OrigText = binary_to_list(Bin),
    case {lfe_io:read_string(OrigText), lfe_io:read_string(SigText)} of
        {{ok, OrigForms}, {ok, SigForms}} ->
            ?assertEqual(OrigForms, SigForms,
                         io_lib:format("ast-equivalence failed for ~200p", [Bin]));
        {{error, _}, _} ->
            %% Original not parseable by lfe_io (e.g., bare atoms without parens);
            %% skip this oracle for inputs that aren't valid LFE top-level forms.
            ok;
        {_, {error, E}} ->
            ct:fail("lfe_io:read_string failed on significant text: ~p~nSrc: ~p",
                    [E, Bin])
    end.

is_trivia(whitespace)    -> true;
is_trivia(newline)       -> true;
is_trivia(line_comment)  -> true;
is_trivia(block_comment) -> true;
is_trivia(_)             -> false.

%% corpus_binaries: inline snippets + integration files + tq_corpus fixture.
corpus_binaries() ->
    Inline = [
        <<"(foo bar)">>,
        <<"(foo) ; trailing comment">>,
        <<";;; section\n(defun f () 'ok)">>,
        <<"foo\n\nbar">>,
        <<"(a\n  ;; c\n  )">>,
        <<"'foo">>,
        <<"`(,x ,@rest)">>,
        <<"#m(key val)">>,
        <<"#(a b c)">>,
        <<"#b(1 2 3)">>,
        <<"[a b c]">>,
        <<"#| block comment |#\n(foo)">>,
        <<"(defmodule mymod (export (f 0)))\n\n(defun f () 42)">>,
        <<>>
    ],
    FileBins = lists:filtermap(
        fun(F) ->
            case file:read_file(F) of
                {ok, B} -> {true, B};
                _       -> false
            end
        end,
        integration_files() ++ [tq_corpus_file()]
    ),
    Inline ++ FileBins.

integration_files() ->
    TestDir = filename:dirname(filename:absname(?FILE)),
    IntDir = filename:join([TestDir, "..", "_integration"]),
    AllFiles = filelib:wildcard(filename:join([IntDir, "**", "*.lfe"])),
    [F || F <- AllFiles,
          re:run(F, "/_build/", [{capture, none}]) =:= nomatch].

tq_corpus_file() ->
    TestDir = filename:dirname(filename:absname(?FILE)),
    filename:join([TestDir, "r3lfe_format_lexer_SUITE_data", "tq_corpus.lfe"]).
