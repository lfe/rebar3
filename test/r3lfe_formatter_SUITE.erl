-module(r3lfe_formatter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% flat group
-export([
    flat_idempotency/1,
    flat_token_preservation/1,
    flat_ast_equivalence/1,
    flat_golden_defun/1,
    flat_golden_nested/1,
    flat_golden_prefix_glue/1,
    flat_golden_containers/1,
    flat_golden_blank_between_toplevel/1
]).

%% breaking group
-export([
    breaking_idempotency/1,
    breaking_token_preservation/1,
    breaking_ast_equivalence/1,
    breaking_golden_wide_form/1,
    breaking_golden_nested_inner_fits/1,
    breaking_golden_single_child/1,
    breaking_tqstring_verbatim/1
]).

%% comments group
-export([
    comment_leading/1,
    comment_trailing/1,
    comment_dangling/1,
    comment_blank_between/1,
    comment_block_inside/1,
    comment_head_leading_comment/1,
    comment_head_leading_single_child/1,
    comment_head_leading_blank_only/1
]).

%% edge group
-export([
    edge_empty_file/1,
    edge_comment_only/1,
    edge_crlf_normalised/1,
    edge_tqstring_form/1,
    edge_wide_with_comment/1
]).

%% oracles group (full Arc A3 acceptance)
-export([
    oracle_idempotency/1,
    oracle_token_preservation/1,
    oracle_comment_preservation/1,
    oracle_ast_equivalence/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [{group, flat}, {group, breaking},
     {group, comments}, {group, edge}, {group, oracles}].

groups() ->
    [
        {flat, [], [
            flat_idempotency,
            flat_token_preservation,
            flat_ast_equivalence,
            flat_golden_defun,
            flat_golden_nested,
            flat_golden_prefix_glue,
            flat_golden_containers,
            flat_golden_blank_between_toplevel
        ]},
        {breaking, [], [
            breaking_idempotency,
            breaking_token_preservation,
            breaking_ast_equivalence,
            breaking_golden_wide_form,
            breaking_golden_nested_inner_fits,
            breaking_golden_single_child,
            breaking_tqstring_verbatim
        ]},
        {comments, [], [
            comment_leading,
            comment_trailing,
            comment_dangling,
            comment_blank_between,
            comment_block_inside,
            comment_head_leading_comment,
            comment_head_leading_single_child,
            comment_head_leading_blank_only
        ]},
        {edge, [], [
            edge_empty_file,
            edge_comment_only,
            edge_crlf_normalised,
            edge_tqstring_form,
            edge_wide_with_comment
        ]},
        {oracles, [], [
            oracle_idempotency,
            oracle_token_preservation,
            oracle_comment_preservation,
            oracle_ast_equivalence
        ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% flat group
%%====================================================================

flat_idempotency(_Config) ->
    Inputs = flat_corpus(),
    lists:foreach(fun assert_idempotent/1, Inputs).

flat_token_preservation(_Config) ->
    Inputs = flat_corpus(),
    lists:foreach(fun assert_token_preservation/1, Inputs).

flat_ast_equivalence(_Config) ->
    %% Exclude #.( (read-eval). #' fun_ref IS included: the printer glues prefix
    %% to inner, so "#'foo/1" round-trips through lfe_io correctly (unlike A2's
    %% oracle which joined with spaces).
    Inputs = [I || I <- flat_corpus(),
                   binary:match(I, <<"#.(">>) =:= nomatch],
    lists:foreach(fun assert_ast_equiv/1, Inputs).

flat_golden_defun(_Config) ->
    assert_format(<<"(defun f (x) (+ x 1))">>,
                  <<"(defun f (x) (+ x 1))\n">>).

flat_golden_nested(_Config) ->
    assert_format(<<"(a (b c) d)">>, <<"(a (b c) d)\n">>).

flat_golden_prefix_glue(_Config) ->
    %% Prefixes must glue to their target — no space.
    assert_format(<<"'foo">>,    <<"'foo\n">>),
    assert_format(<<"`(,x)">>,   <<"`(,x)\n">>),
    assert_format(<<",@rest">>,  <<",@rest\n">>),
    assert_format(<<"#'foo/1">>, <<"#'foo/1\n">>).

flat_golden_containers(_Config) ->
    assert_format(<<"()">>,      <<"()\n">>),
    assert_format(<<"#m(k v)">>, <<"#m(k v)\n">>),
    assert_format(<<"[a b]">>,   <<"[a b]\n">>),
    assert_format(<<"#(a b)">>,  <<"#(a b)\n">>),
    assert_format(<<"#b(1 2)">>, <<"#b(1 2)\n">>).

flat_golden_blank_between_toplevel(_Config) ->
    %% A blank line between top-level forms is preserved as exactly one empty line.
    assert_format(<<"foo\n\nbar">>,     <<"foo\n\nbar\n">>),
    assert_format(<<"foo\n\n\n\nbar">>, <<"foo\n\nbar\n">>).

%%====================================================================
%% Helpers
%%====================================================================

flat_corpus() ->
    [
        <<"(defun f (x) (+ x 1))">>,
        <<"(a (b c) d)">>,
        <<"'foo">>,
        <<",@rest">>,
        <<"#'foo/1">>,
        <<"#m(k v)">>,
        <<"[a b]">>,
        <<"#(a b)">>,
        <<"#b(1 2)">>,
        <<"()">>,
        <<"foo\n\nbar">>,
        <<"(defmodule m (export (f 0)))\n\n(defun f () 42)">>,
        <<>>  %% empty file
    ].

assert_format(Input, Expected) ->
    {ok, OutIO} = r3lfe_formatter:format(Input),
    ?assertEqual(Expected, iolist_to_binary(OutIO),
                 io_lib:format("format(~p)", [Input])).

assert_idempotent(Input) ->
    {ok, Out1IO} = r3lfe_formatter:format(Input),
    Out1 = iolist_to_binary(Out1IO),
    {ok, Out2IO} = r3lfe_formatter:format(Out1),
    Out2 = iolist_to_binary(Out2IO),
    ?assertEqual(Out1, Out2,
                 io_lib:format("idempotency failed for ~200p", [Input])).

assert_token_preservation(Input) ->
    {ok, OutIO} = r3lfe_formatter:format(Input),
    OutBin = iolist_to_binary(OutIO),
    SigIn  = sig_pairs(Input),
    SigOut = sig_pairs(OutBin),
    ?assertEqual(SigIn, SigOut,
                 io_lib:format("token-preservation failed for ~200p", [Input])).

assert_ast_equiv(Input) ->
    {ok, OutIO} = r3lfe_formatter:format(Input),
    OutBin = iolist_to_binary(OutIO),
    OrigText = binary_to_list(iolist_to_binary([Input])),
    OutText  = binary_to_list(OutBin),
    case {lfe_io:read_string(OrigText), lfe_io:read_string(OutText)} of
        {{ok, OrigForms}, {ok, OutForms}} ->
            ?assertEqual(OrigForms, OutForms,
                         io_lib:format("ast-equiv failed for ~200p", [Input]));
        {{error, _}, _} ->
            ok;  %% original not parseable as forms (e.g. bare atoms), skip
        {_, {error, E}} ->
            ct:fail("lfe_io failed on formatted output: ~p~nInput: ~200p", [E, Input])
    end.

sig_pairs(Bin) ->
    {ok, Toks} = r3lfe_format_lexer:tokens(Bin),
    {ok, Doc}  = r3lfe_format_cst:parse(Toks),
    [{r3lfe_format_lexer:kind(T), r3lfe_format_lexer:text(T)}
     || T <- r3lfe_format_cst:significant_tokens(Doc)].

%%====================================================================
%% breaking group
%%====================================================================

breaking_idempotency(_Config) ->
    lists:foreach(fun assert_idempotent/1, breaking_corpus()).

breaking_token_preservation(_Config) ->
    lists:foreach(fun assert_token_preservation/1, breaking_corpus()).

breaking_ast_equivalence(_Config) ->
    Inputs = [I || I <- breaking_corpus(),
                   binary:match(I, <<"#.(">>) =:= nomatch],
    lists:foreach(fun assert_ast_equiv/1, Inputs).

breaking_golden_wide_form(_Config) ->
    %% A form > 80 cols: breaks to (+2 hanging).
    %% "(foo " ++ 71 X's ++ " bar)" = 81 chars -> must break.
    Xs = list_to_binary(lists:duplicate(71, $X)),
    Input    = <<"(foo ", Xs/binary, " bar)">>,
    Expected = <<"(foo\n  ", Xs/binary, "\n  bar)\n">>,
    assert_format(Input, Expected).

breaking_golden_nested_inner_fits(_Config) ->
    %% Outer breaks; inner list fits flat at its indent of 2.
    %% "(outer (inner-a inner-b) baz)" fits in 30 cols, but let's make outer break.
    %% Total: "(outer " + 50 X's + " (a b) baz)" where X's push it past 80.
    %% "(outer " + 73 x's + " (a b) baz)" = 7+73+6+5 = 91 > 80 -> breaks
    Xs2 = list_to_binary(lists:duplicate(73, $x)),
    Input2    = <<"(outer ", Xs2/binary, " (a b) baz)">>,
    Expected2 = <<"(outer\n  ", Xs2/binary, "\n  (a b)\n  baz)\n">>,
    assert_format(Input2, Expected2).

breaking_golden_single_child(_Config) ->
    %% Container with one child that is too wide to fit flat:
    %% "(foo)" at any column fits trivially; test a 1-child container that breaks
    %% because the whole thing > 80.
    %% "(some-really-long-head)" = 24 chars, always flat.
    %% Make a container with a head that breaks itself:
    %% "(outer (inner 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))"
    %%   -> outer + inner. Inner flat_width = "(inner 1 2 ... 23)" - let me use simple:
    %% Simple: "(a b)" = 5 chars, at col 0 fits. Single child means close hugs head.
    %% Test that a 2-child list where head is the only wide part breaks correctly.
    %% (head trailing-baz) where head is 77 chars -> total 83 > 80
    Head = list_to_binary(lists:duplicate(77, $h)),
    Input = <<"(", Head/binary, " baz)">>,
    %% Broken: "(h...h\n  baz)" - head at col 1 (1+77=78 <= 80, fits flat as head!)
    Expected = <<"(", Head/binary, "\n  baz)\n">>,
    assert_format(Input, Expected).

breaking_tqstring_verbatim(_Config) ->
    %% A tqstring in a container: the tqstring is never flat (infinity width),
    %% so the container must break. The tqstring text is emitted verbatim.
    Input = <<"\"\"\"\nhello\n\"\"\"">>,
    {ok, OutIO} = r3lfe_formatter:format(Input),
    Out = iolist_to_binary(OutIO),
    %% The tqstring token text is verbatim; top-level gets \n appended.
    ?assertEqual(<<"\"\"\"\nhello\n\"\"\"\n">>, Out),
    assert_idempotent(Input).

breaking_corpus() ->
    Xs = list_to_binary(lists:duplicate(73, $x)),
    Head = list_to_binary(lists:duplicate(77, $h)),
    [
        %% Forms that fit flat (regression: S2 must not break these)
        <<"(defun f (x) (+ x 1))">>,
        <<"(a (b c) d)">>,
        <<"()">>,
        %% Wide forms that must break
        <<"(foo ", (list_to_binary(lists:duplicate(71, $X)))/binary, " bar)">>,
        <<"(outer ", Xs/binary, " (a b) baz)">>,
        <<"(", Head/binary, " baz)">>,
        %% Deeply nested
        <<"(a (b (c (d (e (f (g 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)))))))">>,
        %% tqstring
        <<"\"\"\"\nhello\n\"\"\"">>
    ].

%%====================================================================
%% comments group
%%====================================================================

comment_leading(_Config) ->
    %% Leading own-line comment before a list form
    Src = <<";;; section\n(defun f () ok)">>,
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    ?assertEqual(<<";;; section\n(defun f () ok)\n">>, Out),
    assert_idempotent(Src).

comment_trailing(_Config) ->
    %% Trailing comment on same line as a form
    Src = <<"(foo bar) ; note">>,
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    ?assertEqual(<<"(foo bar) ; note\n">>, Out),
    assert_idempotent(Src).

comment_dangling(_Config) ->
    %% Dangling comment before closing paren: close on its own line
    Src = <<"(a\n  ;; c\n  )">>,
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    %% Broken: (a\n  ;; c\n), close on own line at C=0
    ?assertEqual(<<"(a\n  ;; c\n)\n">>, Out),
    assert_idempotent(Src).

comment_blank_between(_Config) ->
    %% Blank line between top-level forms is preserved as exactly one empty line
    Src = <<"foo\n\nbar">>,
    assert_format(Src, <<"foo\n\nbar\n">>),
    %% Multiple blanks collapse to one
    assert_format(<<"foo\n\n\n\nbar">>, <<"foo\n\nbar\n">>).

comment_block_inside(_Config) ->
    %% Block comment inside a form, on its own line
    Src = <<"(foo\n  #| block |#\n  bar)">>,
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    ?assertEqual(<<"(foo\n  #| block |#\n  bar)\n">>, Out),
    assert_idempotent(Src).

comment_head_leading_comment(_Config) ->
    %% The exact repro from fix1: comment between ( and head child.
    %% Opener stands alone; all children (head included) at indent.
    Src = <<"(;; c\nalpha beta)">>,
    assert_format(Src, <<"(\n  ;; c\n  alpha\n  beta)\n">>),
    assert_idempotent(Src),
    %% Nested child that itself fits flat:
    Src2 = <<"(;; c\nalpha (b c))">>,
    assert_format(Src2, <<"(\n  ;; c\n  alpha\n  (b c))\n">>),
    assert_idempotent(Src2).

comment_head_leading_single_child(_Config) ->
    %% Single child with head-leading comment: opener alone, one child at indent.
    Src = <<"(;; c\nalpha)">>,
    assert_format(Src, <<"(\n  ;; c\n  alpha)\n">>),
    assert_idempotent(Src).

comment_head_leading_blank_only(_Config) ->
    %% Head has blank-only leading (no comment): stays on opener line, blank dropped.
    %% Two \n are needed: a single \n after ( is structural and produces no blank in
    %% the head's leading; two \n trigger the blank accumulator.
    Src = <<"(\n\nalpha beta)">>,
    {ok, IO1} = r3lfe_formatter:format(Src),
    Out1 = iolist_to_binary(IO1),
    %% First pass: alpha's blank-in-leading forces break; blank dropped; head on opener
    ?assertEqual(<<"(alpha\n  beta)\n">>, Out1),
    %% Second pass: no trivia left → flat
    assert_format(Out1, <<"(alpha beta)\n">>).

%%====================================================================
%% edge group
%%====================================================================

edge_empty_file(_Config) ->
    %% Empty input → empty output (no trailing \n; no forms to end).
    assert_format(<<>>, <<>>).

edge_comment_only(_Config) ->
    %% A file with only a comment (no top-level forms).
    assert_format(<<"; only a comment">>, <<"; only a comment\n">>),
    assert_idempotent(<<"; only a comment">>).

edge_crlf_normalised(_Config) ->
    %% CRLF → LF: A1 lexes \r as whitespace (dropped by CST).
    {ok, OutIO} = r3lfe_formatter:format(<<"foo\r\nbar">>),
    Out = iolist_to_binary(OutIO),
    ?assertEqual(<<"foo\nbar\n">>, Out).

edge_tqstring_form(_Config) ->
    %% A form containing a triple-quoted string: emitted verbatim.
    Input = <<"\"\"\"\nhello world\n\"\"\"">>,
    assert_format(Input, <<"\"\"\"\nhello world\n\"\"\"\n">>),
    assert_idempotent(Input).

edge_wide_with_comment(_Config) ->
    %% A form > 80 cols that also has a trailing comment: must break.
    Xs = list_to_binary(lists:duplicate(71, $X)),
    Src = <<"(foo ", Xs/binary, " bar) ; note">>,
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    %% form breaks; trailing comment stays on the last line before \n
    Expected = <<"(foo\n  ", Xs/binary, "\n  bar) ; note\n">>,
    ?assertEqual(Expected, Out),
    assert_idempotent(Src).

%%====================================================================
%% oracles group — full Arc A3 acceptance
%%====================================================================

oracle_idempotency(_Config) ->
    Corpus = full_corpus(),
    ct:log("Oracle 1 (idempotency) over ~p inputs", [length(Corpus)]),
    lists:foreach(fun assert_idempotent/1, Corpus).

oracle_token_preservation(_Config) ->
    Corpus = full_corpus(),
    ct:log("Oracle 2 (token-preservation) over ~p inputs", [length(Corpus)]),
    lists:foreach(fun assert_token_preservation/1, Corpus).

oracle_comment_preservation(_Config) ->
    Corpus = full_corpus(),
    ct:log("Oracle 3 (comment-preservation) over ~p inputs", [length(Corpus)]),
    lists:foreach(fun assert_comment_preservation/1, Corpus).

oracle_ast_equivalence(_Config) ->
    %% Exclude only #.( (read-eval). #' fun_ref IS included: the printer glues
    %% prefixes, so "#'foo/1" in output is re-read correctly by lfe_io.
    Corpus = [B || B <- full_corpus(),
                   binary:match(B, <<"#.(">>) =:= nomatch],
    ct:log("Oracle 4 (AST-equivalence) over ~p inputs (excl. read-eval)",
           [length(Corpus)]),
    lists:foreach(fun assert_ast_equiv/1, Corpus).

%%====================================================================
%% Full corpus (inline snippets + integration files + tq_corpus)
%%====================================================================

full_corpus() ->
    Inline = [
        %% comment-free basics
        <<"(defun f (x) (+ x 1))">>,
        <<"(a (b c) d)">>,
        <<"'foo">>,
        <<"#'foo/1">>,
        <<"#m(k v)">>,
        <<"[a b]">>,
        <<"()">>,
        <<>>,
        %% blank-line forms
        <<"foo\n\nbar">>,
        <<"(defmodule m (export (f 0)))\n\n(defun f () 42)">>,
        %% wide forms
        <<"(foo ", (list_to_binary(lists:duplicate(71, $X)))/binary, " bar)">>,
        %% comment forms
        <<";;; section\n(defun f () ok)">>,
        <<"(foo bar) ; trailing">>,
        <<"(a\n  ;; c\n  )">>,
        <<"(foo\n  #| block |#\n  bar)">>,
        %% tqstring
        <<"\"\"\"\nhello\n\"\"\"">>,
        %% comment-only
        <<"; only a comment">>,
        %% fix1: head-leading comment (idempotency repro)
        <<"(;; c\nalpha beta)">>,
        <<"(;; c\nalpha)">>,
        <<"(;; c\nalpha (b c))">>
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
    IntDir  = filename:join([TestDir, "..", "_integration"]),
    AllFiles = filelib:wildcard(filename:join([IntDir, "**", "*.lfe"])),
    [F || F <- AllFiles,
          re:run(F, "/_build/", [{capture, none}]) =:= nomatch].

tq_corpus_file() ->
    TestDir = filename:dirname(filename:absname(?FILE)),
    filename:join([TestDir, "r3lfe_format_lexer_SUITE_data", "tq_corpus.lfe"]).

%%====================================================================
%% Shared assertion helpers (comment-preservation added)
%%====================================================================

assert_comment_preservation(Input) ->
    {ok, InToks} = r3lfe_format_lexer:tokens(Input),
    {ok, InDoc}  = r3lfe_format_cst:parse(InToks),
    InComments   = [r3lfe_format_lexer:text(T)
                    || T <- r3lfe_format_cst:comments(InDoc)],
    {ok, OutIO}  = r3lfe_formatter:format(Input),
    OutBin       = iolist_to_binary(OutIO),
    {ok, OutToks} = r3lfe_format_lexer:tokens(OutBin),
    {ok, OutDoc}  = r3lfe_format_cst:parse(OutToks),
    OutComments   = [r3lfe_format_lexer:text(T)
                     || T <- r3lfe_format_cst:comments(OutDoc)],
    ?assertEqual(InComments, OutComments,
                 io_lib:format("comment-preservation failed for ~200p", [Input])).
