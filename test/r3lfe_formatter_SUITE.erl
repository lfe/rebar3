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

%% indent group (Arc A4·S1)
-export([
    indent_funcall_align/1,
    indent_specform_case/1,
    indent_specform_progn/1,
    indent_specform_if/1,
    indent_list_head/1,
    indent_defform_provisional/1
]).

%% fix1 group (A4·S1·fix1 — comment-ends-line)
-export([
    fix1_close_after_trailing_progn/1,
    fix1_close_after_trailing_case/1,
    fix1_close_after_trailing_funcall/1,
    fix1_close_after_trailing_list_head/1,
    fix1_dist_arg_trailing_comment/1,
    fix1_dist_arg_leading_comment/1
]).

%% fix2 group (A4·S1·fix2 — head trailing comment + matrix)
-export([
    fix2_funcall_head_trail_args/1,
    fix2_funcall_head_trail_no_args/1,
    fix2_specform_case_head_trail/1,
    fix2_specform_progn_head_trail_no_args/1,
    fix2_specform_progn_head_trail_args/1,
    fix2_specform_call_head_trail/1,
    fix2_defun_head_trail/1,
    fix2_list_head_head_trail/1,
    fix2_combination_head_and_body_trail/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [{group, flat}, {group, breaking},
     {group, comments}, {group, edge}, {group, oracles},
     {group, indent}, {group, fix1}, {group, fix2}].

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
        ]},
        {indent, [], [
            indent_funcall_align,
            indent_specform_case,
            indent_specform_progn,
            indent_specform_if,
            indent_list_head,
            indent_defform_provisional
        ]},
        {fix1, [], [
            fix1_close_after_trailing_progn,
            fix1_close_after_trailing_case,
            fix1_close_after_trailing_funcall,
            fix1_close_after_trailing_list_head,
            fix1_dist_arg_trailing_comment,
            fix1_dist_arg_leading_comment
        ]},
        {fix2, [], [
            fix2_funcall_head_trail_args,
            fix2_funcall_head_trail_no_args,
            fix2_specform_case_head_trail,
            fix2_specform_progn_head_trail_no_args,
            fix2_specform_progn_head_trail_args,
            fix2_specform_call_head_trail,
            fix2_defun_head_trail,
            fix2_list_head_head_trail,
            fix2_combination_head_and_body_trail
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
    %% funcall rule (A4): a1 on head line, a2 aligned under a1.
    %% "(foo " ++ 71 X's ++ " bar)" = 81 chars -> must break.
    %% AlignCol = 0+1+3+1=5; X's fit at col 5 (5+71=76≤80); bar at col 5.
    %% Updated from A3 (+2 hang) for A4 funcall rule.
    Xs = list_to_binary(lists:duplicate(71, $X)),
    Input    = <<"(foo ", Xs/binary, " bar)">>,
    Expected = <<"(foo ", Xs/binary, "\n     bar)\n">>,
    assert_format(Input, Expected).

breaking_golden_nested_inner_fits(_Config) ->
    %% funcall rule (A4): outer breaks, inner (a b) fits flat at AlignCol=7.
    %% "(outer " + 73 x's + " (a b) baz)" = 91 chars > 80.
    %% AlignCol = 0+1+5+1=7 ("outer"=5); x's at col 7 (7+73=80); rest at col 7.
    %% Updated from A3 (+2 hang) for A4 funcall rule.
    Xs2 = list_to_binary(lists:duplicate(73, $x)),
    Input2    = <<"(outer ", Xs2/binary, " (a b) baz)">>,
    Expected2 = <<"(outer ", Xs2/binary, "\n       (a b)\n       baz)\n">>,
    assert_format(Input2, Expected2).

breaking_golden_single_child(_Config) ->
    %% funcall rule (A4): head 77 h's, a1=baz.
    %% AlignCol=79; 79+3=82>80, baz can't break (symbol) → stays on head line.
    %% Output is one 82-char line; no cross-line break possible for single symbol arg.
    %% Updated from A3 (+2 hang) for A4 funcall rule.
    Head = list_to_binary(lists:duplicate(77, $h)),
    Input    = <<"(", Head/binary, " baz)">>,
    Expected = <<"(", Head/binary, " baz)\n">>,
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
    %% funcall rule (A4): alpha is head, beta is a1; fits on head line → (alpha beta).
    %% Two \n are needed for alpha to get leading=[blank]; single \n is structural.
    %% Updated from A3 (+2 hang) for A4 funcall rule.
    Src = <<"(\n\nalpha beta)">>,
    {ok, IO1} = r3lfe_formatter:format(Src),
    Out1 = iolist_to_binary(IO1),
    %% First pass: alpha's blank forces break; funcall puts beta on head line → flat
    ?assertEqual(<<"(alpha beta)\n">>, Out1),
    %% Idempotent: both passes give (alpha beta)
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
    %% A form > 80 cols with trailing comment: funcall rule (A4).
    %% AlignCol=5; X's at col 5; bar at col 5; trailing stays on last line.
    %% Updated from A3 (+2 hang) for A4 funcall rule.
    Xs = list_to_binary(lists:duplicate(71, $X)),
    Src = <<"(foo ", Xs/binary, " bar) ; note">>,
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    Expected = <<"(foo ", Xs/binary, "\n     bar) ; note\n">>,
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
        <<"(;; c\nalpha (b c))">>,
        %% A4·S1·fix1: trailing comment on last child — close on own line
        <<"(progn\n  a\n  b ; note\n  )">>,
        <<"(case x\n  (ok y) ; good\n  )">>,
        <<"(some-fn a\n         b ; note\n         )">>,
        <<"((a b)\n (c d) ; note\n )">>,
        %% A4·S1·fix1: N≥2 specform with comment on distinguished arg
        <<"(call mod fun ; trailing\n  arg1 arg2)">>,
        <<"(: mod\n  fun arg1 arg2)">>,
        %% A4·S1·fix2: head trailing comment forces body layout
        <<"(foo ; c\n  a1\n  a2)">>,
        <<"(foo ; c\n)">>,
        <<"(case ; c\n  expr\n  (ok yes))">>,
        <<"(progn ; c\n)">>,
        <<"(progn ; c\n  a\n  b)">>,
        <<"(call ; c\n  mod\n  fun\n  arg)">>,
        <<"(defun ; c\n  name\n  args\n  body)">>,
        <<"(foo ; c\n  a\n  b ; note\n)">>
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

%%====================================================================
%% indent group — Arc A4·S1 goldens
%%====================================================================

indent_funcall_align(_Config) ->
    %% funcall: a1 on head line; a2..aN aligned under a1's column.
    %% "some-function" = 13 chars; AlignCol = 0+1+13+1 = 15.
    %% Three 22-char args → flat_width = 2+13+3+22*3+2 = 86 > 80.
    A = list_to_binary(lists:duplicate(22, $a)),
    B = list_to_binary(lists:duplicate(22, $b)),
    C = list_to_binary(lists:duplicate(22, $c)),
    Input    = <<"(some-function ", A/binary, " ", B/binary, " ", C/binary, ")">>,
    Pad      = list_to_binary(lists:duplicate(15, $\s)),
    Expected = <<"(some-function ", A/binary, "\n",
                 Pad/binary, B/binary, "\n",
                 Pad/binary, C/binary, ")\n">>,
    assert_format(Input, Expected),
    assert_idempotent(Input).

indent_specform_case(_Config) ->
    %% specform 1 (case): distinguished expr on head line; clauses at C+2.
    %% (case aa (a b) ... (w x)) with 12 clauses = 81 chars > 80.
    Input = <<"(case aa (a b) (c d) (e f) (g h) (i j) (k l) (m n) (o p) (q r) (s t) (u v) (w x))">>,
    Expected = <<"(case aa\n  (a b)\n  (c d)\n  (e f)\n  (g h)\n  (i j)\n  (k l)\n  (m n)\n  (o p)\n  (q r)\n  (s t)\n  (u v)\n  (w x))\n">>,
    assert_format(Input, Expected),
    assert_idempotent(Input).

indent_specform_progn(_Config) ->
    %% specform 0 (progn): head alone on opener line; all args at C+2.
    %% 13 five-char items = flat_width 84 > 80.
    Input = <<"(progn aaaaa bbbbb ccccc ddddd eeeee fffff ggggg hhhhh iiiii jjjjj kkkkk lllll mmmmm)">>,
    Expected = <<"(progn\n  aaaaa\n  bbbbb\n  ccccc\n  ddddd\n  eeeee\n  fffff\n  ggggg\n  hhhhh\n  iiiii\n  jjjjj\n  kkkkk\n  lllll\n  mmmmm)\n">>,
    assert_format(Input, Expected),
    assert_idempotent(Input).

indent_specform_if(_Config) ->
    %% specform 1 (if): distinguished condition on head line; branches at C+2.
    %% flat_width > 80 with long predicate.
    Cond = <<"(some-long-predicate-function arg-one arg-two arg-three arg-four arg-five)">>,
    Input    = <<"(if ", Cond/binary, " yes no)">>,
    Expected = <<"(if ", Cond/binary, "\n  yes\n  no)\n">>,
    assert_format(Input, Expected),
    assert_idempotent(Input).

indent_list_head(_Config) ->
    %% list_head: head not a symbol; all elements aligned under first at C+1.
    %% Six `(aa bb cc dd)` items (14 chars each) = flat_width 91 > 80.
    Input = <<"((aa bb cc dd) (ee ff gg hh) (ii jj kk ll) (mm nn oo pp) (qq rr ss tt) (uu vv ww xx))">>,
    Expected = <<"((aa bb cc dd)\n (ee ff gg hh)\n (ii jj kk ll)\n (mm nn oo pp)\n (qq rr ss tt)\n (uu vv ww xx))\n">>,
    assert_format(Input, Expected),
    assert_idempotent(Input).

indent_defform_provisional(_Config) ->
    %% defform (provisional = specform 1): name (d1) on head line; rest at C+2.
    %% S2 will refine this with proper signature-line + docstring layout.
    Args = <<"(a b c d e f g h i j k l m n o p q)">>,
    Body = <<"(+ a b c d e f g h i j k l m n o p q)">>,
    Input = <<"(defun my-function ", Args/binary, " ", Body/binary, ")">>,
    %% Provisional: my-function is d1 on head line; args and body at C+2.
    Expected = <<"(defun my-function\n  ", Args/binary, "\n  ", Body/binary, ")\n">>,
    assert_format(Input, Expected),
    assert_idempotent(Input).

%%====================================================================
%% fix1 group — A4·S1·fix1: comment-ends-line regressions
%%====================================================================

fix1_close_after_trailing_progn(_Config) ->
    %% Trailing comment on last body child of a progn: close must go on its own line.
    Src = <<"(progn\n  a\n  b ; note\n  )">>,
    assert_format(Src, <<"(progn\n  a\n  b ; note\n)\n">>),
    assert_idempotent(Src),
    assert_token_preservation(Src).

fix1_close_after_trailing_case(_Config) ->
    %% Trailing comment on last clause of a case: close on own line.
    Src = <<"(case x\n  (ok y) ; good\n  )">>,
    assert_format(Src, <<"(case x\n  (ok y) ; good\n)\n">>),
    assert_idempotent(Src),
    assert_token_preservation(Src).

fix1_close_after_trailing_funcall(_Config) ->
    %% Trailing comment on last aligned arg of funcall: close on own line.
    Src = <<"(some-fn a\n         b ; note\n         )">>,
    assert_format(Src, <<"(some-fn a\n         b ; note\n)\n">>),
    assert_idempotent(Src),
    assert_token_preservation(Src).

fix1_close_after_trailing_list_head(_Config) ->
    %% Trailing comment on last element of list_head list: close on own line.
    Src = <<"((a b)\n (c d) ; note\n )">>,
    assert_format(Src, <<"((a b)\n (c d) ; note\n)\n">>),
    assert_idempotent(Src),
    assert_token_preservation(Src).

fix1_dist_arg_trailing_comment(_Config) ->
    %% Trailing comment on a non-last distinguished arg (call, N=2):
    %% must fall back to body layout so neither arg nor close is swallowed.
    Src = <<"(call mod fun ; trailing\n  arg1 arg2)">>,
    %% Falls back: all RestChildren at C+2; trailing comment on fun forces close
    %% to own line since fun becomes last on its line (but there's a body here).
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    %% Verify the closing paren is present (not swallowed by comment)
    ?assert(binary:match(Out, <<")">>) =/= nomatch,
            "closing paren must survive trailing comment on dist arg"),
    assert_idempotent(Src),
    assert_token_preservation(Src).

fix1_dist_arg_leading_comment(_Config) ->
    %% Leading comment on the 2nd distinguished arg of `:` (N=2): fall back to body.
    Src = <<"(:\n  mod\n  ;; c\n  fun arg1 arg2)">>,
    {ok, OutIO} = r3lfe_formatter:format(Src),
    Out = iolist_to_binary(OutIO),
    ?assert(binary:match(Out, <<")">>) =/= nomatch,
            "closing paren must survive leading comment on 2nd dist arg"),
    assert_idempotent(Src),
    assert_token_preservation(Src).

%%====================================================================
%% fix2 group — A4·S1·fix2: head trailing comment matrix
%%====================================================================

%% Helper: verify output is valid (paren intact), token-preserved, idempotent.
assert_fix2(Src, Expected) ->
    assert_format(Src, Expected),
    assert_idempotent(Src),
    assert_token_preservation(Src),
    %% Paren intact — closing paren must be present and not swallowed
    ?assert(binary:match(Expected, <<")">>) =/= nomatch).

fix2_funcall_head_trail_args(_Config) ->
    %% funcall: head trailing comment → all args fall to body at C+2.
    assert_fix2(<<"(foo ; c\n  a1\n  a2)">>,
                <<"(foo ; c\n  a1\n  a2)\n">>).

fix2_funcall_head_trail_no_args(_Config) ->
    %% funcall: head trailing, no args → close on its own line (not swallowed).
    %% Note: (foo ; c) without \n before ) would have ) consumed by the comment;
    %% the source must have a newline before ).
    assert_fix2(<<"(foo ; c\n)">>,
                <<"(foo ; c\n)\n">>).

fix2_specform_case_head_trail(_Config) ->
    %% specform N=1 (case): head trailing → all RestChildren to body.
    assert_fix2(<<"(case ; c\n  expr\n  (ok yes))">>,
                <<"(case ; c\n  expr\n  (ok yes))\n">>).

fix2_specform_progn_head_trail_no_args(_Config) ->
    %% specform N=0 (progn): head trailing, no args → close on own line.
    %% Need \n before ) so ) is not consumed by the line comment.
    assert_fix2(<<"(progn ; c\n)">>,
                <<"(progn ; c\n)\n">>).

fix2_specform_progn_head_trail_args(_Config) ->
    %% specform N=0 (progn): head trailing with body args.
    assert_fix2(<<"(progn ; c\n  a\n  b)">>,
                <<"(progn ; c\n  a\n  b)\n">>).

fix2_specform_call_head_trail(_Config) ->
    %% specform N=2 (call): head trailing → all args to body (no args on head line).
    assert_fix2(<<"(call ; c\n  mod\n  fun\n  arg)">>,
                <<"(call ; c\n  mod\n  fun\n  arg)\n">>).

fix2_defun_head_trail(_Config) ->
    %% defform (= specform 1): head trailing → all RestChildren to body.
    assert_fix2(<<"(defun ; c\n  name\n  args\n  body)">>,
                <<"(defun ; c\n  name\n  args\n  body)\n">>).

fix2_list_head_head_trail(_Config) ->
    %% list_head: head trailing is already safe (rest starts with \n).
    %% Verify it stays correct and idempotent.
    assert_fix2(<<"((a b) ; c\n (c d))">>,
                <<"((a b) ; c\n (c d))\n">>).

fix2_combination_head_and_body_trail(_Config) ->
    %% Head trailing + last body child trailing → close on its own line.
    %% Need \n before ) so ) is not swallowed by the trailing comment.
    assert_fix2(<<"(foo ; c\n  a\n  b ; note\n)">>,
                <<"(foo ; c\n  a\n  b ; note\n)\n">>).
