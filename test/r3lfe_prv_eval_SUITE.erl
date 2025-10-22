-module(r3lfe_prv_eval_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    eval_provider_registers/1,
    eval_simple_arithmetic/1,
    eval_multiplication/1,
    eval_list_operations/1,
    eval_lambda_expression/1,
    eval_nested_expression/1,
    eval_conditional/1,
    eval_let_binding/1,
    eval_function_call/1,
    eval_atom_result/1,
    eval_string_result/1,
    eval_empty_list/1,
    eval_missing_expression_error/1,
    eval_empty_string_error/1,
    eval_invalid_syntax_error/1,
    eval_undefined_function_error/1,
    eval_parse_error/1,
    eval_format_error_messages/1,
    eval_format_result/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        eval_provider_registers,
        eval_simple_arithmetic,
        eval_multiplication,
        eval_list_operations,
        eval_lambda_expression,
        eval_nested_expression,
        eval_conditional,
        eval_let_binding,
        eval_function_call,
        eval_atom_result,
        eval_string_result,
        eval_empty_list,
        eval_missing_expression_error,
        eval_empty_string_error,
        eval_invalid_syntax_error,
        eval_undefined_function_error,
        eval_parse_error,
        eval_format_error_messages,
        eval_format_result
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

eval_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_eval:init(State),

    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),
    ok.

eval_simple_arithmetic(_Config) ->
    %% Test: (+ 1 2 3)
    %% Expected: 6
    Result = r3lfe_prv_eval:eval_expression("(+ 1 2 3)"),
    ?assertMatch({ok, 6}, Result),
    ok.

eval_multiplication(_Config) ->
    %% Test: (* 2 (+ 1 2 3 4 5 6))
    %% Expected: 42
    Result = r3lfe_prv_eval:eval_expression("(* 2 (+ 1 2 3 4 5 6))"),
    ?assertMatch({ok, 42}, Result),
    ok.

eval_list_operations(_Config) ->
    %% Test: (length (list 1 2 3 4 5))
    %% Expected: 5
    Result = r3lfe_prv_eval:eval_expression("(length (list 1 2 3 4 5))"),
    ?assertMatch({ok, 5}, Result),
    ok.

eval_lambda_expression(_Config) ->
    %% Test: (lists:map (lambda (x) (* x x)) (list 1 2 3))
    %% Expected: [1, 4, 9]
    Result = r3lfe_prv_eval:eval_expression("(lists:map (lambda (x) (* x x)) (list 1 2 3))"),
    ?assertMatch({ok, [1, 4, 9]}, Result),
    ok.

eval_nested_expression(_Config) ->
    %% Test: (let ((x 10) (y 20)) (+ x y))
    %% Expected: 30
    Result = r3lfe_prv_eval:eval_expression("(let ((x 10) (y 20)) (+ x y))"),
    ?assertMatch({ok, 30}, Result),
    ok.

eval_conditional(_Config) ->
    %% Test: (if (> 5 3) 'yes 'no)
    %% Expected: yes
    Result = r3lfe_prv_eval:eval_expression("(if (> 5 3) 'yes 'no)"),
    ?assertMatch({ok, yes}, Result),
    ok.

eval_let_binding(_Config) ->
    %% Test: (let ((x 5)) (* x x))
    %% Expected: 25
    Result = r3lfe_prv_eval:eval_expression("(let ((x 5)) (* x x))"),
    ?assertMatch({ok, 25}, Result),
    ok.

eval_function_call(_Config) ->
    %% Test: (- 10 3)
    %% Expected: 7
    Result = r3lfe_prv_eval:eval_expression("(- 10 3)"),
    ?assertMatch({ok, 7}, Result),
    ok.

eval_atom_result(_Config) ->
    %% Test: 'test-atom
    %% Expected: test-atom
    Result = r3lfe_prv_eval:eval_expression("'test-atom"),
    ?assertMatch({ok, 'test-atom'}, Result),
    ok.

eval_string_result(_Config) ->
    %% Test: "hello"
    %% Expected: "hello"
    Result = r3lfe_prv_eval:eval_expression("\"hello\""),
    ?assertMatch({ok, "hello"}, Result),
    ok.

eval_empty_list(_Config) ->
    %% Test: ()
    %% Expected: []
    Result = r3lfe_prv_eval:eval_expression("()"),
    ?assertMatch({ok, []}, Result),
    ok.

eval_missing_expression_error(_Config) ->
    %% Test that format_error handles missing expression
    Error = r3lfe_prv_eval:format_error({missing_expr}),
    ?assert(is_list(Error)),
    ?assert(length(Error) > 0),
    ok.

eval_empty_string_error(_Config) ->
    %% Test: ""
    %% Expected: Error
    Result = r3lfe_prv_eval:eval_expression(""),
    ?assertMatch({error, _}, Result),
    ok.

eval_invalid_syntax_error(_Config) ->
    %% Test: (+ 1 2
    %% Expected: Incomplete expression error
    Result = r3lfe_prv_eval:eval_expression("(+ 1 2"),
    ?assertMatch({error, _}, Result),
    ok.

eval_undefined_function_error(_Config) ->
    %% Test: (nonexistent:function 1 2)
    %% Expected: Evaluation error
    Result = r3lfe_prv_eval:eval_expression("(nonexistent:function 1 2)"),
    ?assertMatch({error, _}, Result),
    ok.

eval_parse_error(_Config) ->
    %% Test: Invalid LFE syntax
    Result = r3lfe_prv_eval:eval_expression("((("),
    ?assertMatch({error, _}, Result),
    ok.

eval_format_error_messages(_Config) ->
    %% Test error message formatting
    Error1 = {missing_expr},
    Error2 = {scan_error, "some error"},
    Error3 = {parse_error, "bad syntax"},
    Error4 = {eval_error, badarith, []},
    Error5 = {throw, some_value},
    Error6 = {exit, normal},
    Error7 = unknown_error,

    Msg1 = r3lfe_prv_eval:format_error(Error1),
    Msg2 = r3lfe_prv_eval:format_error(Error2),
    Msg3 = r3lfe_prv_eval:format_error(Error3),
    Msg4 = r3lfe_prv_eval:format_error(Error4),
    Msg5 = r3lfe_prv_eval:format_error(Error5),
    Msg6 = r3lfe_prv_eval:format_error(Error6),
    Msg7 = r3lfe_prv_eval:format_error(Error7),

    ?assert(is_list(Msg1)),
    ?assert(is_list(Msg2)),
    ?assert(is_list(Msg3)),
    ?assert(is_list(Msg4)),
    ?assert(is_list(Msg5)),
    ?assert(is_list(Msg6)),
    ?assert(is_list(Msg7)),

    ok.

eval_format_result(_Config) ->
    %% Test that format_result works correctly
    %% We can't easily capture io:format output in CT, but we can verify it doesn't crash
    ok = r3lfe_prv_eval:format_result(42),
    ok = r3lfe_prv_eval:format_result([1, 2, 3]),
    ok = r3lfe_prv_eval:format_result(test_atom),
    ok = r3lfe_prv_eval:format_result("test string"),
    ok.
