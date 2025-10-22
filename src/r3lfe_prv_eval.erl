-module(r3lfe_prv_eval).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

%% Exported for testing
-ifdef(TEST).
-export([
    eval_expression/1,
    format_result/1
]).
-endif.

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, eval).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Evaluate an LFE expression",

    Opts = [
        {expr, undefined, undefined, string,
         "LFE expression to evaluate (required)"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe eval '(+ 1 2 3)'"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE eval provider starting", []),

    %% Get command-line arguments
    {Opts, Args} = rebar_state:command_parsed_args(State),

    %% Extract expression - could be from --expr option or positional argument
    ExprString = case proplists:get_value(expr, Opts) of
        undefined when length(Args) > 0 ->
            %% Use first positional argument if no --expr flag
            hd(Args);
        undefined ->
            undefined;
        Expr ->
            Expr
    end,

    case ExprString of
        undefined ->
            {error, {missing_expr}};
        "" ->
            {error, {missing_expr}};
        _ ->
            %% Set up code paths to ensure LFE and dependencies are available
            rebar_paths:set_paths([deps, plugins, runtime], State),

            %% Ensure LFE is available
            case code:ensure_loaded(lfe_eval) of
                {module, lfe_eval} ->
                    evaluate_and_print(ExprString, State);
                {error, _} ->
                    {error, "LFE not found. Ensure LFE is available as a dependency."}
            end
    end.

-spec format_error(term()) -> iolist().
format_error({missing_expr}) ->
    "Error: No expression provided. Usage: rebar3 lfe eval '<expression>'";
format_error({scan_error, Error}) ->
    io_lib:format("Scan error: ~p", [Error]);
format_error({parse_error, Error}) ->
    io_lib:format("Parse error: ~p", [Error]);
format_error({eval_error, Reason, Stack}) ->
    io_lib:format("Evaluation error: ~p~n~p", [Reason, Stack]);
format_error({throw, Reason}) ->
    io_lib:format("Expression threw: ~p", [Reason]);
format_error({exit, Reason}) ->
    io_lib:format("Expression exited: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("Error: ~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec evaluate_and_print(string(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
evaluate_and_print(ExprString, State) ->
    case eval_expression(ExprString) of
        {ok, Result} ->
            format_result(Result),
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

-spec eval_expression(string()) -> {ok, term()} | {error, term()}.
eval_expression(ExprString) ->
    try
        %% Parse the expression
        case lfe_scan:string(ExprString) of
            {ok, Tokens, _Line} ->
                case lfe_parse:sexpr(Tokens) of
                    {ok, _LineNum, Form, _Remaining} ->
                        %% Evaluate in new environment
                        Env = lfe_env:new(),
                        Result = lfe_eval:expr(Form, Env),
                        {ok, Result};
                    {more, _Continuation} ->
                        {error, "Incomplete expression"};
                    {error, ParseError} ->
                        {error, {parse_error, ParseError}}
                end;
            {error, ScanError, _} ->
                {error, {scan_error, ScanError}}
        end
    catch
        error:Reason:Stack ->
            {error, {eval_error, Reason, Stack}};
        throw:Reason ->
            {error, {throw, Reason}};
        exit:Reason ->
            {error, {exit, Reason}}
    end.

-spec format_result(term()) -> ok.
format_result(Result) ->
    %% Print result in LFE format
    ResultStr = lfe_io:print1(Result),
    io:format("~ts~n", [ResultStr]),
    ok.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Evaluates a single LFE expression and prints the result.~n"
        "~n"
        "Usage:~n"
        "  rebar3 lfe eval '<expression>'~n"
        "~n"
        "Examples:~n"
        "  rebar3 lfe eval '(+ 1 2 3)'~n"
        "  rebar3 lfe eval '(lists:map (lambda (x) (* x x)) (list 1 2 3))'~n"
        "  rebar3 lfe eval '(: io format \"Hello, LFE!~~n\" ())'~n"
        "~n"
        "Note: The expression must be quoted to prevent shell interpretation.~n"
        "      Use single quotes for simple expressions, or escape characters~n"
        "      as needed for your shell.~n",
        [Description]
    ).
