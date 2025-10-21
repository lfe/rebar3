-module(r3lfe_prv_ltest).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, ltest).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Run LFE tests using ltest",

    Opts = [
        {suite, $s, "suite", string,
         "Test suite to run"},
        {test, $t, "test", string,
         "Specific test to run"},
        {verbose, $v, "verbose", boolean,
         "Verbose output"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe ltest"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE ltest provider starting", []),

    %% Ensure ltest is available
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            run_tests(State);
        {error, _} ->
            {error, "ltest not found. Add {ltest, \"~> 0.13\"} to deps in rebar.config"}
    end.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec run_tests(rebar_state:t()) -> {ok, rebar_state:t()}.
run_tests(State) ->
    %% Set up code paths (deps, plugins, and project apps for testing)
    rebar_paths:set_paths([deps, plugins, runtime], State),
    add_test_paths(State),

    %% Get test options
    {Opts, _Args} = rebar_state:command_parsed_args(State),

    %% Run ltest
    TestOpts = build_test_opts(Opts),

    ?INFO("Running LFE tests...", []),

    Result = ltest:run(TestOpts),

    case Result of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, format_error(Reason)}
    end.

-spec add_test_paths(rebar_state:t()) -> ok.
add_test_paths(State) ->
    Apps = rebar_state:project_apps(State),

    lists:foreach(
        fun(AppInfo) ->
            AppDir = rebar_app_info:dir(AppInfo),
            TestDir = filename:join(AppDir, "test"),
            case filelib:is_dir(TestDir) of
                true ->
                    code:add_patha(TestDir);
                false ->
                    ok
            end
        end,
        Apps
    ),

    ok.

-spec build_test_opts(proplists:proplist()) -> map().
build_test_opts(Opts) ->
    DefaultOpts = ltest:'default-opts'(),

    %% Override with command line options
    maps:merge(
        DefaultOpts,
        maps:from_list(Opts)
    ).

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Runs LFE tests using the ltest framework.~n"
        "~n"
        "Requires ltest as a dependency:~n"
        "  {deps, [{ltest, \"~~> 0.13\"}]}.~n"
        "~n"
        "Options:~n"
        "  --suite SUITE    Run specific test suite~n"
        "  --test TEST      Run specific test~n"
        "  --verbose        Enable verbose output~n",
        [Description]
    ).
