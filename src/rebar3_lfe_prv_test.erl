-module(rebar3_lfe_prv_test).

-export([init/1,
         do/1,
         format_error/1]).

-define(NAMESPACE, lfe).
-define(PROVIDER, ltest).
-define(DEPS, [compile]).
-define(DEFAULT_TEST_TYPE, unit).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Run LFE tests that have been created with the ltest project.",
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                {namespace, ?NAMESPACE},
                {name, ?PROVIDER},
                {module, ?MODULE},
                {bare, true},
                {deps, ?DEPS},
                {example, "rebar3 lfe ltest"},
                {short_desc, Description},
                {desc, info(Description)},
                {opts, []}
            ])
    ),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, test(State)}.

-spec format_error(any()) -> iolist().
format_error({unknown_app, Unknown}) ->
    io_lib:format("Applications list for test contains an unrecognizable application definition: ~p", [Unknown]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

info(Description) ->
    io_lib:format(
            "~n~s~n"
            "~n"
            "Run the unit, system, and integration tests "
            "for the project.~n"
            "~n",
            [Description]).

%% Calculate effective test type based on command parsed args, rebar.config and
%% default test-type value.
effective_test_type(State) ->
    Args = rebar_state:command_parsed_args(State),
    CmdArgs = element(1, Args),
    StateArgs = rebar_state:get(State, ?PROVIDER, []),
    TestTypeCmd = proplists:get_value('test-type', CmdArgs),
    TestTypeState = proplists:get_value('test-type', StateArgs),
    case TestTypeCmd of
        undefined ->
            case TestTypeState of
                undefined -> ?DEFAULT_TEST_TYPE;
                _ -> TestTypeState
            end;
        _ -> TestTypeCmd
    end.

test(Config) ->
    case effective_test_type(Config) of
        all -> 'ltest-runner':all();
        unit -> 'ltest-runner':unit();
        system -> 'ltest-runner':system();
        integration -> 'test-runner':integration();
        UnknownType ->
            rebar_api:error("Unknown test-type value: ~p", [UnknownType])
    end,
    {ok, Config}.
