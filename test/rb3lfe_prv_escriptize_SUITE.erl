-module(rb3lfe_prv_escriptize_SUITE).

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
    escriptize_provider_registers/1,
    run_escript_provider_registers/1,
    escriptize_builds_escript/1,
    run_escript_executes/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        escriptize_provider_registers,
        run_escript_provider_registers,
        escriptize_builds_escript,
        run_escript_executes
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("escript"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

escriptize_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = rb3lfe_prv_escriptize:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Escriptize provider should be registered"),

    ok.

run_escript_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = rb3lfe_prv_run_escript:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Run-escript provider should be registered"),

    ok.

escriptize_builds_escript(_Config) ->
    %% Test that escriptize provider can be invoked
    %% This is a minimal test since actual escript building requires
    %% a properly configured project with escript section in rebar.config
    State = rebar_state:new(),

    %% Initialize the provider
    {ok, State1} = rb3lfe_prv_escriptize:init(State),

    %% Verify provider was registered
    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),

    %% Note: We don't call do/1 here because it requires a valid project
    %% with escript configuration. The provider registration itself
    %% validates the module is loadable and properly structured.

    ok.

run_escript_executes(_Config) ->
    %% Test that run-escript provider can be invoked
    %% This is a minimal test since actual execution requires
    %% a built escript to exist
    State = rebar_state:new(),

    %% Initialize the provider
    {ok, State1} = rb3lfe_prv_run_escript:init(State),

    %% Verify provider was registered
    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),

    %% Test that calling without an escript returns appropriate error
    State2 = rebar_state:command_parsed_args(State1, {[], []}),
    Result = rb3lfe_prv_run_escript:do(State2),

    %% Should get an error since no escript exists
    ?assertMatch({error, _}, Result),

    ok.
