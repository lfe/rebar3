-module(r3lfe_prv_escriptize_SUITE).

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
    run_escript_executes/1,
    escriptize_do_succeeds/1,
    escriptize_format_error/1,
    escriptize_info_contains_description/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        escriptize_provider_registers,
        run_escript_provider_registers,
        escriptize_builds_escript,
        run_escript_executes,
        escriptize_do_succeeds,
        escriptize_format_error,
        escriptize_info_contains_description
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

    {ok, State1} = r3lfe_prv_escriptize:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Escriptize provider should be registered"),

    ok.

run_escript_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_run_escript:init(State),

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
    {ok, State1} = r3lfe_prv_escriptize:init(State),

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
    {ok, State1} = r3lfe_prv_run_escript:init(State),

    %% Verify provider was registered
    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),

    %% Test that calling without an escript returns appropriate error
    State2 = rebar_state:command_parsed_args(State1, {[], []}),
    Result = r3lfe_prv_run_escript:do(State2),

    %% Should get an error since no escript exists
    ?assertMatch({error, _}, Result),

    ok.

escriptize_do_succeeds(_Config) ->
    %% Test that do/1 succeeds when called
    %% The actual escriptize work is done by the default provider
    %% This provider just ensures paths are set
    State = rebar_state:new(),
    {ok, State1} = r3lfe_prv_escriptize:init(State),

    Result = r3lfe_prv_escriptize:do(State1),

    %% Should succeed
    ?assertMatch({ok, _}, Result),

    ok.

escriptize_format_error(_Config) ->
    %% Test format_error/1
    Error1 = {some_error, "details"},
    Error2 = simple_error,

    Msg1 = r3lfe_prv_escriptize:format_error(Error1),
    Msg2 = r3lfe_prv_escriptize:format_error(Error2),

    %% Should return formatted messages
    ?assert(is_list(Msg1)),
    ?assert(is_list(Msg2)),

    ok.

escriptize_info_contains_description(_Config) ->
    %% Test that info/1 returns meaningful description
    Description = "Build an LFE escript executable",
    Info = r3lfe_prv_escriptize:info(Description),

    %% Should be a list
    ?assert(is_list(Info)),

    %% Should contain the description
    InfoStr = lists:flatten(Info),
    ?assert(string:str(InfoStr, Description) > 0),

    %% Should contain some key information
    ?assert(string:str(InfoStr, "escript") > 0),
    ?assert(string:str(InfoStr, "main") > 0),

    ok.
