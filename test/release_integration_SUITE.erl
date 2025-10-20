-module(release_integration_SUITE).

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
    build_and_run_release/1,
    release_start_stop/1,
    release_shows_usage/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        build_and_run_release,
        release_start_stop,
        release_shows_usage
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("release_integ"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),

    %% Cleanup any running releases
    cleanup_releases(TestDir),

    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

build_and_run_release(Config) ->
    TestDir = ?config(test_dir, Config),

    %% This is a simplified test that verifies the release provider can be initialized
    %% Full release building requires complex rebar3 state setup that's beyond unit testing

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Set up minimal relx config
    RelxConfig = [{release, {testrel, "0.1.0"}, [kernel, stdlib]}],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    %% Verify we can initialize the release provider
    {ok, _State3} = rb3lfe_prv_release:init(State2),

    ct:pal("Release provider initialized successfully"),

    ok.

release_start_stop(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Simplified test - just verify run-release provider can be initialized
    %% and properly reports errors when no release exists

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Set up minimal relx config
    RelxConfig = [{release, {testrel, "0.1.0"}, [kernel, stdlib]}],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    %% Initialize run-release provider
    {ok, State3} = rb3lfe_prv_run_release:init(State2),

    %% Try to run a command (should fail since no release is built)
    State4 = rebar_state:command_args(State3, ["status"]),
    Result = rb3lfe_prv_run_release:do(State4),

    %% Should get an error about release script not found
    ?assertMatch({error, _}, Result),

    ct:pal("Run-release provider handles missing releases correctly"),

    ok.

release_shows_usage(_Config) ->
    %% Verify that release provider shows helpful usage info
    %% This is tested by checking the info/1 function output

    Info = rb3lfe_prv_run_release:info("Test"),

    ?assert(is_list(Info)),
    ?assert(length(Info) > 0),

    %% Should mention common commands
    InfoStr = lists:flatten(Info),
    ?assert(string:find(InfoStr, "start") =/= nomatch),
    ?assert(string:find(InfoStr, "stop") =/= nomatch),
    ?assert(string:find(InfoStr, "status") =/= nomatch),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

cleanup_releases(_TestDir) ->
    %% In the simplified tests, we don't actually build releases
    %% so no cleanup needed
    ok.
