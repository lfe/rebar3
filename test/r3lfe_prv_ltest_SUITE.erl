-module(r3lfe_prv_ltest_SUITE).

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
    ltest_provider_registers/1,
    ltest_requires_ltest_dependency/1,
    build_test_opts_default/1,
    build_test_opts_with_listener/1,
    build_test_opts_eunit_listener/1,
    build_test_opts_unknown_listener/1,
    build_test_opts_with_suite/1,
    build_test_opts_with_test/1,
    build_test_opts_with_verbose/1,
    add_test_paths_adds_test_dir/1,
    add_test_paths_no_test_dir/1,
    add_test_paths_multiple_apps/1,
    ltest_format_error_messages/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        ltest_provider_registers,
        ltest_requires_ltest_dependency,
        build_test_opts_default,
        build_test_opts_with_listener,
        build_test_opts_eunit_listener,
        build_test_opts_unknown_listener,
        build_test_opts_with_suite,
        build_test_opts_with_test,
        build_test_opts_with_verbose,
        add_test_paths_adds_test_dir,
        add_test_paths_no_test_dir,
        add_test_paths_multiple_apps,
        ltest_format_error_messages
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("ltest"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

ltest_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_ltest:init(State),

    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),
    ok.

ltest_requires_ltest_dependency(_Config) ->
    %% Test that running without ltest shows error

    %% Unload ltest if loaded
    case code:is_loaded(ltest) of
        {file, _} -> code:purge(ltest), code:delete(ltest);
        false -> ok
    end,

    State = rebar_state:new(),
    {ok, State1} = r3lfe_prv_ltest:init(State),

    Result = r3lfe_prv_ltest:do(State1),

    %% Should fail if ltest not available
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            %% ltest is available, can't test error case
            ct:pal("ltest is available, skipping error test");
        {error, _} ->
            %% Should get error about missing ltest
            ?assertMatch({error, _}, Result)
    end,
    ok.

build_test_opts_default(_Config) ->
    %% Test build_test_opts with default options
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            Opts = [],
            Result = r3lfe_prv_ltest:build_test_opts(Opts),
            ?assert(is_map(Result)),
            %% Should contain default options from ltest
            ok;
        {error, _} ->
            ct:pal("ltest not available, skipping test")
    end.

build_test_opts_with_listener(_Config) ->
    %% Test build_test_opts with ltest listener
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            Opts = [{listener, ltest}],
            Result = r3lfe_prv_ltest:build_test_opts(Opts),
            ?assert(is_map(Result)),
            %% Should have test-listener set to ltest-listener
            ?assertEqual('ltest-listener', maps:get('test-listener', Result)),
            ok;
        {error, _} ->
            ct:pal("ltest not available, skipping test")
    end.

build_test_opts_eunit_listener(_Config) ->
    %% Test build_test_opts with eunit listener
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            Opts = [{listener, eunit}],
            Result = r3lfe_prv_ltest:build_test_opts(Opts),
            ?assert(is_map(Result)),
            %% Should have test-listener set to eunit_surefire
            ?assertEqual(eunit_surefire, maps:get('test-listener', Result)),
            ok;
        {error, _} ->
            ct:pal("ltest not available, skipping test")
    end.

build_test_opts_unknown_listener(_Config) ->
    %% Test build_test_opts with unknown listener type
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            Opts = [{listener, unknown_listener}],
            Result = r3lfe_prv_ltest:build_test_opts(Opts),
            %% Should still return a map and use default
            ?assert(is_map(Result)),
            ok;
        {error, _} ->
            ct:pal("ltest not available, skipping test")
    end.

build_test_opts_with_suite(_Config) ->
    %% Test build_test_opts with suite option
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            Opts = [{suite, "my_test_suite"}],
            Result = r3lfe_prv_ltest:build_test_opts(Opts),
            ?assert(is_map(Result)),
            ?assertEqual("my_test_suite", maps:get(suite, Result)),
            ok;
        {error, _} ->
            ct:pal("ltest not available, skipping test")
    end.

build_test_opts_with_test(_Config) ->
    %% Test build_test_opts with specific test option
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            Opts = [{test, "my_specific_test"}],
            Result = r3lfe_prv_ltest:build_test_opts(Opts),
            ?assert(is_map(Result)),
            ?assertEqual("my_specific_test", maps:get(test, Result)),
            ok;
        {error, _} ->
            ct:pal("ltest not available, skipping test")
    end.

build_test_opts_with_verbose(_Config) ->
    %% Test build_test_opts with verbose option
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            Opts = [{verbose, true}],
            Result = r3lfe_prv_ltest:build_test_opts(Opts),
            ?assert(is_map(Result)),
            ?assertEqual(true, maps:get(verbose, Result)),
            ok;
        {error, _} ->
            ct:pal("ltest not available, skipping test")
    end.

add_test_paths_adds_test_dir(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create test directory
    TestSubDir = filename:join(TestDir, "test"),
    ok = filelib:ensure_dir(filename:join(TestSubDir, "dummy")),
    ok = file:write_file(filename:join(TestSubDir, "dummy"), <<>>),

    %% Create app
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    %% Get current code path before
    PathsBefore = code:get_path(),

    %% Call add_test_paths
    ok = r3lfe_prv_ltest:add_test_paths(State1),

    %% Verify test path was added
    PathsAfter = code:get_path(),
    ?assert(length(PathsAfter) >= length(PathsBefore)),
    ?assert(lists:member(TestSubDir, PathsAfter)),

    ok.

add_test_paths_no_test_dir(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create app WITHOUT test directory
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    %% Should not fail even if test dir doesn't exist
    ok = r3lfe_prv_ltest:add_test_paths(State1),

    ok.

add_test_paths_multiple_apps(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create first app with test dir
    App1Dir = filename:join(TestDir, "app1"),
    TestDir1 = filename:join(App1Dir, "test"),
    ok = filelib:ensure_dir(filename:join(TestDir1, "dummy")),
    ok = file:write_file(filename:join(TestDir1, "dummy"), <<>>),

    %% Create second app with test dir
    App2Dir = filename:join(TestDir, "app2"),
    TestDir2 = filename:join(App2Dir, "test"),
    ok = filelib:ensure_dir(filename:join(TestDir2, "dummy")),
    ok = file:write_file(filename:join(TestDir2, "dummy"), <<>>),

    %% Create apps
    {ok, AppInfo1} = rebar_app_info:new(app1, "0.1.0", App1Dir),
    {ok, AppInfo2} = rebar_app_info:new(app2, "0.1.0", App2Dir),

    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo1, AppInfo2]),

    %% Call add_test_paths
    ok = r3lfe_prv_ltest:add_test_paths(State1),

    %% Verify both test paths were added
    PathsAfter = code:get_path(),
    ?assert(lists:member(TestDir1, PathsAfter)),
    ?assert(lists:member(TestDir2, PathsAfter)),

    ok.

ltest_format_error_messages(_Config) ->
    %% Test error message formatting
    Error1 = {test_failed, "module_test"},
    Error2 = {suite_not_found, "my_suite"},
    Error3 = unknown_error,

    Msg1 = r3lfe_prv_ltest:format_error(Error1),
    Msg2 = r3lfe_prv_ltest:format_error(Error2),
    Msg3 = r3lfe_prv_ltest:format_error(Error3),

    ?assert(is_list(Msg1)),
    ?assert(is_list(Msg2)),
    ?assert(is_list(Msg3)),

    ok.
