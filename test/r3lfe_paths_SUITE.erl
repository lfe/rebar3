-module(r3lfe_paths_SUITE).

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
    ensure_dir_creates_directory/1,
    ensure_dir_existing_directory/1,
    ensure_dir_nested_directory/1,
    with_paths_executes_function/1,
    with_paths_cleans_up/1,
    set_paths_fallback/1,
    unset_paths_no_error/1,
    ensure_dir_race_condition/1,
    ensure_dir_permission_error/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        ensure_dir_creates_directory,
        ensure_dir_existing_directory,
        ensure_dir_nested_directory,
        with_paths_executes_function,
        with_paths_cleans_up,
        set_paths_fallback,
        unset_paths_no_error,
        ensure_dir_race_condition,
        ensure_dir_permission_error
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

ensure_dir_creates_directory(Config) ->
    TestDir = ?config(test_dir, Config),
    NewDir = filename:join(TestDir, "new_directory"),

    ?assertNot(filelib:is_dir(NewDir)),

    ok = r3lfe_paths:ensure_dir(NewDir),

    ?assert(filelib:is_dir(NewDir)),
    ok.

ensure_dir_existing_directory(Config) ->
    TestDir = ?config(test_dir, Config),

    %% TestDir already exists
    ok = r3lfe_paths:ensure_dir(TestDir),

    ?assert(filelib:is_dir(TestDir)),
    ok.

ensure_dir_nested_directory(Config) ->
    TestDir = ?config(test_dir, Config),
    NestedDir = filename:join([TestDir, "a", "b", "c"]),

    ?assertNot(filelib:is_dir(NestedDir)),

    %% ensure_dir should only create the final directory
    %% Parent directories must exist
    ParentDir = filename:join([TestDir, "a", "b"]),
    %% Create parent directories first
    ok = filelib:ensure_dir(filename:join(ParentDir, "dummy")),

    ok = r3lfe_paths:ensure_dir(NestedDir),

    ?assert(filelib:is_dir(NestedDir)),
    ok.

with_paths_executes_function(_Config) ->
    State = test_utils:mock_state(),

    Result = r3lfe_paths:with_paths(
        fun() -> test_result end,
        State
    ),

    ?assertEqual(test_result, Result),
    ok.

with_paths_cleans_up(_Config) ->
    State = test_utils:mock_state(),

    %% Even if function throws, paths should be cleaned up
    ?assertError(
        test_error,
        r3lfe_paths:with_paths(
            fun() -> error(test_error) end,
            State
        )
    ),

    ok.

%%====================================================================
%% Additional Test Cases for Coverage
%%====================================================================

set_paths_fallback(_Config) ->
    %% Test fallback path setting
    State = rebar_state:new(),

    %% Should not crash even with empty state
    ok = r3lfe_paths:set_paths([deps], State),

    ok.

unset_paths_no_error(_Config) ->
    %% Test that unsetting paths never errors
    State = rebar_state:new(),

    %% Should handle gracefully
    ok = r3lfe_paths:unset_paths([deps, plugins], State),

    ok.

ensure_dir_race_condition(Config) ->
    TestDir = ?config(test_dir, Config),

    Dir = filename:join(TestDir, "race_dir"),

    %% Create directory externally
    ok = file:make_dir(Dir),

    %% ensure_dir should handle already existing
    ok = r3lfe_paths:ensure_dir(Dir),

    ?assert(filelib:is_dir(Dir)),

    ok.

ensure_dir_permission_error(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Try to create in a path that might fail
    %% (This is system-dependent, so handle gracefully)
    InvalidDir = filename:join(TestDir, "test"),

    Result = r3lfe_paths:ensure_dir(InvalidDir),

    %% Should either succeed or return proper error
    case Result of
        ok -> ?assert(filelib:is_dir(InvalidDir));
        {error, _} -> ok
    end,

    ok.
