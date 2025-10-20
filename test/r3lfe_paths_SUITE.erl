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
    with_paths_cleans_up/1
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
        with_paths_cleans_up
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
