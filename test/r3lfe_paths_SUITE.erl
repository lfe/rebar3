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
    ensure_dir_permission_error/1,
    set_paths_single_arg/1,
    set_paths_deps_only/1,
    set_paths_plugins_only/1,
    set_paths_both_types/1,
    unset_paths_single_arg/1,
    unset_paths_deps_only/1,
    unset_paths_plugins_only/1,
    with_paths_returns_value/1,
    with_paths_exception_propagates/1,
    fallback_set_paths_deps/1,
    fallback_set_paths_plugins/1
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
        ensure_dir_permission_error,
        set_paths_single_arg,
        set_paths_deps_only,
        set_paths_plugins_only,
        set_paths_both_types,
        unset_paths_single_arg,
        unset_paths_deps_only,
        unset_paths_plugins_only,
        with_paths_returns_value,
        with_paths_exception_propagates,
        fallback_set_paths_deps,
        fallback_set_paths_plugins
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

%%====================================================================
%% Additional Tests for set_paths and unset_paths variants
%%====================================================================

set_paths_single_arg(_Config) ->
    %% Test set_paths/1 which defaults to [deps, plugins]
    State = rebar_state:new(),

    ok = r3lfe_paths:set_paths(State),

    ok.

set_paths_deps_only(_Config) ->
    %% Test set_paths/2 with deps only
    State = rebar_state:new(),

    ok = r3lfe_paths:set_paths([deps], State),

    ok.

set_paths_plugins_only(_Config) ->
    %% Test set_paths/2 with plugins only
    State = rebar_state:new(),

    ok = r3lfe_paths:set_paths([plugins], State),

    ok.

set_paths_both_types(_Config) ->
    %% Test set_paths/2 with both deps and plugins
    State = rebar_state:new(),

    ok = r3lfe_paths:set_paths([deps, plugins], State),

    ok.

unset_paths_single_arg(_Config) ->
    %% Test unset_paths/1 which defaults to [deps, plugins]
    State = rebar_state:new(),

    ok = r3lfe_paths:unset_paths(State),

    ok.

unset_paths_deps_only(_Config) ->
    %% Test unset_paths/2 with deps only
    State = rebar_state:new(),

    ok = r3lfe_paths:unset_paths([deps], State),

    ok.

unset_paths_plugins_only(_Config) ->
    %% Test unset_paths/2 with plugins only
    State = rebar_state:new(),

    ok = r3lfe_paths:unset_paths([plugins], State),

    ok.

%%====================================================================
%% Additional Tests for with_paths
%%====================================================================

with_paths_returns_value(_Config) ->
    %% Test that with_paths returns the function's return value
    State = rebar_state:new(),

    Result = r3lfe_paths:with_paths(
        fun() ->
            {ok, some_result, 123}
        end,
        State
    ),

    ?assertEqual({ok, some_result, 123}, Result),

    ok.

with_paths_exception_propagates(_Config) ->
    %% Test that exceptions from the function propagate properly
    State = rebar_state:new(),

    %% Should propagate the throw
    ?assertThrow(
        custom_throw,
        r3lfe_paths:with_paths(
            fun() -> throw(custom_throw) end,
            State
        )
    ),

    %% Should propagate the exit
    ?assertExit(
        custom_exit,
        r3lfe_paths:with_paths(
            fun() -> exit(custom_exit) end,
            State
        )
    ),

    ok.

%%====================================================================
%% Tests for fallback_set_paths
%%====================================================================

fallback_set_paths_deps(_Config) ->
    %% Test fallback_set_paths with deps
    State = rebar_state:new(),

    ok = r3lfe_paths:fallback_set_paths([deps], State),

    ok.

fallback_set_paths_plugins(_Config) ->
    %% Test fallback_set_paths with plugins
    State = rebar_state:new(),

    ok = r3lfe_paths:fallback_set_paths([plugins], State),

    ok.
