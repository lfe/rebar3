-module(r3lfe_dep_cache_SUITE).

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
    init_creates_cache_table/1,
    init_idempotent/1,
    get_cache_miss/1,
    get_cache_hit/1,
    get_stale_cache/1,
    get_uninitialized_cache/1,
    put_creates_entry/1,
    put_updates_timestamp/1,
    put_uninitialized_cache/1,
    invalidate_removes_entry/1,
    clear_removes_all_entries/1,
    cache_staleness_detection/1,
    concurrent_cache_access/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        init_creates_cache_table,
        init_idempotent,
        get_cache_miss,
        get_cache_hit,
        get_stale_cache,
        get_uninitialized_cache,
        put_creates_entry,
        put_updates_timestamp,
        put_uninitialized_cache,
        invalidate_removes_entry,
        clear_removes_all_entries,
        cache_staleness_detection,
        concurrent_cache_access
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clean up cache before each test
    try ets:delete(r3lfe_dep_cache) catch _:_ -> ok end,
    TestDir = test_utils:create_temp_dir(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    try ets:delete(r3lfe_dep_cache) catch _:_ -> ok end,
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

init_creates_cache_table(_Config) ->
    %% Cache should not exist initially
    ?assertEqual(undefined, ets:info(r3lfe_dep_cache)),

    %% Initialize
    ok = r3lfe_dep_cache:init(),

    %% Table should now exist
    ?assertNotEqual(undefined, ets:info(r3lfe_dep_cache)),

    %% Should be public, set, named_table
    TableInfo = ets:info(r3lfe_dep_cache),
    ?assertEqual(set, proplists:get_value(type, TableInfo)),
    ?assertEqual(r3lfe_dep_cache, proplists:get_value(name, TableInfo)),
    ok.

init_idempotent(_Config) ->
    %% First init
    ok = r3lfe_dep_cache:init(),
    TableId1 = ets:whereis(r3lfe_dep_cache),

    %% Second init should not recreate
    ok = r3lfe_dep_cache:init(),
    TableId2 = ets:whereis(r3lfe_dep_cache),

    ?assertEqual(TableId1, TableId2),
    ok.

get_cache_miss(_Config) ->
    r3lfe_dep_cache:init(),

    %% Try to get non-existent entry
    Result = r3lfe_dep_cache:get("/tmp/nonexistent.lfe"),

    ?assertEqual(error, Result),
    ok.

get_cache_hit(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_dep_cache:init(),

    %% Create a file
    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),

    %% Add to cache
    Deps = ["/tmp/dep1.lfe", "/tmp/dep2.lfe"],
    ok = r3lfe_dep_cache:put(SourceFile, Deps),

    %% Retrieve from cache
    Result = r3lfe_dep_cache:get(SourceFile),

    ?assertMatch({ok, Deps, _Timestamp}, Result),

    {ok, CachedDeps, Timestamp} = Result,
    ?assertEqual(Deps, CachedDeps),
    ?assert(is_tuple(Timestamp)), % calendar:datetime() tuple
    ok.

get_stale_cache(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_dep_cache:init(),

    %% Create a file
    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),

    %% Add to cache
    Deps = ["/tmp/dep1.lfe"],
    ok = r3lfe_dep_cache:put(SourceFile, Deps),

    %% Wait a bit
    timer:sleep(1100),

    %% Modify source file (makes cache stale)
    test_utils:write_file(SourceFile, "(defmodule test)\n(defun new () 'ok)\n"),

    %% Try to retrieve - should be stale
    Result = r3lfe_dep_cache:get(SourceFile),

    ?assertEqual(error, Result, "Cache should be stale after file modification"),
    ok.

get_uninitialized_cache(_Config) ->
    %% Don't initialize cache

    Result = r3lfe_dep_cache:get("/tmp/test.lfe"),

    ?assertEqual(error, Result),
    ok.

put_creates_entry(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_dep_cache:init(),

    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),

    Deps = ["/tmp/dep.lfe"],

    %% Should not exist yet
    ?assertEqual(error, r3lfe_dep_cache:get(SourceFile)),

    %% Add entry
    ok = r3lfe_dep_cache:put(SourceFile, Deps),

    %% Should now exist
    ?assertMatch({ok, _, _}, r3lfe_dep_cache:get(SourceFile)),
    ok.

put_updates_timestamp(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_dep_cache:init(),

    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),

    %% Put with old deps
    OldDeps = ["/tmp/old.lfe"],
    ok = r3lfe_dep_cache:put(SourceFile, OldDeps),

    {ok, _, Timestamp1} = r3lfe_dep_cache:get(SourceFile),

    %% Update file
    timer:sleep(1100),
    test_utils:write_file(SourceFile, "(defmodule test)\n%% modified\n"),

    %% Put with new deps
    NewDeps = ["/tmp/new.lfe"],
    ok = r3lfe_dep_cache:put(SourceFile, NewDeps),

    {ok, CachedDeps, Timestamp2} = r3lfe_dep_cache:get(SourceFile),

    %% Should have new deps and new timestamp
    ?assertEqual(NewDeps, CachedDeps),
    ?assert(Timestamp2 >= Timestamp1),
    ok.

put_uninitialized_cache(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Don't initialize
    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),

    %% Put should handle gracefully
    ok = r3lfe_dep_cache:put(SourceFile, ["/tmp/dep.lfe"]),
    ok.

invalidate_removes_entry(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_dep_cache:init(),

    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),

    %% Add entry
    ok = r3lfe_dep_cache:put(SourceFile, ["/tmp/dep.lfe"]),
    ?assertMatch({ok, _, _}, r3lfe_dep_cache:get(SourceFile)),

    %% Invalidate
    ok = r3lfe_dep_cache:invalidate(SourceFile),

    %% Should be gone
    ?assertEqual(error, r3lfe_dep_cache:get(SourceFile)),
    ok.

clear_removes_all_entries(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_dep_cache:init(),

    %% Add multiple entries
    Files = [
        filename:join(TestDir, "file1.lfe"),
        filename:join(TestDir, "file2.lfe"),
        filename:join(TestDir, "file3.lfe")
    ],

    lists:foreach(
        fun(File) ->
            test_utils:write_file(File, "(defmodule test)\n"),
            ok = r3lfe_dep_cache:put(File, ["/tmp/dep.lfe"])
        end,
        Files
    ),

    %% Verify all exist
    lists:foreach(
        fun(File) ->
            ?assertMatch({ok, _, _}, r3lfe_dep_cache:get(File))
        end,
        Files
    ),

    %% Clear all
    ok = r3lfe_dep_cache:clear(),

    %% All should be gone
    lists:foreach(
        fun(File) ->
            ?assertEqual(error, r3lfe_dep_cache:get(File))
        end,
        Files
    ),
    ok.

cache_staleness_detection(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_dep_cache:init(),

    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),

    %% Cache at time T1
    ok = r3lfe_dep_cache:put(SourceFile, ["/tmp/dep.lfe"]),

    %% Verify cache is fresh
    ?assertMatch({ok, _, _}, r3lfe_dep_cache:get(SourceFile)),

    %% Wait and modify file (time T2 > T1)
    timer:sleep(1100),
    test_utils:write_file(SourceFile, "(defmodule test)\n%% comment\n"),

    %% Cache should now be stale
    Result = r3lfe_dep_cache:get(SourceFile),
    ?assertEqual(error, Result, "Cache should detect file modification"),
    ok.

concurrent_cache_access(_Config) ->
    r3lfe_dep_cache:init(),

    %% Multiple processes accessing cache concurrently
    Parent = self(),

    NumProcesses = 10,
    Pids = [
        spawn(fun() ->
            File = "/tmp/test" ++ integer_to_list(N) ++ ".lfe",
            Deps = ["/tmp/dep" ++ integer_to_list(N) ++ ".lfe"],

            %% Write
            ok = r3lfe_dep_cache:put(File, Deps),

            %% Read back immediately (from same process)
            %% Note: We can't guarantee order in true concurrency,
            %% but we can verify no crashes
            _ = r3lfe_dep_cache:get(File),

            Parent ! {self(), ok}
        end)
        || N <- lists:seq(1, NumProcesses)
    ],

    %% Collect results
    Results = [receive {Pid, Result} -> Result end || Pid <- Pids],

    %% All should succeed
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    ok.
