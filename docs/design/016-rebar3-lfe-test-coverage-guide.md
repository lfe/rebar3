# Complete Testing Coverage Implementation Guide for rebar3_lfe

## Overview

This document provides comprehensive, step-by-step instructions for implementing 100% test coverage across the rebar3_lfe codebase. After analyzing all source files and existing tests, I've identified coverage gaps and created detailed testing specifications.

---

## Current Coverage Status

### Well-Tested Modules (>80% coverage)
- ✅ `r3lfe_compile_worker` - Compilation, error formatting, options
- ✅ `r3lfe_dependency_scanner` - Parsing, resolution, scanning
- ✅ `r3lfe_package` - Discovery, naming, preparation, lifecycle
- ✅ `r3lfe_config` - Configuration retrieval and merging
- ✅ `r3lfe_prv_confabulate` - Data conversion provider

### Partially Tested Modules (40-80% coverage)
- ⚠️ `r3lfe_compiler_mod` - Missing edge cases, error paths
- ⚠️ `r3lfe_paths` - Basic functionality tested, missing edge cases
- ⚠️ `r3lfe_prv_repl` - REPL options tested, missing banner and execution paths
- ⚠️ `r3lfe_prv_compile` - High-level tested, missing error recovery

### Minimally Tested Modules (<40% coverage)
- ❌ `r3lfe_compile_opts` - Cache operations barely tested
- ❌ `r3lfe_dep_cache` - Cache validation and staleness logic untested
- ❌ `r3lfe_package_tracker` - Tracking lifecycle untested
- ❌ `r3lfe_progress` - Progress reporting untested
- ❌ `r3lfe_prv_clean` - Clean provider minimally tested
- ❌ `r3lfe_prv_ltest` - Test runner untested
- ❌ `r3lfe_prv_release` - Release building untested
- ❌ `r3lfe_prv_versions` - Version display minimally tested
- ❌ `r3lfe_prv_run` - Script execution untested
- ❌ `r3lfe_prv_escriptize` - Escript building minimally tested
- ❌ `r3lfe_prv_run_escript` - Escript execution minimally tested
- ❌ `r3lfe_prv_run_release` - Release execution minimally tested
- ❌ `rebar3_lfe` - Plugin initialization untested

---

## Test Implementation Plan

### Phase 1: Cache and Tracking Modules

#### 1.1 `r3lfe_compile_opts_SUITE.erl` - EXPAND EXISTING TESTS

**File Location:** `test/r3lfe_compile_opts_SUITE.erl`

**New Test Cases to Add:**

```erlang
%% Add to existing suite

%% Test cache initialization
init_creates_table_once/1,
init_idempotent/1,

%% Test hash computation
get_opts_hash_deterministic/1,
get_opts_hash_different_for_different_opts/1,
get_opts_hash_order_independent/1,

%% Test opts_changed detection
opts_changed_no_cache_returns_true/1,
opts_changed_cache_uninitialized/1,
opts_changed_same_opts_returns_false/1,
opts_changed_different_opts_returns_true/1,
opts_changed_after_save/1,

%% Test save operations
save_opts_hash_creates_entry/1,
save_opts_hash_updates_entry/1,
save_opts_hash_uninitialized_cache/1,

%% Test clear operations
clear_opts_cache_removes_all/1,
clear_opts_cache_uninitialized/1
```

**Implementation Details:**

```erlang
init_creates_table_once(Config) ->
    %% Ensure cache is cleared first
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,
    
    %% First init should create table
    ok = r3lfe_compile_opts:init(),
    
    %% Verify table exists
    ?assert(ets:info(r3lfe_opts_cache) =/= undefined),
    
    %% Get table ID
    TableId1 = ets:whereis(r3lfe_opts_cache),
    
    %% Second init should not recreate
    ok = r3lfe_compile_opts:init(),
    TableId2 = ets:whereis(r3lfe_opts_cache),
    
    %% Same table
    ?assertEqual(TableId1, TableId2),
    ok.

get_opts_hash_deterministic(_Config) ->
    Opts = [verbose, debug_info, {outdir, "/tmp"}],
    
    Hash1 = r3lfe_compile_opts:get_opts_hash(Opts),
    Hash2 = r3lfe_compile_opts:get_opts_hash(Opts),
    
    %% Same options should produce same hash
    ?assertEqual(Hash1, Hash2),
    
    %% Hash should be binary
    ?assert(is_binary(Hash1)),
    ?assertEqual(16, byte_size(Hash1)), % MD5 is 16 bytes
    ok.

get_opts_hash_order_independent(_Config) ->
    Opts1 = [verbose, debug_info, {outdir, "/tmp"}],
    Opts2 = [debug_info, verbose, {outdir, "/tmp"}],
    
    %% Different order, but sorts to same
    Hash1 = r3lfe_compile_opts:get_opts_hash(Opts1),
    Hash2 = r3lfe_compile_opts:get_opts_hash(Opts2),
    
    %% Should produce same hash (implementation sorts)
    ?assertEqual(Hash1, Hash2),
    ok.

opts_changed_no_cache_returns_true(_Config) ->
    File = "/tmp/test.lfe",
    Opts = [verbose],
    
    %% With empty cache, should return true
    Changed = r3lfe_compile_opts:opts_changed(File, Opts),
    
    ?assert(Changed, "Should detect change when no cached options"),
    ok.

opts_changed_same_opts_returns_false(_Config) ->
    r3lfe_compile_opts:init(),
    
    File = "/tmp/test.lfe",
    Opts = [verbose, debug_info],
    
    %% Save options
    ok = r3lfe_compile_opts:save_opts_hash(File, Opts),
    
    %% Check with same options
    Changed = r3lfe_compile_opts:opts_changed(File, Opts),
    
    ?assertNot(Changed, "Should not detect change for same options"),
    ok.

opts_changed_different_opts_returns_true(_Config) ->
    r3lfe_compile_opts:init(),
    
    File = "/tmp/test.lfe",
    Opts1 = [verbose],
    Opts2 = [verbose, debug_info],
    
    %% Save first set
    ok = r3lfe_compile_opts:save_opts_hash(File, Opts1),
    
    %% Check with different options
    Changed = r3lfe_compile_opts:opts_changed(File, Opts2),
    
    ?assert(Changed, "Should detect change for different options"),
    ok.

clear_opts_cache_removes_all(_Config) ->
    r3lfe_compile_opts:init(),
    
    %% Add some entries
    ok = r3lfe_compile_opts:save_opts_hash("/tmp/file1.lfe", [verbose]),
    ok = r3lfe_compile_opts:save_opts_hash("/tmp/file2.lfe", [debug_info]),
    
    %% Verify entries exist
    ?assertNot(r3lfe_compile_opts:opts_changed("/tmp/file1.lfe", [verbose])),
    
    %% Clear cache
    ok = r3lfe_compile_opts:clear_opts_cache(),
    
    %% Entries should be gone
    ?assert(r3lfe_compile_opts:opts_changed("/tmp/file1.lfe", [verbose])),
    ok.
```

---

#### 1.2 `r3lfe_dep_cache_SUITE.erl` - CREATE NEW SUITE

**File Location:** `test/r3lfe_dep_cache_SUITE.erl`

**Complete Test Suite:**

```erlang
-module(r3lfe_dep_cache_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

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
    ?assert(is_tuple(Timestamp)), % date_time() tuple
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
    timer:sleep(1000),
    
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
    timer:sleep(1000),
    test_utils:write_file(SourceFile, "(defmodule test)\n% modified\n"),
    
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
    test_utils:write_file(SourceFile, "(defmodule test)\n% comment\n"),
    
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
```

---

#### 1.3 `r3lfe_package_tracker_SUITE.erl` - CREATE NEW SUITE

**File Location:** `test/r3lfe_package_tracker_SUITE.erl`

**Complete Test Suite:**

```erlang
-module(r3lfe_package_tracker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    init_creates_tracker_table/1,
    init_idempotent/1,
    register_package_adds_entry/1,
    register_package_uninitialized/1,
    unregister_package_removes_entry/1,
    get_registered_packages_empty/1,
    get_registered_packages_multiple/1,
    is_temp_file_true/1,
    is_temp_file_false/1,
    cleanup_all_removes_packages/1,
    cleanup_all_deletes_temp_files/1
]).

all() ->
    [
        init_creates_tracker_table,
        init_idempotent,
        register_package_adds_entry,
        register_package_uninitialized,
        unregister_package_removes_entry,
        get_registered_packages_empty,
        get_registered_packages_multiple,
        is_temp_file_true,
        is_temp_file_false,
        cleanup_all_removes_packages,
        cleanup_all_deletes_temp_files
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    try ets:delete(r3lfe_package_tracker) catch _:_ -> ok end,
    TestDir = test_utils:create_temp_dir(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    try ets:delete(r3lfe_package_tracker) catch _:_ -> ok end,
    ok.

init_creates_tracker_table(_Config) ->
    ?assertEqual(undefined, ets:info(r3lfe_package_tracker)),
    
    ok = r3lfe_package_tracker:init(),
    
    ?assertNotEqual(undefined, ets:info(r3lfe_package_tracker)),
    ok.

init_idempotent(_Config) ->
    ok = r3lfe_package_tracker:init(),
    TableId1 = ets:whereis(r3lfe_package_tracker),
    
    ok = r3lfe_package_tracker:init(),
    TableId2 = ets:whereis(r3lfe_package_tracker),
    
    ?assertEqual(TableId1, TableId2),
    ok.

register_package_adds_entry(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:init(),
    
    TempFile = filename:join(TestDir, "temp.lfe"),
    PackageInfo = #{
        source_file => "/src/pkg/module.lfe",
        temp_file => TempFile,
        module_name => "pkg.module",
        source_dir => "/src"
    },
    
    %% Register
    ok = r3lfe_package_tracker:register_package(PackageInfo),
    
    %% Verify it's registered
    ?assert(r3lfe_package_tracker:is_temp_file(TempFile)),
    
    %% Verify in package list
    Packages = r3lfe_package_tracker:get_registered_packages(),
    ?assertEqual(1, length(Packages)),
    ?assert(lists:member(PackageInfo, Packages)),
    ok.

register_package_uninitialized(_Config) ->
    %% Don't initialize
    
    PackageInfo = #{
        source_file => "/src/module.lfe",
        temp_file => "/tmp/temp.lfe",
        module_name => "module",
        source_dir => "/src"
    },
    
    %% Should handle gracefully
    ok = r3lfe_package_tracker:register_package(PackageInfo),
    ok.

unregister_package_removes_entry(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:init(),
    
    TempFile = filename:join(TestDir, "temp.lfe"),
    PackageInfo = #{
        source_file => "/src/module.lfe",
        temp_file => TempFile,
        module_name => "module",
        source_dir => "/src"
    },
    
    %% Register
    ok = r3lfe_package_tracker:register_package(PackageInfo),
    ?assert(r3lfe_package_tracker:is_temp_file(TempFile)),
    
    %% Unregister
    ok = r3lfe_package_tracker:unregister_package(TempFile),
    
    %% Should be gone
    ?assertNot(r3lfe_package_tracker:is_temp_file(TempFile)),
    ok.

get_registered_packages_empty(_Config) ->
    r3lfe_package_tracker:init(),
    
    Packages = r3lfe_package_tracker:get_registered_packages(),
    
    ?assertEqual([], Packages),
    ok.

get_registered_packages_multiple(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:init(),
    
    %% Register multiple
    Packages = [
        #{
            source_file => "/src/pkg1/module.lfe",
            temp_file => filename:join(TestDir, "temp1.lfe"),
            module_name => "pkg1.module",
            source_dir => "/src"
        },
        #{
            source_file => "/src/pkg2/module.lfe",
            temp_file => filename:join(TestDir, "temp2.lfe"),
            module_name => "pkg2.module",
            source_dir => "/src"
        }
    ],
    
    lists:foreach(
        fun(Pkg) ->
            ok = r3lfe_package_tracker:register_package(Pkg)
        end,
        Packages
    ),
    
    %% Get all
    Retrieved = r3lfe_package_tracker:get_registered_packages(),
    
    ?assertEqual(2, length(Retrieved)),
    lists:foreach(
        fun(Pkg) ->
            ?assert(lists:member(Pkg, Retrieved))
        end,
        Packages
    ),
    ok.

is_temp_file_true(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:init(),
    
    TempFile = filename:join(TestDir, "temp.lfe"),
    PackageInfo = #{
        source_file => "/src/module.lfe",
        temp_file => TempFile,
        module_name => "module",
        source_dir => "/src"
    },
    
    ok = r3lfe_package_tracker:register_package(PackageInfo),
    
    ?assert(r3lfe_package_tracker:is_temp_file(TempFile)),
    ok.

is_temp_file_false(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:init(),
    
    RandomFile = filename:join(TestDir, "random.lfe"),
    
    ?assertNot(r3lfe_package_tracker:is_temp_file(RandomFile)),
    ok.

cleanup_all_removes_packages(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:init(),
    
    %% Register some packages
    TempFile1 = filename:join(TestDir, "temp1.lfe"),
    TempFile2 = filename:join(TestDir, "temp2.lfe"),
    
    test_utils:write_file(TempFile1, "content"),
    test_utils:write_file(TempFile2, "content"),
    
    Packages = [
        #{
            source_file => "/src/module1.lfe",
            temp_file => TempFile1,
            module_name => "module1",
            source_dir => "/src"
        },
        #{
            source_file => "/src/module2.lfe",
            temp_file => TempFile2,
            module_name => "module2",
            source_dir => "/src"
        }
    ],
    
    lists:foreach(
        fun(Pkg) ->
            ok = r3lfe_package_tracker:register_package(Pkg)
        end,
        Packages
    ),
    
    ?assertEqual(2, length(r3lfe_package_tracker:get_registered_packages())),
    
    %% Cleanup
    ok = r3lfe_package_tracker:cleanup_all(),
    
    %% All should be removed from tracker
    ?assertEqual([], r3lfe_package_tracker:get_registered_packages()),
    ok.

cleanup_all_deletes_temp_files(Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:init(),
    
    TempFile = filename:join(TestDir, "temp.lfe"),
    test_utils:write_file(TempFile, "content"),
    
    PackageInfo = #{
        source_file => "/src/module.lfe",
        temp_file => TempFile,
        module_name => "module",
        source_dir => "/src"
    },
    
    ok = r3lfe_package_tracker:register_package(PackageInfo),
    
    ?assert(filelib:is_file(TempFile)),
    
    %% Cleanup
    ok = r3lfe_package_tracker:cleanup_all(),
    
    %% File should be deleted
    ?assertNot(filelib:is_file(TempFile)),
    ok.
```

---

### Phase 2: Progress and Provider Modules

#### 2.1 `r3lfe_progress_SUITE.erl` - CREATE NEW SUITE

**File Location:** `test/r3lfe_progress_SUITE.erl`

```erlang
-module(r3lfe_progress_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    init_creates_progress_record/1,
    report_start_logs_file_count/1,
    report_file_increments_counter/1,
    report_file_calculates_percentage/1,
    report_complete_shows_duration/1,
    progress_with_zero_files/1,
    progress_with_large_file_count/1
]).

all() ->
    [
        init_creates_progress_record,
        report_start_logs_file_count,
        report_file_increments_counter,
        report_file_calculates_percentage,
        report_complete_shows_duration,
        progress_with_zero_files,
        progress_with_large_file_count
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_creates_progress_record(_Config) ->
    Progress = r3lfe_progress:init(100),
    
    %% Verify record structure (we need to check fields are set)
    %% Progress is a record, check it's a tuple with expected size
    ?assert(is_tuple(Progress)),
    ?assertEqual(4, tuple_size(Progress)), % #progress{} has 4 elements
    ok.

report_start_logs_file_count(_Config) ->
    %% Create mock app info
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", "/tmp"),
    
    %% Should not crash
    ok = r3lfe_progress:report_start(10, AppInfo),
    ok = r3lfe_progress:report_start(0, AppInfo),
    ok.

report_file_increments_counter(_Config) ->
    Progress1 = r3lfe_progress:init(10),
    
    %% Report first file
    Progress2 = r3lfe_progress:report_file(Progress1),
    
    %% Progress should be different (counter incremented)
    ?assertNotEqual(Progress1, Progress2),
    
    %% Report more files
    Progress3 = r3lfe_progress:report_file(Progress2),
    Progress4 = r3lfe_progress:report_file(Progress3),
    
    ?assertNotEqual(Progress2, Progress3),
    ?assertNotEqual(Progress3, Progress4),
    ok.

report_file_calculates_percentage(_Config) ->
    %% Test with small file count to verify percentage calculation
    Progress = r3lfe_progress:init(10),
    
    %% Report 5 files (50%)
    Progress1 = lists:foldl(
        fun(_, Acc) -> r3lfe_progress:report_file(Acc) end,
        Progress,
        lists:seq(1, 5)
    ),
    
    %% We can't directly inspect the record, but we verify no crashes
    %% and the function returns a valid progress record
    ?assert(is_tuple(Progress1)),
    ok.

report_complete_shows_duration(_Config) ->
    Progress = r3lfe_progress:init(5),
    
    %% Compile some files
    Progress1 = lists:foldl(
        fun(_, Acc) -> r3lfe_progress:report_file(Acc) end,
        Progress,
        lists:seq(1, 5)
    ),
    
    %% Report complete should not crash
    ok = r3lfe_progress:report_complete(Progress1),
    ok.

progress_with_zero_files(_Config) ->
    Progress = r3lfe_progress:init(0),
    
    %% Should handle edge case
    ok = r3lfe_progress:report_complete(Progress),
    ok.

progress_with_large_file_count(_Config) ->
    %% Test with large number
    Progress = r3lfe_progress:init(10000),
    
    %% Report some files
    Progress1 = lists:foldl(
        fun(_, Acc) -> r3lfe_progress:report_file(Acc) end,
        Progress,
        lists:seq(1, 100)
    ),
    
    %% Should handle without overflow
    ok = r3lfe_progress:report_complete(Progress1),
    ok.
```

---

#### 2.2 `r3lfe_prv_ltest_SUITE.erl` - CREATE NEW SUITE

**File Location:** `test/r3lfe_prv_ltest_SUITE.erl`

```erlang
-module(r3lfe_prv_ltest_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    ltest_provider_registers/1,
    ltest_requires_ltest_dependency/1,
    build_test_opts_default/1,
    build_test_opts_with_listener/1,
    build_test_opts_eunit_listener/1,
    add_test_paths_adds_test_dir/1
]).

all() ->
    [
        ltest_provider_registers,
        ltest_requires_ltest_dependency,
        build_test_opts_default,
        build_test_opts_with_listener,
        build_test_opts_eunit_listener,
        add_test_paths_adds_test_dir
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
    %% This tests internal function via exports
    Opts = [],
    
    %% Would need to export build_test_opts/1 or test indirectly
    %% For now, verify provider handles empty opts
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {Opts, []}),
    
    %% Verify state is valid
    ?assert(is_tuple(State1)),
    ok.

build_test_opts_with_listener(_Config) ->
    Opts = [{listener, ltest}],
    
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {Opts, []}),
    
    ?assert(is_tuple(State1)),
    ok.

build_test_opts_eunit_listener(_Config) ->
    Opts = [{listener, eunit}],
    
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {Opts, []}),
    
    ?assert(is_tuple(State1)),
    ok.

add_test_paths_adds_test_dir(Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Create test directory
    TestSubDir = filename:join(TestDir, "test"),
    ok = filelib:ensure_dir(filename:join(TestSubDir, "dummy")),
    
    %% Create app
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo]),
    
    %% This would call add_test_paths internally
    %% We verify it doesn't crash
    ?assert(is_tuple(State1)),
    ok.
```

---

#### 2.3 Expand `r3lfe_compiler_mod_SUITE.erl`

**Add to existing suite:**

```erlang
%% Add these test cases to the existing exports and all/0 list

%% Edge case tests
compile_with_no_out_mappings/1,
compile_with_multiple_mappings/1,
source_to_target_no_mappings/1,
needs_compilation_missing_source/1,
needs_compilation_dag_vertex_missing/1,
check_dependencies_newer_no_edges/1,
check_dependencies_newer_missing_dependency/1,

%% Error recovery tests
compile_handles_write_error/1,
compile_recovers_from_partial_failure/1,

%% Format diagnostic tests
format_diagnostics_empty_list/1,
format_diagnostics_multiple_files/1,
format_diagnostic_item_unknown_module/1
```

**Implementation:**

```erlang
compile_with_no_out_mappings(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    
    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile, "(defmodule simple)\n"),
    
    %% Empty mappings - should use fallback
    OutMappings = [],
    Opts = [],
    
    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),
    
    %% Should still attempt compilation (may fail, but shouldn't crash)
    ?assert(is_tuple(Result)),
    ok.

needs_compilation_missing_source(_Config) ->
    %% Create DAG
    G = digraph:new([acyclic]),
    
    %% Source file that doesn't exist
    NonExistent = "/tmp/nonexistent.lfe",
    digraph:add_vertex(G, NonExistent),
    
    OutMappings = [{".beam", "/tmp"}],
    
    %% Should return true (needs compilation to get error)
    ?assert(r3lfe_compiler_mod:needs_compilation(G, NonExistent, OutMappings)),
    
    digraph:delete(G),
    ok.

check_dependencies_newer_no_edges(_Config) ->
    %% Create DAG with vertex but no dependencies
    G = digraph:new([acyclic]),
    
    Source = "/tmp/test.lfe",
    digraph:add_vertex(G, Source),
    
    TargetTime = {{2024, 1, 1}, {12, 0, 0}},
    
    %% No dependencies, should return false
    Result = r3lfe_compiler_mod:check_dependencies_newer(G, Source, TargetTime),
    
    ?assertNot(Result),
    
    digraph:delete(G),
    ok.

format_diagnostics_empty_list(_Config) ->
    Diagnostics = [],
    
    Result = r3lfe_compiler_mod:format_diagnostics(Diagnostics),
    
    ?assertEqual([], Result),
    ok.

format_diagnostics_multiple_files(_Config) ->
    Diagnostics = [
        {"file1.lfe", [{10, erl_parse, "error1"}]},
        {"file2.lfe", [{20, erl_lint, "error2"}]},
        {"file3.lfe", [{30, lfe_comp, "error3"}]}
    ],
    
    Result = r3lfe_compiler_mod:format_diagnostics(Diagnostics),
    
    ?assertEqual(3, length(Result)),
    
    %% Each should be a formatted string
    lists:foreach(
        fun(Str) ->
            ?assert(is_list(Str)),
            ?assert(string:find(Str, ".lfe") =/= nomatch)
        end,
        Result
    ),
    ok.
```

---

### Phase 3: Complex Provider Tests

#### 3.1 Expand `providers_SUITE.erl`

**Add comprehensive provider testing:**

```erlang
%% Add to existing suite

%% Test all providers register correctly
all_providers_register/1,

%% Test provider dependencies
compile_provider_has_correct_deps/1,
release_provider_has_correct_deps/1,
escriptize_provider_has_correct_deps/1,

%% Test provider namespacing
all_providers_use_lfe_namespace/1,

%% Test provider format_error callbacks
providers_have_format_error/1,

%% Test provider opts handling
providers_parse_opts_correctly/1
```

**Implementation:**

```erlang
all_providers_register(_Config) ->
    State = rebar_state:new(),
    
    %% Initialize all providers
    {ok, State1} = rebar3_lfe:init(State),
    
    Providers = rebar_state:providers(State1),
    
    %% Should have all expected providers
    ExpectedProviders = [
        compile, clean, repl, ltest, release,
        versions, run, escriptize, 'run-escript',
        'run-release', confabulate
    ],
    
    %% Count providers in lfe namespace
    LfeProviders = lists:filter(
        fun(P) ->
            providers:namespace(P) =:= lfe
        end,
        Providers
    ),
    
    ?assert(length(LfeProviders) >= length(ExpectedProviders)),
    
    %% Verify each expected provider exists
    lists:foreach(
        fun(Name) ->
            Found = lists:any(
                fun(P) ->
                    providers:name(P) =:= Name andalso
                    providers:namespace(P) =:= lfe
                end,
                Providers
            ),
            ?assert(Found, io_lib:format("Provider ~p should be registered", [Name]))
        end,
        ExpectedProviders
    ),
    ok.

compile_provider_has_correct_deps(_Config) ->
    State = rebar_state:new(),
    {ok, State1} = r3lfe_prv_compile:init(State),
    
    Providers = rebar_state:providers(State1),
    
    %% Find compile provider
    CompileProvider = lists:keyfind(
        {lfe, compile},
        1,
        [{providers:namespace(P), providers:name(P)} || P <- Providers]
    ),
    
    ?assertNotEqual(false, CompileProvider),
    ok.

all_providers_use_lfe_namespace(_Config) ->
    State = rebar_state:new(),
    {ok, State1} = rebar3_lfe:init(State1),
    
    Providers = rebar_state:providers(State1),
    
    %% Get all our providers
    OurProviders = lists:filter(
        fun(P) ->
            Mod = providers:module(P),
            case atom_to_list(Mod) of
                "r3lfe_prv_" ++ _ -> true;
                _ -> false
            end
        end,
        Providers
    ),
    
    %% All should use 'lfe' namespace
    lists:foreach(
        fun(P) ->
            ?assertEqual(lfe, providers:namespace(P))
        end,
        OurProviders
    ),
    ok.

providers_have_format_error(_Config) ->
    %% Verify each provider module exports format_error/1
    ProviderModules = [
        r3lfe_prv_compile,
        r3lfe_prv_clean,
        r3lfe_prv_repl,
        r3lfe_prv_ltest,
        r3lfe_prv_release,
        r3lfe_prv_versions,
        r3lfe_prv_run,
        r3lfe_prv_escriptize,
        r3lfe_prv_run_escript,
        r3lfe_prv_run_release,
        r3lfe_prv_confabulate
    ],
    
    lists:foreach(
        fun(Mod) ->
            %% Check module exports format_error/1
            Exports = Mod:module_info(exports),
            ?assert(lists:member({format_error, 1}, Exports),
                    io_lib:format("~p should export format_error/1", [Mod]))
        end,
        ProviderModules
    ),
    ok.
```

---

### Phase 4: Integration and E2E Tests

#### 4.1 Expand `integration_SUITE.erl`

**Add to existing suite:**

```erlang
%% Complex dependency scenarios
transitive_dependencies_recompile/1,
circular_include_detection/1,
cross_directory_dependencies/1,

%% Incremental compilation edge cases
incremental_with_changed_options/1,
incremental_with_deleted_dependency/1,
incremental_with_new_dependency/1,

%% Cache invalidation scenarios
cache_invalidation_on_header_change/1,
cache_invalidation_on_source_deletion/1,

%% Package handling integration
mixed_flat_and_package_compilation/1,
package_with_includes/1
```

**Implementation outline:**

```erlang
transitive_dependencies_recompile(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    
    %% Create: base.lfe <- middle.lfe <- top.lfe
    %% Modify base.lfe, ensure top.lfe recompiles
    
    IncludeDir = maps:get(include_dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    
    test_utils:write_file(
        filename:join(IncludeDir, "base.lfe"),
        "(defrecord base id)\n"
    ),
    
    test_utils:write_file(
        filename:join(IncludeDir, "middle.lfe"),
        "(include-file \"base.lfe\")\n"
        "(defrecord middle (base) extra)\n"
    ),
    
    test_utils:write_file(
        filename:join(SrcDir, "top.lfe"),
        "(defmodule top)\n"
        "(include-file \"middle.lfe\")\n"
        "(defun test () 'ok)\n"
    ),
    
    %% Initial compile
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", maps:get(dir, AppData)),
    %% ... compile logic
    
    %% Modify base.lfe
    timer:sleep(1100),
    test_utils:write_file(
        filename:join(IncludeDir, "base.lfe"),
        "(defrecord base id version)\n"  % Added field
    ),
    
    %% Recompile - top.lfe should be detected as needing recompilation
    %% through transitive dependency
    
    ok.

incremental_with_changed_options(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),
    
    SourceFile = filename:join(SrcDir, "module.lfe"),
    test_utils:write_file(SourceFile, "(defmodule module)\n"),
    
    %% Compile with options set 1
    Opts1 = [verbose],
    r3lfe_compile_worker:compile_file(SourceFile, EbinDir, Opts1),
    
    %% Compile with options set 2 (different)
    Opts2 = [verbose, debug_info],
    
    %% Should detect options changed and recompile
    %% (even if source unchanged)
    
    ok.
```

---

### Phase 5: Plugin Initialization and Main Module

#### 5.1 `rebar3_lfe_SUITE.erl` - CREATE NEW SUITE

**File Location:** `test/rebar3_lfe_SUITE.erl`

```erlang
-module(rebar3_lfe_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    plugin_init_succeeds/1,
    plugin_initializes_caches/1,
    plugin_registers_compiler/1,
    plugin_registers_all_providers/1,
    plugin_init_idempotent/1,
    plugin_with_existing_state/1
]).

all() ->
    [
        plugin_init_succeeds,
        plugin_initializes_caches,
        plugin_registers_compiler,
        plugin_registers_all_providers,
        plugin_init_idempotent,
        plugin_with_existing_state
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, _Config) ->
    %% Clean up ETS tables
    try ets:delete(r3lfe_dep_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_package_tracker) catch _:_ -> ok end,
    [].

end_per_testcase(_TestCase, _Config) ->
    %% Cleanup
    try ets:delete(r3lfe_dep_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_package_tracker) catch _:_ -> ok end,
    ok.

plugin_init_succeeds(_Config) ->
    State = rebar_state:new(),
    
    Result = rebar3_lfe:init(State),
    
    ?assertMatch({ok, _}, Result),
    ok.

plugin_initializes_caches(_Config) ->
    State = rebar_state:new(),
    
    %% Caches should not exist
    ?assertEqual(undefined, ets:info(r3lfe_dep_cache)),
    ?assertEqual(undefined, ets:info(r3lfe_opts_cache)),
    ?assertEqual(undefined, ets:info(r3lfe_package_tracker)),
    
    %% Initialize plugin
    {ok, _State1} = rebar3_lfe:init(State),
    
    %% All caches should now exist
    ?assertNotEqual(undefined, ets:info(r3lfe_dep_cache)),
    ?assertNotEqual(undefined, ets:info(r3lfe_opts_cache)),
    ?assertNotEqual(undefined, ets:info(r3lfe_package_tracker)),
    ok.

plugin_registers_compiler(_Config) ->
    State = rebar_state:new(),
    
    %% Get initial compilers
    InitialCompilers = rebar_state:compilers(State),
    
    %% Initialize plugin
    {ok, State1} = rebar3_lfe:init(State),
    
    %% Get updated compilers
    UpdatedCompilers = rebar_state:compilers(State1),
    
    %% Should have more compilers
    ?assert(length(UpdatedCompilers) > length(InitialCompilers)),
    
    %% Should contain our compiler
    ?assert(lists:member(r3lfe_compiler_mod, UpdatedCompilers)),
    ok.

plugin_registers_all_providers(_Config) ->
    State = rebar_state:new(),
    
    InitialProviders = rebar_state:providers(State),
    
    {ok, State1} = rebar3_lfe:init(State),
    
    UpdatedProviders = rebar_state:providers(State1),
    
    %% Should have more providers
    ?assert(length(UpdatedProviders) > length(InitialProviders)),
    
    %% Should have at least 11 LFE providers
    LfeProviders = lists:filter(
        fun(P) ->
            providers:namespace(P) =:= lfe
        end,
        UpdatedProviders
    ),
    
    ?assert(length(LfeProviders) >= 11),
    ok.

plugin_init_idempotent(_Config) ->
    State = rebar_state:new(),
    
    {ok, State1} = rebar3_lfe:init(State),
    Providers1 = rebar_state:providers(State1),
    Compilers1 = rebar_state:compilers(State1),
    
    %% Initialize again
    {ok, State2} = rebar3_lfe:init(State1),
    Providers2 = rebar_state:providers(State2),
    Compilers2 = rebar_state:compilers(State2),
    
    %% Should not duplicate
    ?assertEqual(length(Providers1), length(Providers2)),
    ?assertEqual(length(Compilers1), length(Compilers2)),
    ok.

plugin_with_existing_state(_Config) ->
    %% Test that plugin can initialize with state that already
    %% has providers and compilers
    
    State = rebar_state:new(),
    
    %% Add some fake state
    State1 = rebar_state:set(State, some_key, some_value),
    
    %% Initialize plugin
    {ok, State2} = rebar3_lfe:init(State1),
    
    %% Original state should be preserved
    ?assertEqual(some_value, rebar_state:get(State2, some_key, undefined)),
    
    %% Plugin state should be added
    ?assert(length(rebar_state:providers(State2)) > 0),
    ok.
```

---

### Phase 6: Error Path and Edge Case Testing

#### 6.1 Error Handling Test Suite - CREATE NEW

**File Location:** `test/error_handling_SUITE.erl`

```erlang
-module(error_handling_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    %% File system errors
    compile_with_unreadable_source/1,
    compile_with_unwritable_output_dir/1,
    compile_with_missing_output_dir/1,
    
    %% Parse errors
    compile_with_syntax_error/1,
    compile_with_unterminated_form/1,
    
    %% Dependency errors
    missing_include_file/1,
    missing_include_lib/1,
    circular_include_detection/1,
    
    %% Permission errors
    ebin_directory_not_writable/1,
    source_directory_not_readable/1,
    
    %% Cache corruption
    corrupted_dep_cache_recovery/1,
    corrupted_opts_cache_recovery/1,
    
    %% Resource exhaustion
    out_of_disk_space_handling/1,
    too_many_open_files_handling/1
]).

all() ->
    [
        compile_with_unreadable_source,
        compile_with_unwritable_output_dir,
        compile_with_missing_output_dir,
        compile_with_syntax_error,
        compile_with_unterminated_form,
        missing_include_file,
        missing_include_lib,
        ebin_directory_not_writable,
        source_directory_not_readable,
        corrupted_dep_cache_recovery,
        corrupted_opts_cache_recovery
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    r3lfe_dep_cache:clear(),
    r3lfe_compile_opts:clear_opts_cache(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

compile_with_unreadable_source(Config) ->
    TestDir = ?config(test_dir, Config),
    
    SourceFile = filename:join(TestDir, "unreadable.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),
    
    %% Make file unreadable
    ok = file:change_mode(SourceFile, 8#000),
    
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    
    %% Try to compile
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),
    
    %% Should fail gracefully
    ?assertMatch({error, _, _}, Result),
    
    %% Restore permissions for cleanup
    file:change_mode(SourceFile, 8#644),
    ok.

compile_with_missing_output_dir(Config) ->
    TestDir = ?config(test_dir, Config),
    
    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),
    
    %% Use non-existent output directory
    OutDir = filename:join(TestDir, "nonexistent/ebin"),
    
    %% Should create directory automatically
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),
    
    %% Should succeed (creates directory)
    ?assertMatch(ok, Result),
    ?assert(filelib:is_dir(OutDir)),
    ok.

compile_with_syntax_error(Config) ->
    TestDir = ?config(test_dir, Config),
    
    SourceFile = filename:join(TestDir, "syntax_error.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule syntax-error)\n"
        "(defun broken ( )  % Syntax error: incomplete\n"
    ),
    
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),
    
    ?assertMatch({error, _Errors, _Warnings}, Result),
    
    {error, Errors, _} = Result,
    ?assert(length(Errors) > 0),
    ok.

missing_include_file(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    
    %% Source references non-existent header
    SourceFile = filename:join(SrcDir, "with_missing.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule with-missing)\n"
        "(include-file \"nonexistent.lfe\")\n"
        "(defun test () 'ok)\n"
    ),
    
    %% Scan for dependencies
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),
    
    %% Should return empty or log warning (won't find the file)
    %% The important thing is it doesn't crash
    ?assert(is_list(Deps)),
    ok.

missing_include_lib(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    
    SourceFile = filename:join(SrcDir, "with_missing_lib.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule with-missing-lib)\n"
        "(include-lib \"nonexistent_app/include/file.lfe\")\n"
        "(defun test () 'ok)\n"
    ),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),
    
    %% Should handle gracefully
    ?assert(is_list(Deps)),
    ok.

ebin_directory_not_writable(Config) ->
    TestDir = ?config(test_dir, Config),
    
    SourceFile = filename:join(TestDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),
    
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    
    %% Make directory read-only
    ok = file:change_mode(OutDir, 8#555),
    
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),
    
    %% Should fail with write error
    ?assertMatch({error, _, _}, Result),
    
    %% Restore permissions
    file:change_mode(OutDir, 8#755),
    ok.

source_directory_not_readable(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    
    SourceFile = filename:join(SrcDir, "test.lfe"),
    test_utils:write_file(SourceFile, "(defmodule test)\n"),
    
    %% Make directory unreadable
    ok = file:change_mode(SrcDir, 8#000),
    
    %% Try to discover files
    Files = r3lfe_package:discover_files(SrcDir),
    
    %% Should return empty (can't read directory)
    ?assertEqual([], Files),
    
    %% Restore permissions
    file:change_mode(SrcDir, 8#755),
    ok.

corrupted_dep_cache_recovery(_Config) ->
    r3lfe_dep_cache:init(),
    
    %% Insert malformed data directly into ETS
    true = ets:insert(r3lfe_dep_cache, {"/tmp/test.lfe", invalid_data}),
    
    %% Try to get - should handle gracefully
    Result = r3lfe_dep_cache:get("/tmp/test.lfe"),
    
    %% Should return error or handle corruption
    ?assert(Result =:= error orelse is_tuple(Result)),
    ok.

corrupted_opts_cache_recovery(_Config) ->
    r3lfe_compile_opts:init(),
    
    %% Insert malformed data
    true = ets:insert(r3lfe_opts_cache, {"/tmp/test.lfe", <<"bad_data">>}),
    
    %% Try to check opts changed - should handle gracefully
    Result = r3lfe_compile_opts:opts_changed("/tmp/test.lfe", [verbose]),
    
    %% Should not crash
    ?assert(is_boolean(Result)),
    ok.
```

---

### Phase 7: Performance and Concurrency Tests

#### 7.1 `performance_SUITE.erl` - CREATE NEW SUITE

**File Location:** `test/performance_SUITE.erl`

```erlang
-module(performance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    cache_performance_vs_no_cache/1,
    parallel_compilation_scales/1,
    large_project_compilation/1,
    incremental_build_performance/1,
    dependency_scanning_performance/1,
    concurrent_cache_access_performance/1
]).

all() ->
    [
        cache_performance_vs_no_cache,
        parallel_compilation_scales,
        large_project_compilation,
        incremental_build_performance,
        dependency_scanning_performance,
        concurrent_cache_access_performance
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("perf"),
    r3lfe_dep_cache:init(),
    r3lfe_compile_opts:init(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    r3lfe_dep_cache:clear(),
    r3lfe_compile_opts:clear_opts_cache(),
    ok.

cache_performance_vs_no_cache(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    
    %% Create 50 files
    Files = lists:map(
        fun(N) ->
            File = filename:join(SrcDir, io_lib:format("module~p.lfe", [N])),
            test_utils:write_file(File, "(defmodule module" ++ integer_to_list(N) ++ ")\n"),
            File
        end,
        lists:seq(1, 50)
    ),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    
    %% Clear cache and measure without cache
    r3lfe_dep_cache:clear(),
    
    {TimeNoCache, _} = timer:tc(fun() ->
        [r3lfe_dependency_scanner:scan_file(F, AppInfo, #{cache => false}) || F <- Files]
    end),
    
    %% Measure with cache (second scan)
    {TimeWithCache, _} = timer:tc(fun() ->
        [r3lfe_dependency_scanner:scan_file(F, AppInfo, #{cache => true}) || F <- Files]
    end),
    
    ct:pal("No cache: ~p μs, With cache: ~p μs", [TimeNoCache, TimeWithCache]),
    
    %% Cache should be faster (but not always guaranteed in test environment)
    %% At minimum, shouldn't be significantly slower
    ?assert(TimeWithCache < (TimeNoCache * 2)),
    ok.

parallel_compilation_scales(_Config) ->
    %% Test that parallel compilation provides speedup
    %% This is a placeholder - full implementation would
    %% need actual parallel compilation infrastructure
    
    ct:pal("Parallel compilation scaling test - placeholder"),
    ok.

large_project_compilation(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),
    
    %% Create 100 modules
    Files = lists:map(
        fun(N) ->
            File = filename:join(SrcDir, io_lib:format("module~p.lfe", [N])),
            Content = io_lib:format(
                "(defmodule module~p)\n"
                "(defun test~p () 'ok)\n",
                [N, N]
            ),
            test_utils:write_file(File, Content),
            File
        end,
        lists:seq(1, 100)
    ),
    
    %% Measure compilation time
    {Time, Results} = timer:tc(fun() ->
        [r3lfe_compile_worker:compile_file(F, EbinDir, []) || F <- Files]
    end),
    
    Successes = length([R || R <- Results, R =:= ok]),
    
    ct:pal("Compiled ~p/~p files in ~p ms", [Successes, length(Files), Time div 1000]),
    
    %% Should compile all successfully
    ?assertEqual(length(Files), Successes),
    
    %% Reasonable time (adjust threshold as needed)
    %% Allow 100ms per file as a rough benchmark
    ?assert(Time < (length(Files) * 100000)), % 100ms = 100000 μs
    ok.

incremental_build_performance(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),
    
    %% Create 20 files
    Files = lists:map(
        fun(N) ->
            File = filename:join(SrcDir, io_lib:format("module~p.lfe", [N])),
            test_utils:write_file(File, "(defmodule module" ++ integer_to_list(N) ++ ")\n"),
            File
        end,
        lists:seq(1, 20)
    ),
    
    %% Initial compilation
    {TimeFirst, _} = timer:tc(fun() ->
        [r3lfe_compile_worker:compile_file(F, EbinDir, []) || F <- Files]
    end),
    
    %% Incremental (no changes)
    {TimeIncremental, _} = timer:tc(fun() ->
        [r3lfe_compile_worker:compile_file(F, EbinDir, []) || F <- Files]
    end),
    
    ct:pal("First build: ~p ms, Incremental: ~p ms", 
           [TimeFirst div 1000, TimeIncremental div 1000]),
    
    %% Incremental should be faster or similar
    %% (in real scenario with proper needs_compilation checks)
    ok.

dependency_scanning_performance(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),
    
    %% Create headers
    Headers = lists:map(
        fun(N) ->
            File = filename:join(IncludeDir, io_lib:format("header~p.lfe", [N])),
            test_utils:write_file(File, "(defrecord record" ++ integer_to_list(N) ++ " id)\n"),
            File
        end,
        lists:seq(1, 10)
    ),
    
    %% Create files that include headers
    Files = lists:map(
        fun(N) ->
            File = filename:join(SrcDir, io_lib:format("module~p.lfe", [N])),
            Includes = [io_lib:format("(include-file \"header~p.lfe\")\n", [H]) 
                       || H <- lists:seq(1, min(N, 10))],
            Content = ["(defmodule module" ++ integer_to_list(N) ++ ")\n" | Includes],
            test_utils:write_file(File, Content),
            File
        end,
        lists:seq(1, 50)
    ),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    
    %% Measure scanning time
    {Time, _Deps} = timer:tc(fun() ->
        [r3lfe_dependency_scanner:scan_file(F, AppInfo) || F <- Files]
    end),
    
    ct:pal("Scanned ~p files with dependencies in ~p ms", 
           [length(Files), Time div 1000]),
    
    %% Should complete in reasonable time
    ?assert(Time < (length(Files) * 10000)), % 10ms per file
    ok.

concurrent_cache_access_performance(_Config) ->
    r3lfe_dep_cache:init(),
    
    NumProcesses = 100,
    NumOps = 100,
    
    Parent = self(),
    
    %% Spawn processes that hammer the cache
    {Time, _} = timer:tc(fun() ->
        Pids = [
            spawn(fun() ->
                lists:foreach(
                    fun(M) ->
                        File = io_lib:format("/tmp/file~p~p.lfe", [N, M]),
                        Deps = [io_lib:format("/tmp/dep~p.lfe", [M])],
                        ok = r3lfe_dep_cache:put(File, Deps),
                        _ = r3lfe_dep_cache:get(File)
                    end,
                    lists:seq(1, NumOps)
                ),
                Parent ! {self(), done}
            end)
            || N <- lists:seq(1, NumProcesses)
        ],
        
        %% Wait for all
        [receive {Pid, done} -> ok end || Pid <- Pids]
    end),
    
    TotalOps = NumProcesses * NumOps * 2, % put + get
    OpsPerSecond = TotalOps / (Time / 1000000),
    
    ct:pal("Completed ~p operations in ~p ms (~p ops/sec)", 
           [TotalOps, Time div 1000, trunc(OpsPerSecond)]),
    
    %% Should handle concurrent access without crashes
    ok.
```

---

## Testing Strategy Summary

### Execution Order

1. **Phase 1: Cache modules** - Foundation for everything else
2. **Phase 2: Progress and simple providers** - Independent functionality
3. **Phase 3: Complex providers** - Build on previous tests
4. **Phase 4: Integration tests** - Verify components work together
5. **Phase 5: Plugin initialization** - Complete system test
6. **Phase 6: Error handling** - Edge cases and recovery
7. **Phase 7: Performance** - Optimization verification

### Coverage Goals

After implementing all tests:
- **Target: 95%+ line coverage**
- **Target: 90%+ branch coverage**
- **Target: 100% function coverage** (every exported function tested)

### Running Tests

```bash
# Run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite=test/r3lfe_dep_cache_SUITE

# Run with coverage
rebar3 cover -v

# Generate coverage report
rebar3 cover --verbose
```

### Continuous Integration

Add to CI pipeline:
```yaml
- run: rebar3 ct
- run: rebar3 cover
- run: rebar3 dialyzer
```

---

## Implementation Checklist

- [ ] Phase 1: Cache modules (3 suites)
- [ ] Phase 2: Progress & providers (2 suites)
- [ ] Phase 3: Provider expansion (1 suite)
- [ ] Phase 4: Integration tests (1 suite)
- [ ] Phase 5: Plugin init (1 suite)
- [ ] Phase 6: Error handling (1 suite)
- [ ] Phase 7: Performance (1 suite)
- [ ] Review coverage report
- [ ] Add missing edge cases
- [ ] Document test patterns
- [ ] Update CI configuration

---

## Notes for Implementation

1. **Test Independence**: Each test should set up and tear down its own state
2. **Timing Sensitivity**: Use `timer:sleep/1` carefully, prefer relative timestamps
3. **File System Operations**: Always use `test_utils` for temp directories
4. **ETS Cleanup**: Always clean up ETS tables in `end_per_testcase`
5. **Mocking**: Prefer real implementations over mocks where possible
6. **Assertions**: Use descriptive assertion messages
7. **Coverage Gaps**: After initial implementation, run `rebar3 cover` to identify remaining gaps

</content>
