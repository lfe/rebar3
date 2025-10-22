-module(r3lfe_compile_opts_SUITE).

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
    init_creates_table_once/1,
    init_idempotent/1,
    get_opts_hash_deterministic/1,
    get_opts_hash_different_for_different_opts/1,
    get_opts_hash_order_independent/1,
    opts_changed_no_cache_returns_true/1,
    opts_changed_cache_uninitialized/1,
    opts_changed_same_opts_returns_false/1,
    opts_changed_different_opts_returns_true/1,
    opts_changed_after_save/1,
    save_opts_hash_creates_entry/1,
    save_opts_hash_updates_entry/1,
    save_opts_hash_uninitialized_cache/1,
    clear_opts_cache_removes_all/1,
    clear_opts_cache_uninitialized/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        init_creates_table_once,
        init_idempotent,
        get_opts_hash_deterministic,
        get_opts_hash_different_for_different_opts,
        get_opts_hash_order_independent,
        opts_changed_no_cache_returns_true,
        opts_changed_cache_uninitialized,
        opts_changed_same_opts_returns_false,
        opts_changed_different_opts_returns_true,
        opts_changed_after_save,
        save_opts_hash_creates_entry,
        save_opts_hash_updates_entry,
        save_opts_hash_uninitialized_cache,
        clear_opts_cache_removes_all,
        clear_opts_cache_uninitialized
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Ensure cache is cleared before each test
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up cache after each test
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

init_creates_table_once(_Config) ->
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

init_idempotent(_Config) ->
    %% Ensure cache is cleared first
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,

    %% First init
    ok = r3lfe_compile_opts:init(),
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

get_opts_hash_different_for_different_opts(_Config) ->
    Opts1 = [verbose, debug_info],
    Opts2 = [verbose],

    Hash1 = r3lfe_compile_opts:get_opts_hash(Opts1),
    Hash2 = r3lfe_compile_opts:get_opts_hash(Opts2),

    %% Different options should produce different hashes
    ?assertNotEqual(Hash1, Hash2),
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
    r3lfe_compile_opts:init(),

    File = "/tmp/test.lfe",
    Opts = [verbose],

    %% With empty cache, should return true
    Changed = r3lfe_compile_opts:opts_changed(File, Opts),

    ?assert(Changed, "Should detect change when no cached options"),
    ok.

opts_changed_cache_uninitialized(_Config) ->
    %% Don't initialize cache
    File = "/tmp/test.lfe",
    Opts = [verbose],

    %% Should return true when cache uninitialized
    Changed = r3lfe_compile_opts:opts_changed(File, Opts),

    ?assert(Changed, "Should detect change when cache uninitialized"),
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

opts_changed_after_save(_Config) ->
    r3lfe_compile_opts:init(),

    File = "/tmp/test.lfe",
    Opts = [verbose, debug_info],

    %% Initially should show changed
    ?assert(r3lfe_compile_opts:opts_changed(File, Opts)),

    %% Save options
    ok = r3lfe_compile_opts:save_opts_hash(File, Opts),

    %% After save, same options should not show changed
    ?assertNot(r3lfe_compile_opts:opts_changed(File, Opts)),
    ok.

save_opts_hash_creates_entry(_Config) ->
    r3lfe_compile_opts:init(),

    File = "/tmp/test.lfe",
    Opts = [verbose],

    %% Should not be cached yet
    ?assert(r3lfe_compile_opts:opts_changed(File, Opts)),

    %% Save
    ok = r3lfe_compile_opts:save_opts_hash(File, Opts),

    %% Should now be cached
    ?assertNot(r3lfe_compile_opts:opts_changed(File, Opts)),
    ok.

save_opts_hash_updates_entry(_Config) ->
    r3lfe_compile_opts:init(),

    File = "/tmp/test.lfe",
    Opts1 = [verbose],
    Opts2 = [verbose, debug_info],

    %% Save first set
    ok = r3lfe_compile_opts:save_opts_hash(File, Opts1),
    ?assertNot(r3lfe_compile_opts:opts_changed(File, Opts1)),

    %% Opts2 should be different
    ?assert(r3lfe_compile_opts:opts_changed(File, Opts2)),

    %% Update to Opts2
    ok = r3lfe_compile_opts:save_opts_hash(File, Opts2),

    %% Now Opts2 should be cached
    ?assertNot(r3lfe_compile_opts:opts_changed(File, Opts2)),

    %% And Opts1 should be different
    ?assert(r3lfe_compile_opts:opts_changed(File, Opts1)),
    ok.

save_opts_hash_uninitialized_cache(_Config) ->
    %% Don't initialize cache
    File = "/tmp/test.lfe",
    Opts = [verbose],

    %% Should handle gracefully (may initialize or no-op)
    Result = r3lfe_compile_opts:save_opts_hash(File, Opts),

    %% Should not crash
    ?assertEqual(ok, Result),
    ok.

clear_opts_cache_removes_all(_Config) ->
    r3lfe_compile_opts:init(),

    %% Add some entries
    ok = r3lfe_compile_opts:save_opts_hash("/tmp/file1.lfe", [verbose]),
    ok = r3lfe_compile_opts:save_opts_hash("/tmp/file2.lfe", [debug_info]),

    %% Verify entries exist
    ?assertNot(r3lfe_compile_opts:opts_changed("/tmp/file1.lfe", [verbose])),
    ?assertNot(r3lfe_compile_opts:opts_changed("/tmp/file2.lfe", [debug_info])),

    %% Clear cache
    ok = r3lfe_compile_opts:clear_opts_cache(),

    %% Entries should be gone
    ?assert(r3lfe_compile_opts:opts_changed("/tmp/file1.lfe", [verbose])),
    ?assert(r3lfe_compile_opts:opts_changed("/tmp/file2.lfe", [debug_info])),
    ok.

clear_opts_cache_uninitialized(_Config) ->
    %% Don't initialize cache

    %% Should handle gracefully
    Result = r3lfe_compile_opts:clear_opts_cache(),

    %% Should not crash
    ?assertEqual(ok, Result),
    ok.
