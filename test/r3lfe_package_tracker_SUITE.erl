-module(r3lfe_package_tracker_SUITE).

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

%%====================================================================
%% CT Callbacks
%%====================================================================

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

%%====================================================================
%% Test Cases
%%====================================================================

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
