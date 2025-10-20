-module(rb3lfe_package_SUITE).

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
    discover_flat_files/1,
    discover_nested_files/1,
    discover_mixed_structure/1,
    calculate_module_name_simple/1,
    calculate_module_name_nested/1,
    validate_module_name_valid/1,
    validate_module_name_invalid/1,
    is_nested_file_flat/1,
    is_nested_file_nested/1,
    prepare_packages_single/1,
    prepare_packages_multiple/1,
    prepare_packages_none/1,
    cleanup_packages_success/1,
    package_lifecycle_full/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        discover_flat_files,
        discover_nested_files,
        discover_mixed_structure,
        calculate_module_name_simple,
        calculate_module_name_nested,
        validate_module_name_valid,
        validate_module_name_invalid,
        is_nested_file_flat,
        is_nested_file_nested,
        prepare_packages_single,
        prepare_packages_multiple,
        prepare_packages_none,
        cleanup_packages_success,
        package_lifecycle_full
    ].

init_per_suite(Config) ->
    rb3lfe_package_tracker:init(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    rb3lfe_package_tracker:cleanup_all(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    rb3lfe_package_tracker:cleanup_all(),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases - Discovery
%%====================================================================

discover_flat_files(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),

    %% Create flat files
    File1 = filename:join(SrcDir, "module1.lfe"),
    File2 = filename:join(SrcDir, "module2.lfe"),
    test_utils:write_file(File1, "(defmodule module1)\n"),
    test_utils:write_file(File2, "(defmodule module2)\n"),

    Files = rb3lfe_package:discover_files(SrcDir),

    ?assertEqual(2, length(Files)),
    ?assert(lists:member(File1, Files)),
    ?assert(lists:member(File2, Files)),
    ok.

discover_nested_files(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create nested structure
    SubDir = filename:join(SrcDir, "myapp"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    File1 = filename:join(SubDir, "module1.lfe"),
    test_utils:write_file(File1, "(defmodule myapp.module1)\n"),

    Files = rb3lfe_package:discover_files(SrcDir),

    ?assertEqual(1, length(Files)),
    ?assert(lists:member(File1, Files)),
    ok.

discover_mixed_structure(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create flat file
    FlatFile = filename:join(SrcDir, "flat.lfe"),
    test_utils:write_file(FlatFile, "(defmodule flat)\n"),

    %% Create nested files
    SubDir1 = filename:join(SrcDir, "package1"),
    SubDir2 = filename:join([SrcDir, "package2", "deep"]),
    ok = filelib:ensure_dir(filename:join(SubDir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(SubDir2, "dummy")),

    Nested1 = filename:join(SubDir1, "module1.lfe"),
    Nested2 = filename:join(SubDir2, "module2.lfe"),
    test_utils:write_file(Nested1, "(defmodule package1.module1)\n"),
    test_utils:write_file(Nested2, "(defmodule package2.deep.module2)\n"),

    Files = rb3lfe_package:discover_files(SrcDir),

    ?assertEqual(3, length(Files)),
    ?assert(lists:member(FlatFile, Files)),
    ?assert(lists:member(Nested1, Files)),
    ?assert(lists:member(Nested2, Files)),
    ok.

%%====================================================================
%% Test Cases - Module Naming
%%====================================================================

calculate_module_name_simple(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join([SrcDir, "my", "package.lfe"]),

    ModuleName = rb3lfe_package:calculate_module_name(File, SrcDir),

    ?assertEqual("my.package", ModuleName),
    ok.

calculate_module_name_nested(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join([SrcDir, "my", "deep", "package.lfe"]),

    ModuleName = rb3lfe_package:calculate_module_name(File, SrcDir),

    ?assertEqual("my.deep.package", ModuleName),
    ok.

validate_module_name_valid(_Config) ->
    ?assertEqual(ok, rb3lfe_package:validate_module_name("simple")),
    ?assertEqual(ok, rb3lfe_package:validate_module_name("my.package")),
    ?assertEqual(ok, rb3lfe_package:validate_module_name("my-package")),
    ?assertEqual(ok, rb3lfe_package:validate_module_name("my_package")),
    ?assertEqual(ok, rb3lfe_package:validate_module_name("a.b.c.d")),
    ok.

validate_module_name_invalid(_Config) ->
    ?assertMatch({error, empty_name},
                 rb3lfe_package:validate_module_name("")),
    ?assertMatch({error, starts_with_dot},
                 rb3lfe_package:validate_module_name(".package")),
    ?assertMatch({error, ends_with_dot},
                 rb3lfe_package:validate_module_name("package.")),
    ?assertMatch({error, consecutive_dots},
                 rb3lfe_package:validate_module_name("my..package")),
    ?assertMatch({error, invalid_characters},
                 rb3lfe_package:validate_module_name("my package")),
    ok.

%%====================================================================
%% Test Cases - File Classification
%%====================================================================

is_nested_file_flat(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join(SrcDir, "flat.lfe"),

    ?assert(not rb3lfe_package:is_nested_file(File, SrcDir)),
    ok.

is_nested_file_nested(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join([SrcDir, "my", "nested.lfe"]),

    ?assert(rb3lfe_package:is_nested_file(File, SrcDir)),
    ok.

%%====================================================================
%% Test Cases - Package Preparation
%%====================================================================

prepare_packages_single(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create nested file
    SubDir = filename:join(SrcDir, "myapp"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    SourceFile = filename:join(SubDir, "module.lfe"),
    test_utils:write_file(SourceFile, "(defmodule myapp.module)\n"),

    %% Prepare packages
    {ok, PackageInfos} = rb3lfe_package:prepare_packages([SourceFile]),

    ?assertEqual(1, length(PackageInfos)),

    [Info] = PackageInfos,
    ?assertMatch(#{
        source_file := _,
        temp_file := _,
        module_name := "myapp.module",
        source_dir := _
    }, Info),

    %% Check temp file exists
    #{temp_file := TempFile} = Info,
    ?assert(filelib:is_file(TempFile)),

    %% Clean up
    rb3lfe_package:cleanup_packages(PackageInfos),
    ?assert(not filelib:is_file(TempFile)),

    ok.

prepare_packages_multiple(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create multiple nested files
    Files = lists:map(
        fun(N) ->
            SubDir = filename:join([SrcDir, "pkg" ++ integer_to_list(N)]),
            ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

            File = filename:join(SubDir, "module.lfe"),
            Content = io_lib:format("(defmodule pkg~p.module)\n", [N]),
            test_utils:write_file(File, Content),
            File
        end,
        lists:seq(1, 5)
    ),

    %% Prepare all packages
    {ok, PackageInfos} = rb3lfe_package:prepare_packages(Files),

    ?assertEqual(5, length(PackageInfos)),

    %% Verify all temp files exist
    lists:foreach(
        fun(#{temp_file := TempFile}) ->
            ?assert(filelib:is_file(TempFile))
        end,
        PackageInfos
    ),

    %% Clean up
    rb3lfe_package:cleanup_packages(PackageInfos),

    %% Verify all temp files deleted
    lists:foreach(
        fun(#{temp_file := TempFile}) ->
            ?assert(not filelib:is_file(TempFile))
        end,
        PackageInfos
    ),

    ok.

prepare_packages_none(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),

    %% Create flat file (not a package)
    FlatFile = filename:join(SrcDir, "flat.lfe"),
    test_utils:write_file(FlatFile, "(defmodule flat)\n"),

    %% Prepare packages
    {ok, PackageInfos} = rb3lfe_package:prepare_packages([FlatFile]),

    %% Should return empty list (no packages)
    ?assertEqual(0, length(PackageInfos)),

    ok.

cleanup_packages_success(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create temp file manually
    TempFile = filename:join(SrcDir, "temp.package.lfe"),
    test_utils:write_file(TempFile, "(defmodule temp-package)\n"),

    Info = #{
        source_file => filename:join([SrcDir, "sub", "package.lfe"]),
        temp_file => TempFile,
        module_name => "temp.package",
        source_dir => SrcDir
    },

    ?assert(filelib:is_file(TempFile)),

    %% Clean up
    ok = rb3lfe_package:cleanup_packages([Info]),

    ?assert(not filelib:is_file(TempFile)),

    ok.

%%====================================================================
%% Test Cases - Full Lifecycle
%%====================================================================

package_lifecycle_full(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create complex structure
    Structure = [
        {"flat.lfe", "(defmodule flat)\n"},
        {"my/nested.lfe", "(defmodule my.nested)\n"},
        {"my/deep/module.lfe", "(defmodule my.deep.module)\n"}
    ],

    _Files = lists:map(
        fun({Path, Content}) ->
            FullPath = filename:join(SrcDir, Path),
            ok = filelib:ensure_dir(FullPath),
            test_utils:write_file(FullPath, Content),
            FullPath
        end,
        Structure
    ),

    %% Discover all files
    AllFiles = rb3lfe_package:discover_files(SrcDir),
    ?assertEqual(3, length(AllFiles)),

    %% Prepare packages
    {ok, PackageInfos} = rb3lfe_package:prepare_packages(AllFiles),

    %% Should have 2 packages (flat.lfe is not a package)
    ?assertEqual(2, length(PackageInfos)),

    %% Verify package info is correct
    ModuleNames = [maps:get(module_name, Info) || Info <- PackageInfos],
    ?assert(lists:member("my.nested", ModuleNames)),
    ?assert(lists:member("my.deep.module", ModuleNames)),

    %% Verify cleanup works
    ok = rb3lfe_package:cleanup_packages(PackageInfos),

    %% Verify temp files removed
    lists:foreach(
        fun(#{temp_file := TempFile}) ->
            ?assert(not filelib:is_file(TempFile))
        end,
        PackageInfos
    ),

    ok.
