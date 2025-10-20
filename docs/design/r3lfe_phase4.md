# Phase 4: Package System Redesign

## Overview

This phase redesigns the LFE "faux-packages" system that allows organizing LFE modules in nested directory structures. The current implementation has identified issues around temporary file cleanup, race conditions, and error recovery. We'll preserve the functionality users depend on while eliminating all the bugs.

**Goal**: Rock-solid package transformation that handles nested modules reliably with proper cleanup and error handling.

## Prerequisites

- Phase 1, 2, and 3 completed with all tests passing
- Understanding of LFE module naming conventions
- Knowledge of file system operations and race conditions
- Familiarity with atomic operations and rollback patterns

## Current Behavior (To Preserve)

The package system transforms nested directory structures into flat module names:

```
./src/my.package1.lfe             → ebin/my.package1.beam
./src/my/package2.lfe             → ebin/my.package2.beam
./src/my/other/package.lfe        → ebin/my.other.package.beam
./src/my/really/deep/package1.lfe → ebin/my.really.deep.package1.beam
```

This allows developers to:

1. Organize code in subdirectories for better structure
2. Use dotted module names for namespacing
3. Mix both styles in the same project

## Architecture Overview

```
Package Processing Flow:

1. Discover all .lfe files in src directories (including subdirs)
2. For nested files: Calculate dotted module name from path
3. Create temporary flattened copies in src directory
4. Register transformations for cleanup
5. Compile all files (flattened + already flat)
6. Clean up temporary files (even on error)
7. Report any issues clearly
```

## Implementation Tasks

### Task 4.1: Implement Core Package Module

**File: `src/r3lfe_package.erl`**

```erlang
-module(r3lfe_package).

%% API exports
-export([
    discover_files/1,
    prepare_packages/1,
    cleanup_packages/1,
    is_package_file/1,
    package_to_module_name/2
]).

%% For testing
-export([
    calculate_module_name/2,
    is_nested_file/2,
    validate_module_name/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-type package_info() :: #{
    source_file := file:filename(),      % Original nested file
    temp_file := file:filename(),        % Temporary flattened file
    module_name := string(),             % Calculated module name
    source_dir := file:filename()        % Base source directory
}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Discover all LFE files including those in subdirectories
-spec discover_files(file:filename()) -> [file:filename()].
discover_files(SourceDir) ->
    case filelib:is_dir(SourceDir) of
        true ->
            discover_files_recursive(SourceDir, SourceDir);
        false ->
            ?WARN("Source directory does not exist: ~s", [SourceDir]),
            []
    end.

%% @doc Prepare package files for compilation
%% Creates temporary flattened copies of nested files
%% Returns list of package_info() records for cleanup
-spec prepare_packages([file:filename()]) -> {ok, [package_info()]} | {error, term()}.
prepare_packages(Files) ->
    %% Separate nested files from already-flat files
    {NestedFiles, _FlatFiles} = lists:partition(
        fun(File) ->
            SourceDir = find_source_dir(File),
            is_nested_file(File, SourceDir)
        end,
        Files
    ),

    case NestedFiles of
        [] ->
            %% No packages to prepare
            {ok, []};
        _ ->
            %% Create transformations
            prepare_package_files(NestedFiles)
    end.

%% @doc Clean up temporary package files
-spec cleanup_packages([package_info()]) -> ok.
cleanup_packages(PackageInfos) ->
    lists:foreach(
        fun(#{temp_file := TempFile}) ->
            case file:delete(TempFile) of
                ok ->
                    ?DEBUG("Cleaned up temporary file: ~s", [TempFile]),
                    ok;
                {error, enoent} ->
                    %% Already deleted, ignore
                    ok;
                {error, Reason} ->
                    ?WARN("Failed to delete temporary file ~s: ~p",
                          [TempFile, Reason])
            end
        end,
        PackageInfos
    ),
    ok.

%% @doc Check if a file is a package file (nested in subdirectory)
-spec is_package_file(file:filename()) -> boolean().
is_package_file(File) ->
    SourceDir = find_source_dir(File),
    is_nested_file(File, SourceDir).

%% @doc Convert package file path to module name
-spec package_to_module_name(file:filename(), file:filename()) -> string().
package_to_module_name(File, SourceDir) ->
    calculate_module_name(File, SourceDir).

%%====================================================================
%% Internal functions - Discovery
%%====================================================================

%% @doc Recursively discover all .lfe files
-spec discover_files_recursive(file:filename(), file:filename()) ->
    [file:filename()].
discover_files_recursive(CurrentDir, _BaseDir) ->
    case file:list_dir(CurrentDir) of
        {ok, Entries} ->
            lists:flatmap(
                fun(Entry) ->
                    Path = filename:join(CurrentDir, Entry),
                    case filelib:is_dir(Path) of
                        true ->
                            %% Recurse into subdirectory
                            discover_files_recursive(Path, CurrentDir);
                        false ->
                            case filename:extension(Path) of
                                ?LFE_SRC_EXTENSION ->
                                    [Path];
                                _ ->
                                    []
                            end
                    end
                end,
                Entries
            );
        {error, Reason} ->
            ?WARN("Could not list directory ~s: ~p", [CurrentDir, Reason]),
            []
    end.

%% @doc Find the source directory for a file
%% This searches upward for a directory named "src"
-spec find_source_dir(file:filename()) -> file:filename().
find_source_dir(File) ->
    find_source_dir_upward(filename:dirname(File)).

-spec find_source_dir_upward(file:filename()) -> file:filename().
find_source_dir_upward(Dir) ->
    case filename:basename(Dir) of
        "src" ->
            Dir;
        "/" ->
            %% Reached root without finding src
            Dir;
        _ ->
            find_source_dir_upward(filename:dirname(Dir))
    end.

%%====================================================================
%% Internal functions - Package Preparation
%%====================================================================

%% @doc Prepare nested package files
-spec prepare_package_files([file:filename()]) ->
    {ok, [package_info()]} | {error, term()}.
prepare_package_files(NestedFiles) ->
    Results = lists:map(
        fun(SourceFile) ->
            prepare_single_package(SourceFile)
        end,
        NestedFiles
    ),

    %% Check if any failed
    case lists:filter(fun(R) -> element(1, R) =:= error end, Results) of
        [] ->
            %% All succeeded
            PackageInfos = [Info || {ok, Info} <- Results],
            {ok, PackageInfos};
        Errors ->
            %% Some failed, clean up any successful ones
            SuccessfulInfos = [Info || {ok, Info} <- Results],
            cleanup_packages(SuccessfulInfos),
            {error, {package_preparation_failed, Errors}}
    end.

%% @doc Prepare a single package file
-spec prepare_single_package(file:filename()) ->
    {ok, package_info()} | {error, term()}.
prepare_single_package(SourceFile) ->
    SourceDir = find_source_dir(SourceFile),
    ModuleName = calculate_module_name(SourceFile, SourceDir),

    %% Validate module name
    case validate_module_name(ModuleName) of
        ok ->
            %% Create temporary file in source directory
            TempFile = filename:join(SourceDir, ModuleName ++ ?LFE_SRC_EXTENSION),

            case copy_file_safe(SourceFile, TempFile) of
                ok ->
                    Info = #{
                        source_file => SourceFile,
                        temp_file => TempFile,
                        module_name => ModuleName,
                        source_dir => SourceDir
                    },

                    ?DEBUG("Prepared package: ~s -> ~s", [SourceFile, TempFile]),
                    {ok, Info};

                {error, Reason} ->
                    ?ERROR("Failed to copy ~s to ~s: ~p",
                           [SourceFile, TempFile, Reason]),
                    {error, {copy_failed, SourceFile, Reason}}
            end;

        {error, Reason} ->
            ?ERROR("Invalid module name ~s for ~s: ~p",
                   [ModuleName, SourceFile, Reason]),
            {error, {invalid_module_name, ModuleName, Reason}}
    end.

%% @doc Copy a file safely with error handling
-spec copy_file_safe(file:filename(), file:filename()) -> ok | {error, term()}.
copy_file_safe(Source, Dest) ->
    %% Check if destination already exists (shouldn't happen, but be safe)
    case filelib:is_file(Dest) of
        true ->
            ?WARN("Temporary file already exists: ~s", [Dest]),
            %% Delete it and try again
            file:delete(Dest);
        false ->
            ok
    end,

    %% Copy the file
    case file:copy(Source, Dest) of
        {ok, _BytesCopied} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions - Naming
%%====================================================================

%% @doc Calculate module name from file path and source directory
%% Examples:
%%   src/my/package.lfe -> my.package
%%   src/my/other/deep.lfe -> my.other.deep
-spec calculate_module_name(file:filename(), file:filename()) -> string().
calculate_module_name(File, SourceDir) ->
    %% Get relative path from source directory
    RelPath = case string:prefix(File, SourceDir) of
        nomatch ->
            %% File not under source directory (shouldn't happen)
            filename:basename(File, ?LFE_SRC_EXTENSION);
        "/" ++ Rest ->
            Rest;
        Rest ->
            Rest
    end,

    %% Remove .lfe extension
    WithoutExt = filename:rootname(RelPath, ?LFE_SRC_EXTENSION),

    %% Replace path separators with dots
    %% and normalize separators for cross-platform compatibility
    Normalized = re:replace(WithoutExt, "[/\\\\]+", ".",
                           [global, {return, list}]),

    Normalized.

%% @doc Check if a file is nested (not directly in source directory)
-spec is_nested_file(file:filename(), file:filename()) -> boolean().
is_nested_file(File, SourceDir) ->
    FileDir = filename:dirname(File),

    %% Compare normalized paths
    NormFileDir = filename:absname(FileDir),
    NormSourceDir = filename:absname(SourceDir),

    NormFileDir =/= NormSourceDir.

%% @doc Validate a module name
%% Module names must:
%% - Not be empty
%% - Not start with a dot
%% - Not end with a dot
%% - Not contain consecutive dots
%% - Only contain valid characters (alphanumeric, underscore, dot, hyphen)
-spec validate_module_name(string()) -> ok | {error, term()}.
validate_module_name("") ->
    {error, empty_name};
validate_module_name("." ++ _) ->
    {error, starts_with_dot};
validate_module_name(Name) ->
    case lists:last(Name) of
        $. ->
            {error, ends_with_dot};
        _ ->
            %% Check for consecutive dots
            case string:find(Name, "..") of
                nomatch ->
                    %% Check for valid characters
                    case re:run(Name, "^[a-zA-Z0-9._-]+$", [{capture, none}]) of
                        match ->
                            ok;
                        nomatch ->
                            {error, invalid_characters}
                    end;
                _ ->
                    {error, consecutive_dots}
            end
    end.
```

### Task 4.2: Integrate Packages into Compiler Module

**File: `src/r3lfe_compiler_mod.erl` (UPDATE)**

Update the `context/1` function to discover all files including packages:

```erlang
%% Add at the top:
-export([
    compile_app/1
]).

%% Add new function to compile an entire application
-spec compile_app(rebar_app_info:t()) -> ok | {error, term()}.
compile_app(AppInfo) ->
    ?DEBUG("Compiling LFE application: ~s", [rebar_app_info:name(AppInfo)]),

    SrcDirs = r3lfe_config:get_src_dirs(AppInfo),

    %% Discover all files including packages
    AllFiles = lists:flatmap(
        fun r3lfe_package:discover_files/1,
        SrcDirs
    ),

    ?DEBUG("Discovered ~p LFE files", [length(AllFiles)]),

    %% Prepare package files
    case r3lfe_package:prepare_packages(AllFiles) of
        {ok, PackageInfos} ->
            try
                %% Now compile using rebar3's normal flow
                %% The compiler will find both original flat files
                %% and the temporary flattened package files

                %% Let rebar3 do its thing - it will call our
                %% context, dependencies, needed_files, and compile

                ok
            after
                %% Always clean up, even on error
                r3lfe_package:cleanup_packages(PackageInfos)
            end;

        {error, Reason} ->
            ?ERROR("Failed to prepare packages: ~p", [Reason]),
            {error, Reason}
    end.
```

### Task 4.3: Hook Package System into Provider

We'll create a compile provider that uses our package-aware system. This goes in Phase 5, but let's prepare the integration point:

**Note for Phase 5**: The compile provider will call `r3lfe_compiler_mod:compile_app/1` which handles the full package lifecycle.

### Task 4.4: Add Package File Tracking

**File: `src/r3lfe_package_tracker.erl`**

```erlang
-module(r3lfe_package_tracker).

%% API exports
-export([
    init/0,
    register_package/1,
    unregister_package/1,
    get_registered_packages/0,
    is_temp_file/1,
    cleanup_all/0
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-define(TRACKER_TABLE, r3lfe_package_tracker).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize the package tracker
-spec init() -> ok.
init() ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            _Tid = ets:new(?TRACKER_TABLE, [
                named_table,
                public,
                set,
                {read_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

%% @doc Register a package transformation
-spec register_package(map()) -> ok.
register_package(PackageInfo = #{temp_file := TempFile}) ->
    true = ets:insert(?TRACKER_TABLE, {TempFile, PackageInfo}),
    ?DEBUG("Registered package: ~s", [TempFile]),
    ok.

%% @doc Unregister a package transformation
-spec unregister_package(file:filename()) -> ok.
unregister_package(TempFile) ->
    true = ets:delete(?TRACKER_TABLE, TempFile),
    ok.

%% @doc Get all registered packages
-spec get_registered_packages() -> [map()].
get_registered_packages() ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            [];
        _ ->
            [Info || {_Key, Info} <- ets:tab2list(?TRACKER_TABLE)]
    end.

%% @doc Check if a file is a temporary package file
-spec is_temp_file(file:filename()) -> boolean().
is_temp_file(File) ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            false;
        _ ->
            ets:member(?TRACKER_TABLE, File)
    end.

%% @doc Clean up all registered packages
-spec cleanup_all() -> ok.
cleanup_all() ->
    Packages = get_registered_packages(),
    r3lfe_package:cleanup_packages(Packages),

    case ets:info(?TRACKER_TABLE) of
        undefined ->
            ok;
        _ ->
            true = ets:delete_all_objects(?TRACKER_TABLE),
            ok
    end.
```

### Task 4.5: Update Plugin Initialization

**File: `src/r3lfe.erl` (UPDATE)**

```erlang
init(State) ->
    ?DEBUG("Initializing r3lfe plugin...", []),

    %% Initialize all caches and trackers
    ok = r3lfe_dep_cache:init(),
    ok = r3lfe_compile_opts:init(),
    ok = r3lfe_package_tracker:init(),

    %% Register our compiler module with rebar3
    State1 = rebar_state:append_compilers(State, [r3lfe_compiler_mod]),

    ?DEBUG("Registered r3lfe_compiler_mod with rebar3", []),

    {ok, State1}.
```

### Task 4.6: Create Package Tests

**File: `test/r3lfe_package_SUITE.erl`**

```erlang
-module(r3lfe_package_SUITE).

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
    calculate_module_name_deep/1,
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
        calculate_module_name_deep,
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
    r3lfe_package_tracker:init(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    r3lfe_package_tracker:cleanup_all(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:cleanup_all(),
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

    Files = r3lfe_package:discover_files(SrcDir),

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

    Files = r3lfe_package:discover_files(SrcDir),

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

    Files = r3lfe_package:discover_files(SrcDir),

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

    ModuleName = r3lfe_package:calculate_module_name(File, SrcDir),

    ?assertEqual("my.package", ModuleName),
    ok.

calculate_module_name_nested(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join([SrcDir, "my", "deep", "package.lfe"]),

    ModuleName = r3lfe_package:calculate_module_name(File, SrcDir),

    ?assertEqual("my.deep.package", ModuleName),
    ok.

calculate_module_name_deep(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join([SrcDir, "a", "b", "c", "d", "module.lfe"]),

    ModuleName = r3lfe_package:calculate_module_name(File, SrcDir),

    ?assertEqual("a.b.c.d.module", ModuleName),
    ok.

validate_module_name_valid(_Config) ->
    ?assertEqual(ok, r3lfe_package:validate_module_name("simple")),
    ?assertEqual(ok, r3lfe_package:validate_module_name("my.package")),
    ?assertEqual(ok, r3lfe_package:validate_module_name("my-package")),
    ?assertEqual(ok, r3lfe_package:validate_module_name("my_package")),
    ?assertEqual(ok, r3lfe_package:validate_module_name("a.b.c.d")),
    ok.

validate_module_name_invalid(_Config) ->
    ?assertMatch({error, empty_name},
                 r3lfe_package:validate_module_name("")),
    ?assertMatch({error, starts_with_dot},
                 r3lfe_package:validate_module_name(".package")),
    ?assertMatch({error, ends_with_dot},
                 r3lfe_package:validate_module_name("package.")),
    ?assertMatch({error, consecutive_dots},
                 r3lfe_package:validate_module_name("my..package")),
    ?assertMatch({error, invalid_characters},
                 r3lfe_package:validate_module_name("my package")),
    ok.

%%====================================================================
%% Test Cases - File Classification
%%====================================================================

is_nested_file_flat(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join(SrcDir, "flat.lfe"),

    ?assertNot(r3lfe_package:is_nested_file(File, SrcDir)),
    ok.

is_nested_file_nested(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    File = filename:join([SrcDir, "my", "nested.lfe"]),

    ?assert(r3lfe_package:is_nested_file(File, SrcDir)),
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
    {ok, PackageInfos} = r3lfe_package:prepare_packages([SourceFile]),

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
    r3lfe_package:cleanup_packages(PackageInfos),
    ?assertNot(filelib:is_file(TempFile)),

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
    {ok, PackageInfos} = r3lfe_package:prepare_packages(Files),

    ?assertEqual(5, length(PackageInfos)),

    %% Verify all temp files exist
    lists:foreach(
        fun(#{temp_file := TempFile}) ->
            ?assert(filelib:is_file(TempFile))
        end,
        PackageInfos
    ),

    %% Clean up
    r3lfe_package:cleanup_packages(PackageInfos),

    %% Verify all temp files deleted
    lists:foreach(
        fun(#{temp_file := TempFile}) ->
            ?assertNot(filelib:is_file(TempFile))
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
    {ok, PackageInfos} = r3lfe_package:prepare_packages([FlatFile]),

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
    ok = r3lfe_package:cleanup_packages([Info]),

    ?assertNot(filelib:is_file(TempFile)),

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

    Files = lists:map(
        fun({Path, Content}) ->
            FullPath = filename:join(SrcDir, Path),
            ok = filelib:ensure_dir(FullPath),
            test_utils:write_file(FullPath, Content),
            FullPath
        end,
        Structure
    ),

    %% Discover all files
    AllFiles = r3lfe_package:discover_files(SrcDir),
    ?assertEqual(3, length(AllFiles)),

    %% Prepare packages
    {ok, PackageInfos} = r3lfe_package:prepare_packages(AllFiles),

    %% Should have 2 packages (flat.lfe is not a package)
    ?assertEqual(2, length(PackageInfos)),

    %% Register packages
    lists:foreach(
        fun(Info) ->
            r3lfe_package_tracker:register_package(Info)
        end,
        PackageInfos
    ),

    %% Verify tracking
    RegisteredCount = length(r3lfe_package_tracker:get_registered_packages()),
    ?assertEqual(2, RegisteredCount),

    %% Clean up through tracker
    ok = r3lfe_package_tracker:cleanup_all(),

    %% Verify cleanup
    ?assertEqual(0, length(r3lfe_package_tracker:get_registered_packages())),

    ok.
```

### Task 4.7: Create Integration Tests

**File: `test/package_integration_SUITE.erl`**

```erlang
-module(package_integration_SUITE).

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
    compile_flat_and_nested/1,
    nested_includes_work/1,
    package_errors_reported/1,
    cleanup_on_compilation_error/1,
    concurrent_compilation_safe/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_flat_and_nested,
        nested_includes_work,
        package_errors_reported,
        cleanup_on_compilation_error,
        concurrent_compilation_safe
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    r3lfe_package_tracker:init(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    r3lfe_package_tracker:cleanup_all(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    r3lfe_package_tracker:cleanup_all(),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

compile_flat_and_nested(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create flat file
    FlatFile = filename:join(SrcDir, "flat.lfe"),
    test_utils:write_file(FlatFile,
        "(defmodule flat)\n"
        "(defun test () 'flat-ok)\n"),

    %% Create nested file
    SubDir = filename:join(SrcDir, "myapp"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),
    NestedFile = filename:join(SubDir, "nested.lfe"),
    test_utils:write_file(NestedFile,
        "(defmodule myapp.nested)\n"
        "(defun test () 'nested-ok)\n"),

    %% Discover and prepare
    AllFiles = r3lfe_package:discover_files(SrcDir),
    ?assertEqual(2, length(AllFiles)),

    {ok, PackageInfos} = r3lfe_package:prepare_packages(AllFiles),

    try
        %% Compile all files (flat + temp package files)
        FilesToCompile = AllFiles ++ [maps:get(temp_file, Info)
                                      || Info <- PackageInfos],

        %% Remove duplicates and filter out non-package sources
        UniqueFiles = lists:usort([
            case r3lfe_package:is_package_file(F) of
                true ->
                    %% Use temp file for package
                    case lists:keyfind(F, 1,
                         [{maps:get(source_file, I), maps:get(temp_file, I)}
                          || I <- PackageInfos]) of
                        {_, TempFile} -> TempFile;
                        false -> F
                    end;
                false -> F
            end || F <- FilesToCompile
        ]),

        Results = [r3lfe_compile_worker:compile_file(F, EbinDir, [])
                   || F <- UniqueFiles],

        %% All should succeed
        ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

        %% Check beam files
        ?assert(filelib:is_file(filename:join(EbinDir, "flat.beam"))),
        ?assert(filelib:is_file(filename:join(EbinDir, "myapp.nested.beam"))),

        ok
    after
        r3lfe_package:cleanup_packages(PackageInfos)
    end.

nested_includes_work(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    IncludeDir = filename:join(TestDir, "include"),
    EbinDir = filename:join(TestDir, "ebin"),

    ok = filelib:ensure_dir(filename:join(IncludeDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create header
    HeaderFile = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord person name)\n"),

    %% Create nested module using header
    SubDir = filename:join(SrcDir, "myapp"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    NestedFile = filename:join(SubDir, "useheader.lfe"),
    test_utils:write_file(NestedFile,
        "(defmodule myapp.useheader)\n"
        "(include-file \"../../include/records.lfe\")\n"
        "(defun make () (make-person name 'test))\n"),

    %% Prepare and compile
    {ok, PackageInfos} = r3lfe_package:prepare_packages([NestedFile]),

    try
        #{temp_file := TempFile} = hd(PackageInfos),

        Opts = [{i, IncludeDir}],
        Result = r3lfe_compile_worker:compile_file(TempFile, EbinDir, Opts),

        ?assertEqual(ok, Result),
        ?assert(filelib:is_file(
            filename:join(EbinDir, "myapp.useheader.beam")
        )),

        ok
    after
        r3lfe_package:cleanup_packages(PackageInfos)
    end.

package_errors_reported(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create package file with syntax error
    SubDir = filename:join(SrcDir, "broken"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    BrokenFile = filename:join(SubDir, "syntax.lfe"),
    test_utils:write_file(BrokenFile,
        "(defmodule broken.syntax)\n"
        "(defun bad (  )\n"),  % Syntax error

    {ok, PackageInfos} = r3lfe_package:prepare_packages([BrokenFile]),

    ?assertEqual(1, length(PackageInfos)),

    %% Clean up
    r3lfe_package:cleanup_packages(PackageInfos),

    ok.

cleanup_on_compilation_error(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create package file with error
    SubDir = filename:join(SrcDir, "pkg"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    ErrorFile = filename:join(SubDir, "error.lfe"),
    test_utils:write_file(ErrorFile,
        "(defmodule pkg.error)\n"
        "(defun bad ()\n"),  % Incomplete

    {ok, PackageInfos} = r3lfe_package:prepare_packages([ErrorFile]),

    #{temp_file := TempFile} = hd(PackageInfos),

    %% Verify temp file exists before compilation
    ?assert(filelib:is_file(TempFile)),

    try
        %% Compile (will fail)
        _Result = r3lfe_compile_worker:compile_file(TempFile, EbinDir, []),
        ok
    catch
        _:_ -> ok
    after
        %% Clean up
        r3lfe_package:cleanup_packages(PackageInfos)
    end,

    %% Verify temp file cleaned up even after error
    ?assertNot(filelib:is_file(TempFile)),

    ok.

concurrent_compilation_safe(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),

    %% Create multiple package files
    Files = lists:map(
        fun(N) ->
            SubDir = filename:join([SrcDir, "pkg" ++ integer_to_list(N)]),
            ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

            File = filename:join(SubDir, "module.lfe"),
            Content = io_lib:format("(defmodule pkg~p.module)\n", [N]),
            test_utils:write_file(File, Content),
            File
        end,
        lists:seq(1, 10)
    ),

    %% Prepare packages
    {ok, PackageInfos} = r3lfe_package:prepare_packages(Files),

    try
        %% Get all temp files
        TempFiles = [maps:get(temp_file, Info) || Info <- PackageInfos],

        %% Verify no name collisions
        ?assertEqual(length(TempFiles), length(lists:usort(TempFiles))),

        %% Verify all temp files exist
        lists:foreach(
            fun(TF) -> ?assert(filelib:is_file(TF)) end,
            TempFiles
        ),

        ok
    after
        r3lfe_package:cleanup_packages(PackageInfos)
    end.
```

## Testing Instructions

### Running Tests

```bash
# Run all package tests
rebar3 ct --suite=test/r3lfe_package_SUITE

# Run integration tests
rebar3 ct --suite=test/package_integration_SUITE

# Run all tests
rebar3 ct

# With coverage
rebar3 as test do ct, cover
```

### Manual Verification

```bash
# Create test project with packages
mkdir test_packages
cd test_packages

cat > rebar.config <<EOF
{plugins, [{r3lfe, "0.5.0"}]}.
{deps, [{lfe, "2.2.0"}]}.
EOF

# Create flat and nested structure
mkdir -p src/myapp/utils

cat > src/simple.lfe <<EOF
(defmodule simple)
(defun hello () 'world)
EOF

cat > src/myapp/core.lfe <<EOF
(defmodule myapp.core)
(defun start () 'ok)
EOF

cat > src/myapp/utils/helpers.lfe <<EOF
(defmodule myapp.utils.helpers)
(defun format (x) x)
EOF

# Compile
rebar3 compile

# Check generated beams
ls -la ebin/
# Should see:
# simple.beam
# myapp.core.beam
# myapp.utils.helpers.beam
```

## Expected Outcomes

At the end of Phase 4, you should have:

1. ✅ Robust package discovery for nested structures
2. ✅ Safe temporary file creation with unique names
3. ✅ Proper cleanup even on errors
4. ✅ Package tracking system
5. ✅ Module name validation
6. ✅ All previous functionality working
7. ✅ Comprehensive test coverage (>90%)

### Integration Checklist

- [ ] All previous tests pass
- [ ] Package files discovered correctly
- [ ] Nested modules compile with correct names
- [ ] Temporary files cleaned up after compilation
- [ ] Cleanup happens even on compilation errors
- [ ] No race conditions with concurrent packages
- [ ] Module name validation prevents invalid names
- [ ] Mixed flat/nested structures work
- [ ] Packages with includes work correctly
- [ ] Dialyzer clean
- [ ] Code coverage > 90%

## Next Steps

Phase 5 will implement:

- Complete provider layer (all rebar3 commands)
- REPL provider with proper setup
- Test runner integration
- Release and escript support
- Clean command implementations

## Notes for Claude Code

- Always use `try...after` for cleanup to ensure temp files removed
- Package module names must use dots, never slashes
- Validate module names to prevent filesystem issues
- Track all temporary files in ETS for emergency cleanup
- Handle Windows path separators correctly (normalize to forward slash)
- Test cleanup on both successful and failed compilations
- Ensure no orphaned temp files remain after crashes
- Module name calculation must be deterministic and reversible
