# Phase 2: Dependency Scanner & DAG Integration

## Overview

This phase implements the core solution to the header dependency bug identified in the audit. We'll create a robust dependency scanner that parses LFE source files to extract include directives, integrates with rebar3's DAG (Directed Acyclic Graph) system, and implements proper incremental compilation based on file dependencies.

**Goal**: Eliminate silent compilation bugs caused by stale header files through proper dependency tracking.

## Prerequisites

- Phase 1 completed and all tests passing
- Understanding of LFE's `include-file` and `include-lib` syntax
- Familiarity with rebar3's DAG-based compilation system
- Knowledge of Erlang's `digraph` module

## Architecture Overview

```
Dependency Tracking Flow:

1. rebar3 discovers .lfe files
2. r3lfe_dependency_scanner:scan_file/2 → parses each file
3. Extract include-file and include-lib directives
4. Resolve paths to absolute filesystem locations
5. Return list of header dependencies
6. rebar3 builds DAG with files as vertices, includes as edges
7. r3lfe_compiler_mod:needed_files/4 → uses DAG to find stale files
8. Only recompile files whose source or headers changed
```

## Implementation Tasks

### Task 2.1: Implement Dependency Scanner

**File: `src/r3lfe_dependency_scanner.erl`**

```erlang
-module(r3lfe_dependency_scanner).

%% API exports
-export([
    scan_file/2,
    scan_file/3,
    scan_content/1,
    resolve_include/2,
    resolve_include/3
]).

%% For testing
-export([
    parse_include_forms/1,
    extract_include_path/1,
    classify_include/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-type include_type() :: include_file | include_lib.
-type include_form() :: {include_type(), string()}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Scan an LFE source file for dependencies
%% Returns list of absolute paths to header files this source depends on
-spec scan_file(file:filename(), rebar_app_info:t()) -> [file:filename()].
scan_file(SourceFile, AppInfo) ->
    scan_file(SourceFile, AppInfo, #{}).

%% @doc Scan with options
%% Options:
%%   - cache => true/false (future: cache parsed results)
%%   - include_dirs => [Dir] (additional include directories)
-spec scan_file(file:filename(), rebar_app_info:t(), map()) -> [file:filename()].
scan_file(SourceFile, AppInfo, Opts) ->
    case file:read_file(SourceFile) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            IncludeForms = scan_content(Content),

            %% Resolve each include form to an absolute path
            AppDir = rebar_app_info:dir(AppInfo),
            IncludeDirs = maps:get(include_dirs, Opts,
                                   r3lfe_config:get_include_dirs(AppInfo)),

            ResolvedPaths = lists:filtermap(
                fun(Form) ->
                    case resolve_include(Form, AppDir, IncludeDirs) of
                        {ok, Path} -> {true, Path};
                        {error, Reason} ->
                            ?WARN("Could not resolve include in ~s: ~p",
                                  [SourceFile, Reason]),
                            false
                    end
                end,
                IncludeForms
            ),

            %% Remove duplicates
            lists:usort(ResolvedPaths);

        {error, Reason} ->
            ?ERROR("Failed to read ~s: ~p", [SourceFile, Reason]),
            []
    end.

%% @doc Scan content string for include forms
-spec scan_content(string()) -> [include_form()].
scan_content(Content) ->
    Forms = parse_include_forms(Content),
    lists:filtermap(
        fun(RawForm) ->
            case extract_include_path(RawForm) of
                {ok, Type, Path} -> {true, {Type, Path}};
                error -> false
            end
        end,
        Forms
    ).

%% @doc Resolve an include form to an absolute path
-spec resolve_include(include_form(), file:filename()) ->
    {ok, file:filename()} | {error, term()}.
resolve_include(Form, AppDir) ->
    IncludeDir = filename:join(AppDir, ?DEFAULT_INCLUDE_DIR),
    resolve_include(Form, AppDir, [IncludeDir]).

-spec resolve_include(include_form(), file:filename(), [file:filename()]) ->
    {ok, file:filename()} | {error, term()}.
resolve_include({include_file, Path}, AppDir, IncludeDirs) ->
    resolve_include_file(Path, AppDir, IncludeDirs);
resolve_include({include_lib, Path}, _AppDir, _IncludeDirs) ->
    resolve_include_lib(Path).

%%====================================================================
%% Internal functions - Parsing
%%====================================================================

%% @doc Parse LFE content for include forms
%% Matches: (include-file "path") and (include-lib "app/path")
-spec parse_include_forms(string()) -> [string()].
parse_include_forms(Content) ->
    %% Regular expression to match include forms
    %% Captures the entire form: (include-file "...") or (include-lib "...")
    Pattern = "\\(include-(?:file|lib)\\s+\"[^\"]+\"\\)",

    case re:run(Content, Pattern, [global, {capture, all, list}]) of
        {match, Matches} ->
            %% Matches is [[FullMatch], [FullMatch], ...]
            [Match || [Match] <- Matches];
        nomatch ->
            []
    end.

%% @doc Extract include type and path from a matched form
-spec extract_include_path(string()) -> {ok, include_type(), string()} | error.
extract_include_path(Form) ->
    %% Form looks like: (include-file "path/to/file.lfe")
    %% or: (include-lib "app/include/file.lfe")

    %% Extract the type and path
    TypePattern = "\\(include-(file|lib)\\s+\"([^\"]+)\"\\)",

    case re:run(Form, TypePattern, [{capture, all_but_first, list}]) of
        {match, ["file", Path]} ->
            {ok, include_file, Path};
        {match, ["lib", Path]} ->
            {ok, include_lib, Path};
        _ ->
            error
    end.

%% @doc Classify an include path (for diagnostics/logging)
-spec classify_include(string()) -> include_type().
classify_include(Path) ->
    case string:find(Path, "/") of
        nomatch -> include_file;
        _ ->
            %% If it looks like "app/..." it's probably include-lib
            case re:run(Path, "^[a-z_]+/", [{capture, none}]) of
                match -> include_lib;
                nomatch -> include_file
            end
    end.

%%====================================================================
%% Internal functions - Resolution
%%====================================================================

%% @doc Resolve an include-file path
%% Searches in:
%% 1. Include directories (in order)
%% 2. Relative to app directory
%% 3. As absolute path (if given)
-spec resolve_include_file(string(), file:filename(), [file:filename()]) ->
    {ok, file:filename()} | {error, not_found}.
resolve_include_file(Path, AppDir, IncludeDirs) ->
    %% Build list of candidate paths to check
    Candidates = [
        %% Try include directories first
        [filename:join(IncDir, Path) || IncDir <- IncludeDirs],
        %% Try relative to app directory
        [filename:join(AppDir, Path)],
        %% Try as absolute path
        [Path]
    ],

    AllCandidates = lists:flatten(Candidates),

    case find_existing_file(AllCandidates) of
        {ok, Found} ->
            {ok, filename:absname(Found)};
        error ->
            ?DEBUG("Could not find include-file: ~s", [Path]),
            ?DEBUG("Searched in: ~p", [AllCandidates]),
            {error, not_found}
    end.

%% @doc Resolve an include-lib path
%% Format: "app/include/file.lfe" or "app/path/to/file.lfe"
%% Resolves using code:lib_dir/1
-spec resolve_include_lib(string()) -> {ok, file:filename()} | {error, term()}.
resolve_include_lib(Path) ->
    case string:split(Path, "/", leading) of
        [AppName, RestPath] ->
            %% Convert app name string to atom
            try list_to_existing_atom(AppName) of
                AppAtom ->
                    case code:lib_dir(AppAtom) of
                        {error, bad_name} ->
                            ?DEBUG("Application not found: ~s", [AppName]),
                            {error, {app_not_found, AppName}};
                        AppDir ->
                            FullPath = filename:join(AppDir, RestPath),
                            case filelib:is_file(FullPath) of
                                true ->
                                    {ok, filename:absname(FullPath)};
                                false ->
                                    ?DEBUG("File not found: ~s", [FullPath]),
                                    {error, not_found}
                            end
                    end
            catch
                error:badarg ->
                    %% App name not loaded as atom yet
                    ?DEBUG("Application name not loaded: ~s", [AppName]),
                    {error, {app_not_loaded, AppName}}
            end;
        _ ->
            ?ERROR("Invalid include-lib path format: ~s", [Path]),
            {error, invalid_format}
    end.

%% @doc Find the first existing file in a list of candidates
-spec find_existing_file([file:filename()]) -> {ok, file:filename()} | error.
find_existing_file([]) ->
    error;
find_existing_file([Path | Rest]) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> find_existing_file(Rest)
    end.
```

### Task 2.2: Update Compiler Module with DAG Integration

**File: `src/r3lfe_compiler_mod.erl` (UPDATE)**

Replace the stub implementations from Phase 1 with full DAG integration:

```erlang
-module(r3lfe_compiler_mod).
-behaviour(rebar_compiler).

%% rebar_compiler callbacks
-export([
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2
]).

-include("rebar3_lfe/include/r3lfe.hrl").

%%====================================================================
%% rebar_compiler callbacks
%%====================================================================

%% @doc Define the compiler context
-spec context(rebar_app_info:t()) -> #{
    src_dirs => [file:filename()],
    include_dirs => [file:filename()],
    src_ext => string(),
    out_mappings => [{string(), file:filename()}]
}.
context(AppInfo) ->
    SrcDirs = r3lfe_config:get_src_dirs(AppInfo),
    IncludeDirs = r3lfe_config:get_include_dirs(AppInfo),
    OutDir = r3lfe_config:get_out_dir(AppInfo),

    ?DEBUG("Compiler context for ~s:", [rebar_app_info:name(AppInfo)]),
    ?DEBUG("  Source dirs: ~p", [SrcDirs]),
    ?DEBUG("  Include dirs: ~p", [IncludeDirs]),
    ?DEBUG("  Output dir: ~s", [OutDir]),

    #{
        src_dirs => SrcDirs,
        include_dirs => IncludeDirs,
        src_ext => ?LFE_SRC_EXTENSION,
        out_mappings => [{?BEAM_EXTENSION, OutDir}],
        dependencies_opts => #{app_info => AppInfo}
    }.

%% @doc Extract dependencies from a source file
%% This is called by rebar3's DAG builder for each source file
-spec dependencies(file:filename(), file:filename(), rebar_app_info:t()) ->
    [file:filename()].
dependencies(Source, _SourceDir, AppInfo) ->
    ?DEBUG("Scanning dependencies for: ~s", [Source]),

    %% Use our dependency scanner
    Deps = r3lfe_dependency_scanner:scan_file(Source, AppInfo),

    ?DEBUG("Found ~p dependencies for ~s", [length(Deps), Source]),

    Deps.

%% @doc Determine which files need compilation
%% This uses the DAG (Directed Acyclic Graph) built by rebar3 to determine
%% which files are stale based on timestamps of sources and their dependencies
-spec needed_files(
    rebar_digraph:t(),
    [file:filename()],
    [{string(), file:filename()}],
    rebar_app_info:t()
) -> {{[file:filename()], term()}, {{[file:filename()], [file:filename()]}, term()}}.
needed_files(G, FoundFiles, OutMappings, AppInfo) ->
    ?DEBUG("Checking which files need compilation...", []),

    %% Get first files that must be compiled before others
    FirstFiles = r3lfe_config:get_first_files(AppInfo),

    %% Use the DAG to determine which files need compilation
    %% A file needs compilation if:
    %% 1. Its .beam doesn't exist
    %% 2. Its source is newer than .beam
    %% 3. Any of its dependencies (headers) are newer than .beam

    NeededFiles = lists:filter(
        fun(Source) ->
            needs_compilation(G, Source, OutMappings)
        end,
        FoundFiles
    ),

    ?DEBUG("Files needing compilation: ~p", [length(NeededFiles)]),

    %% Separate first files from regular files
    {FirstNeeded, RestNeeded} = lists:partition(
        fun(File) -> lists:member(File, FirstFiles) end,
        NeededFiles
    ),

    %% Return format: {{Sequential, Opts}, {{Sequential, Parallel}, Opts}}
    %% FirstNeeded must be compiled sequentially first
    %% RestNeeded can potentially be compiled in parallel (Phase 3)
    {{FirstNeeded, []}, {{[], RestNeeded}, []}}.

%% @doc Compile a source file (stub for Phase 3)
-spec compile(file:filename(), [{string(), file:filename()}],
              rebar_dict:t(), list()) -> ok.
compile(Source, _OutMappings, _Dict, _Opts) ->
    ?INFO("Would compile: ~s", [Source]),
    %% Phase 3 will implement actual compilation
    ok.

%% @doc Clean compiled files
-spec clean([file:filename()], rebar_app_info:t()) -> ok.
clean(Files, _AppInfo) ->
    lists:foreach(
        fun(File) ->
            case file:delete(File) of
                ok ->
                    ?DEBUG("Deleted: ~s", [File]),
                    ok;
                {error, enoent} ->
                    ok;
                {error, Reason} ->
                    ?WARN("Failed to delete ~s: ~p", [File, Reason])
            end
        end,
        Files
    ),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Check if a source file needs compilation based on DAG
-spec needs_compilation(
    rebar_digraph:t(),
    file:filename(),
    [{string(), file:filename()}]
) -> boolean().
needs_compilation(G, Source, OutMappings) ->
    %% Determine target file path
    Target = source_to_target(Source, OutMappings),

    case filelib:last_modified(Target) of
        0 ->
            %% Target doesn't exist, must compile
            ?DEBUG("~s needs compilation: target does not exist", [Source]),
            true;
        TargetTime ->
            %% Check if source is newer
            SourceTime = filelib:last_modified(Source),

            if
                SourceTime > TargetTime ->
                    ?DEBUG("~s needs compilation: source is newer", [Source]),
                    true;
                true ->
                    %% Check if any dependencies are newer
                    check_dependencies_newer(G, Source, TargetTime)
            end
    end.

%% @doc Check if any dependencies are newer than the target
-spec check_dependencies_newer(
    rebar_digraph:t(),
    file:filename(),
    file:date_time()
) -> boolean().
check_dependencies_newer(G, Source, TargetTime) ->
    %% Get all dependencies from the DAG
    %% The DAG has vertices for all files (sources and headers)
    %% and edges representing dependencies

    case digraph:vertex(G, Source) of
        false ->
            %% Source not in graph (shouldn't happen)
            ?WARN("Source ~s not found in dependency graph", [Source]),
            false;
        {Source, _Label} ->
            %% Get outgoing edges (dependencies)
            OutEdges = digraph:out_edges(G, Source),

            %% Check each dependency
            lists:any(
                fun(Edge) ->
                    {_Edge, _From, Dependency, _Label} = digraph:edge(G, Edge),

                    case filelib:last_modified(Dependency) of
                        0 ->
                            %% Dependency doesn't exist - trigger compilation
                            %% so we get proper error message
                            ?DEBUG("~s needs compilation: dependency ~s not found",
                                   [Source, Dependency]),
                            true;
                        DepTime when DepTime > TargetTime ->
                            ?DEBUG("~s needs compilation: dependency ~s is newer",
                                   [Source, Dependency]),
                            true;
                        _ ->
                            false
                    end
                end,
                OutEdges
            )
    end.

%% @doc Convert source file path to target (beam) file path
-spec source_to_target(
    file:filename(),
    [{string(), file:filename()}]
) -> file:filename().
source_to_target(Source, OutMappings) ->
    %% OutMappings is [{Extension, OutputDir}, ...]
    %% For LFE, it's [{".beam", "path/to/ebin"}]

    case OutMappings of
        [{_Ext, OutDir} | _] ->
            BaseName = filename:basename(Source, ?LFE_SRC_EXTENSION),
            filename:join(OutDir, BaseName ++ ?BEAM_EXTENSION);
        [] ->
            %% Shouldn't happen, but have a fallback
            filename:rootname(Source) ++ ?BEAM_EXTENSION
    end.
```

### Task 2.3: Add Dependency Caching Support

**File: `src/r3lfe_dep_cache.erl`**

```erlang
-module(r3lfe_dep_cache).

%% API exports
-export([
    init/0,
    get/1,
    put/2,
    invalidate/1,
    clear/0
]).

-include("rebar3_lfe/include/r3lfe.hrl").

%% ETS table name
-define(CACHE_TABLE, r3lfe_dep_cache).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize the dependency cache
-spec init() -> ok.
init() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            ?DEBUG("Creating dependency cache table", []),
            _Tid = ets:new(?CACHE_TABLE, [
                named_table,
                public,
                set,
                {read_concurrency, true}
            ]),
            ok;
        _ ->
            %% Already initialized
            ok
    end.

%% @doc Get cached dependencies for a file
%% Returns {ok, Deps, Timestamp} if cached and still valid
%% Returns error if not cached or cache is stale
-spec get(file:filename()) ->
    {ok, [file:filename()], file:date_time()} | error.
get(SourceFile) ->
    case ets:lookup(?CACHE_TABLE, SourceFile) of
        [] ->
            error;
        [{SourceFile, Deps, CachedTime}] ->
            %% Check if source file has been modified since caching
            CurrentTime = filelib:last_modified(SourceFile),

            if
                CurrentTime =< CachedTime ->
                    {ok, Deps, CachedTime};
                true ->
                    %% Cache is stale
                    ?DEBUG("Cache stale for ~s", [SourceFile]),
                    error
            end
    end.

%% @doc Cache dependencies for a file
-spec put(file:filename(), [file:filename()]) -> ok.
put(SourceFile, Deps) ->
    Timestamp = filelib:last_modified(SourceFile),

    true = ets:insert(?CACHE_TABLE, {SourceFile, Deps, Timestamp}),

    ?DEBUG("Cached ~p dependencies for ~s", [length(Deps), SourceFile]),
    ok.

%% @doc Invalidate cache entry for a specific file
-spec invalidate(file:filename()) -> ok.
invalidate(SourceFile) ->
    true = ets:delete(?CACHE_TABLE, SourceFile),
    ok.

%% @doc Clear entire cache
-spec clear() -> ok.
clear() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            ok;
        _ ->
            true = ets:delete_all_objects(?CACHE_TABLE),
            ok
    end.
```

### Task 2.4: Add Caching to Scanner

**File: `src/r3lfe_dependency_scanner.erl` (UPDATE)**

Add caching support to the scanner:

```erlang
%% Add to scan_file/3 function, before reading the file:

scan_file(SourceFile, AppInfo, Opts) ->
    UseCache = maps:get(cache, Opts, true),

    %% Check cache first
    case UseCache andalso r3lfe_dep_cache:get(SourceFile) of
        {ok, CachedDeps, _Time} ->
            ?DEBUG("Using cached dependencies for ~s", [SourceFile]),
            CachedDeps;
        _ ->
            %% Cache miss or disabled, scan the file
            case file:read_file(SourceFile) of
                {ok, Binary} ->
                    Content = binary_to_list(Binary),
                    IncludeForms = scan_content(Content),

                    AppDir = rebar_app_info:dir(AppInfo),
                    IncludeDirs = maps:get(include_dirs, Opts,
                                           r3lfe_config:get_include_dirs(AppInfo)),

                    ResolvedPaths = lists:filtermap(
                        fun(Form) ->
                            case resolve_include(Form, AppDir, IncludeDirs) of
                                {ok, Path} -> {true, Path};
                                {error, Reason} ->
                                    ?WARN("Could not resolve include in ~s: ~p",
                                          [SourceFile, Reason]),
                                    false
                            end
                        end,
                        IncludeForms
                    ),

                    Result = lists:usort(ResolvedPaths),

                    %% Cache the result
                    UseCache andalso r3lfe_dep_cache:put(SourceFile, Result),

                    Result;

                {error, Reason} ->
                    ?ERROR("Failed to read ~s: ~p", [SourceFile, Reason]),
                    []
            end
    end.
```

### Task 2.5: Initialize Cache in Plugin Entry

**File: `src/r3lfe.erl` (UPDATE)**

```erlang
init(State) ->
    ?DEBUG("Initializing r3lfe plugin...", []),

    %% Initialize dependency cache
    ok = r3lfe_dep_cache:init(),

    %% Register our compiler module with rebar3
    State1 = rebar_state:append_compilers(State, [r3lfe_compiler_mod]),

    ?DEBUG("Registered r3lfe_compiler_mod with rebar3", []),

    {ok, State1}.
```

### Task 2.6: Create Dependency Scanner Tests

**File: `test/r3lfe_dependency_scanner_SUITE.erl`**

```erlang
-module(r3lfe_dependency_scanner_SUITE).

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
    parse_include_forms_single/1,
    parse_include_forms_multiple/1,
    parse_include_forms_none/1,
    extract_include_path_file/1,
    extract_include_path_lib/1,
    scan_content_mixed/1,
    resolve_include_file_in_include_dir/1,
    resolve_include_file_relative/1,
    resolve_include_lib_found/1,
    resolve_include_lib_not_found/1,
    scan_file_with_includes/1,
    scan_file_no_includes/1,
    scan_file_missing_dependency/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        parse_include_forms_single,
        parse_include_forms_multiple,
        parse_include_forms_none,
        extract_include_path_file,
        extract_include_path_lib,
        scan_content_mixed,
        resolve_include_file_in_include_dir,
        resolve_include_file_relative,
        resolve_include_lib_found,
        resolve_include_lib_not_found,
        scan_file_with_includes,
        scan_file_no_includes,
        scan_file_missing_dependency
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
%% Test Cases - Parsing
%%====================================================================

parse_include_forms_single(_Config) ->
    Content = "(defmodule test)\n(include-file \"header.lfe\")\n",

    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),

    ?assertEqual(1, length(Forms)),
    ?assertMatch([_], Forms),
    ok.

parse_include_forms_multiple(_Config) ->
    Content =
        "(defmodule test)\n"
        "(include-file \"header1.lfe\")\n"
        "(include-lib \"app/include/header2.lfe\")\n"
        "(include-file \"header3.lfe\")\n",

    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),

    ?assertEqual(3, length(Forms)),
    ok.

parse_include_forms_none(_Config) ->
    Content = "(defmodule test)\n(defun hello () 'world)\n",

    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),

    ?assertEqual(0, length(Forms)),
    ok.

extract_include_path_file(_Config) ->
    Form = "(include-file \"records.lfe\")",

    Result = r3lfe_dependency_scanner:extract_include_path(Form),

    ?assertMatch({ok, include_file, "records.lfe"}, Result),
    ok.

extract_include_path_lib(_Config) ->
    Form = "(include-lib \"lfe/include/clj.lfe\")",

    Result = r3lfe_dependency_scanner:extract_include_path(Form),

    ?assertMatch({ok, include_lib, "lfe/include/clj.lfe"}, Result),
    ok.

scan_content_mixed(_Config) ->
    Content =
        "(defmodule test)\n"
        "(include-file \"local.lfe\")\n"
        "(include-lib \"lfe/include/clj.lfe\")\n"
        "(defun test () 'ok)\n",

    Forms = r3lfe_dependency_scanner:scan_content(Content),

    ?assertEqual(2, length(Forms)),
    ?assert(lists:member({include_file, "local.lfe"}, Forms)),
    ?assert(lists:member({include_lib, "lfe/include/clj.lfe"}, Forms)),
    ok.

%%====================================================================
%% Test Cases - Resolution
%%====================================================================

resolve_include_file_in_include_dir(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create a header file
    HeaderFile = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord person name age)\n"),

    %% Resolve the include
    Result = r3lfe_dependency_scanner:resolve_include(
        {include_file, "records.lfe"},
        AppDir,
        [IncludeDir]
    ),

    ?assertMatch({ok, _Path}, Result),
    {ok, ResolvedPath} = Result,
    ?assert(filelib:is_file(ResolvedPath)),
    ok.

resolve_include_file_relative(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create a header file in src directory
    HeaderFile = filename:join(SrcDir, "local.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord data value)\n"),

    %% Resolve with relative path
    Result = r3lfe_dependency_scanner:resolve_include(
        {include_file, "src/local.lfe"},
        AppDir,
        []
    ),

    ?assertMatch({ok, _Path}, Result),
    ok.

resolve_include_lib_found(_Config) ->
    %% This tests resolution of a real application (lfe)
    %% which should be available in the test environment

    Result = r3lfe_dependency_scanner:resolve_include(
        {include_lib, "lfe/include/clj.lfe"},
        "/tmp",  % AppDir doesn't matter for include-lib
        []
    ),

    %% This might fail if LFE isn't on the code path, which is ok
    case Result of
        {ok, Path} ->
            ?assert(filelib:is_file(Path)),
            ok;
        {error, _} ->
            ct:pal("Note: LFE not on code path, skipping include-lib test"),
            ok
    end.

resolve_include_lib_not_found(_Config) ->
    Result = r3lfe_dependency_scanner:resolve_include(
        {include_lib, "nonexistent_app/include/file.lfe"},
        "/tmp",
        []
    ),

    ?assertMatch({error, _}, Result),
    ok.

%%====================================================================
%% Test Cases - Full Scanning
%%====================================================================

scan_file_with_includes(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create header files
    Header1 = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(Header1, "(defrecord person name age)\n"),

    Header2 = filename:join(IncludeDir, "macros.lfe"),
    test_utils:write_file(Header2, "(defmacro debug (x) `(io:format \"~p\" ,x))\n"),

    %% Create source file with includes
    SourceFile = filename:join(SrcDir, "test_module.lfe"),
    SourceContent =
        "(defmodule test-module)\n"
        "(include-file \"records.lfe\")\n"
        "(include-file \"macros.lfe\")\n"
        "(defun test () 'ok)\n",
    test_utils:write_file(SourceFile, SourceContent),

    %% Create mock AppInfo
    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),

    %% Scan the file
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    ?assertEqual(2, length(Deps)),
    ?assert(lists:any(fun(P) -> filename:basename(P) =:= "records.lfe" end, Deps)),
    ?assert(lists:any(fun(P) -> filename:basename(P) =:= "macros.lfe" end, Deps)),
    ok.

scan_file_no_includes(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create source file without includes
    SourceFile = filename:join(SrcDir, "simple.lfe"),
    SourceContent =
        "(defmodule simple)\n"
        "(defun hello () 'world)\n",
    test_utils:write_file(SourceFile, SourceContent),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),

    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    ?assertEqual(0, length(Deps)),
    ok.

scan_file_missing_dependency(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create source file that references non-existent header
    SourceFile = filename:join(SrcDir, "broken.lfe"),
    SourceContent =
        "(defmodule broken)\n"
        "(include-file \"nonexistent.lfe\")\n"
        "(defun test () 'ok)\n",
    test_utils:write_file(SourceFile, SourceContent),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),

    %% Should return empty list (with warnings logged)
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    ?assertEqual(0, length(Deps)),
    ok.
```

### Task 2.7: Create Integration Tests

**File: `test/integration_SUITE.erl`**

```erlang
-module(integration_SUITE).

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
    header_change_triggers_recompile/1,
    multiple_headers_tracked/1,
    nested_includes_tracked/1,
    missing_header_reported/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        header_change_triggers_recompile,
        multiple_headers_tracked,
        nested_includes_tracked,
        missing_header_reported
    ].

init_per_suite(Config) ->
    %% Initialize dependency cache
    r3lfe_dep_cache:init(),
    Config.

end_per_suite(_Config) ->
    r3lfe_dep_cache:clear(),
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    r3lfe_dep_cache:clear(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

header_change_triggers_recompile(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create header file
    HeaderFile = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord person name)\n"),

    %% Create source file
    SourceFile = filename:join(SrcDir, "mymodule.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule mymodule)\n"
        "(include-file \"records.lfe\")\n"
        "(defun test () 'ok)\n"),

    %% Create mock target (beam file)
    EbinDir = maps:get(ebin_dir, AppData),
    TargetFile = filename:join(EbinDir, "mymodule.beam"),
    test_utils:write_file(TargetFile, <<>>),

    %% Wait to ensure timestamp difference
    timer:sleep(1000),

    %% Scan dependencies
    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    %% Verify header is tracked
    ?assert(lists:any(
        fun(D) -> filename:basename(D) =:= "records.lfe" end,
        Deps
    )),

    %% Modify header file
    test_utils:write_file(HeaderFile,
        "(defrecord person name age)\n"),

    %% Create a mock DAG
    G = digraph:new([acyclic]),
    digraph:add_vertex(G, SourceFile),
    digraph:add_vertex(G, HeaderFile),
    digraph:add_edge(G, SourceFile, HeaderFile),

    %% Check if recompilation needed
    OutMappings = [{".beam", EbinDir}],
    NeedsCompile = r3lfe_compiler_mod:needs_compilation(
        G, SourceFile, OutMappings
    ),

    ?assert(NeedsCompile, "Should need recompilation after header change"),

    digraph:delete(G),
    ok.

multiple_headers_tracked(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create multiple header files
    Header1 = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(Header1, "(defrecord person name)\n"),

    Header2 = filename:join(IncludeDir, "macros.lfe"),
    test_utils:write_file(Header2, "(defmacro debug (x) x)\n"),

    Header3 = filename:join(IncludeDir, "types.lfe"),
    test_utils:write_file(Header3, "(deftype string-list (list string))\n"),

    %% Create source file including all headers
    SourceFile = filename:join(SrcDir, "complex.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule complex)\n"
        "(include-file \"records.lfe\")\n"
        "(include-file \"macros.lfe\")\n"
        "(include-file \"types.lfe\")\n"
        "(defun test () 'ok)\n"),

    %% Scan dependencies
    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    %% Verify all three headers tracked
    ?assertEqual(3, length(Deps)),
    ?assert(lists:any(fun(D) -> filename:basename(D) =:= "records.lfe" end, Deps)),
    ?assert(lists:any(fun(D) -> filename:basename(D) =:= "macros.lfe" end, Deps)),
    ?assert(lists:any(fun(D) -> filename:basename(D) =:= "types.lfe" end, Deps)),
    ok.

nested_includes_tracked(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create base header
    BaseHeader = filename:join(IncludeDir, "base.lfe"),
    test_utils:write_file(BaseHeader, "(defrecord base id)\n"),

    %% Create derived header that includes base
    DerivedHeader = filename:join(IncludeDir, "derived.lfe"),
    test_utils:write_file(DerivedHeader,
        "(include-file \"base.lfe\")\n"
        "(defrecord derived (base) extra)\n"),

    %% Create source that includes derived
    SourceFile = filename:join(SrcDir, "nested.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule nested)\n"
        "(include-file \"derived.lfe\")\n"
        "(defun test () 'ok)\n"),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),

    %% Scan source file - should find derived.lfe
    SourceDeps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),
    ?assert(lists:any(
        fun(D) -> filename:basename(D) =:= "derived.lfe" end,
        SourceDeps
    )),

    %% Scan derived header - should find base.lfe
    DerivedDeps = r3lfe_dependency_scanner:scan_file(DerivedHeader, AppInfo),
    ?assert(lists:any(
        fun(D) -> filename:basename(D) =:= "base.lfe" end,
        DerivedDeps
    )),

    ct:pal("Note: Transitive dependency tracking (nested.lfe -> base.lfe) "
           "requires full DAG traversal by rebar3"),
    ok.

missing_header_reported(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create source file referencing non-existent header
    SourceFile = filename:join(SrcDir, "broken.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule broken)\n"
        "(include-file \"missing.lfe\")\n"
        "(defun test () 'ok)\n"),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),

    %% Scan should return empty list with warning logged
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    ?assertEqual(0, length(Deps)),
    ok.
```

## Testing Instructions

### Running Tests

```bash
# Run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite=test/r3lfe_dependency_scanner_SUITE

# Run integration tests
rebar3 ct --suite=test/integration_SUITE

# Run with coverage
rebar3 as test do ct, cover

# Check coverage report
open _build/test/cover/index.html
```

### Manual Integration Test

Create a test project to verify dependency tracking:

```bash
# Create test project
mkdir test_project
cd test_project

# Create structure
mkdir -p src include ebin

# Create header file
cat > include/records.lfe <<EOF
(defrecord person
  name
  age)
EOF

# Create source using header
cat > src/myapp.lfe <<EOF
(defmodule myapp)
(include-file "records.lfe")

(defun make-person (name age)
  (make-person name name age age))
EOF

# Create rebar.config
cat > rebar.config <<EOF
{plugins, [{r3lfe, "0.5.0"}]}.
{deps, [{lfe, "2.2.0"}]}.
EOF

# First compile
rebar3 compile

# Modify header
echo "\n; Comment added" >> include/records.lfe

# Recompile - should detect header change
rebar3 compile
# Should show: "Compiling myapp.lfe"
```

## Expected Outcomes

At the end of Phase 2, you should have:

1. ✅ Full dependency scanner parsing include directives
2. ✅ Resolution for both include-file and include-lib
3. ✅ Integration with rebar3's DAG system
4. ✅ Proper incremental compilation based on header changes
5. ✅ Dependency caching for performance
6. ✅ Comprehensive test coverage (>90%)
7. ✅ Manual verification showing header changes trigger recompilation

### Integration Checklist

- [ ] All Phase 1 tests still pass
- [ ] All Phase 2 tests pass
- [ ] Dependency scanner correctly identifies includes
- [ ] Header changes trigger recompilation
- [ ] Unchanged files are not recompiled
- [ ] Cache improves performance on repeat scans
- [ ] Missing headers generate appropriate warnings
- [ ] Dialyzer clean
- [ ] Code coverage > 90%

## Next Steps

Phase 3 will implement:

- Actual compilation worker calling lfe_comp
- Error and warning formatting
- Progress reporting
- Compiler option change detection

## Notes for Claude Code

- The DAG integration is critical - study how rebar3's internal compiler uses digraph
- Be careful with file timestamps - use `filelib:last_modified/1` consistently
- The cache uses ETS - make sure to handle concurrent access properly
- Test both include-file (relative paths) and include-lib (code:lib_dir) resolution
- Remember: include-lib format is "app/path/to/file", not "app:path/to/file"
- Always return absolute paths from resolution functions
- Handle symlinks correctly - use filename:absname/1
