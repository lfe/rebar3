# Phase 1: Project Setup & Core Infrastructure

## Overview

This phase establishes the foundation for the rewritten rebar3_lfe plugin. We're building modern infrastructure that integrates properly with rebar3's Custom Compiler Modules interface, implements proper configuration management, and sets up path handling correctly.

**Goal**: Create a solid, testable foundation that follows 2025 rebar3 best practices.

## Prerequisites

- Erlang/OTP 24+ (support through OTP 28)
- rebar3 3.22+ (3.25 for OTP 28)
- LFE 2.2+
- Understanding of rebar3's Custom Compiler Modules interface

## Architecture Overview

```
rebar3_lfe/
├── include/
│   └── rb3lfe.hrl                    % Shared header
├── src/
│   ├── rebar3_lfe.app.src
│   ├── rb3lfe.erl                    % Plugin entry point
│   ├── rb3lfe_compiler_mod.erl       % rebar_compiler behavior
│   ├── rb3lfe_config.erl             % Configuration management
│   └── rb3lfe_paths.erl              % Path management
├── test/
│   ├── rb3lfe_config_SUITE.erl
│   ├── rb3lfe_paths_SUITE.erl
│   └── test_utils.erl                % Shared test utilities
└── rebar.config
```

## Implementation Tasks

### Task 1.1: Update Project Configuration

**File: `rebar.config`**

```erlang
{erl_opts, [
    debug_info,
    warnings_as_errors,
    warn_export_vars,
    warn_shadow_vars,
    warn_obsolete_guard
]}.

{deps, [
    {lfe, "2.2.0"}
]}.

{project_plugins, [
    rebar3_hex,
    {rebar_oscmd, "0.5.0"}
]}.

{xref_checks, [
    undefined_function_calls,
    undefined_functions,
    locals_not_used,
    deprecated_function_calls,
    deprecated_functions
]}.

{dialyzer, [
    {warnings, [
        unknown,
        unmatched_returns,
        error_handling,
        underspecs
    ]},
    {plt_extra_apps, [lfe]}
]}.

{profiles, [
    {test, [
        {erl_opts, [
            debug_info,
            {d, 'TEST'},
            {src_dirs, ["src", "test"]}
        ]},
        {deps, [
            {proper, "1.5.0"}
        ]},
        {ct_opts, [
            {sys_config, "test/test.config"},
            {verbose, true}
        ]}
    ]},
    {maintainer, [
        {deps, [rebar3_hex]}
    ]}
]}.

{alias, [
    {check, [
        compile,
        xref,
        dialyzer,
        ct,
        {proper, "-c"}
    ]},
    {test, [
        ct,
        {proper, "-c"}
    ]}
]}.
```

**File: `src/rebar3_lfe.app.src`**

```erlang
{application, rebar3_lfe, [
    {description, "The LFE build and software management tool"},
    {vsn, "0.5.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        lfe
    ]},
    {env, []},
    {modules, []},  % Filled automatically

    %% Hex.pm metadata
    {licenses, ["Apache-2.0"]},
    {links, [
        {"GitHub", "https://github.com/lfe/rebar3"},
        {"Hex", "https://hex.pm/packages/rebar3_lfe"},
        {"LFE", "https://lfe.io"}
    ]},
    {files, [
        "src",
        "README.md",
        "LICENSE",
        "rebar.config"
    ]}
]}.
```

### Task 1.2: Create Shared Header

**File: `include/rb3lfe.hrl`**

```erlang
-ifndef(RB3LFE_HRL).
-define(RB3LFE_HRL, true).

%%% Plugin namespace
-define(NAMESPACE, lfe).

%%% File patterns
-define(LFE_SRC_EXTENSION, ".lfe").
-define(BEAM_EXTENSION, ".beam").
-define(LFE_FILE_REGEX, ".*\\.lfe$").

%%% Default directories
-define(DEFAULT_SRC_DIR, "src").
-define(DEFAULT_INCLUDE_DIR, "include").
-define(DEFAULT_OUT_DIR, "ebin").
-define(DEFAULT_TEST_DIR, "test").

%%% Compiler options
-define(DEFAULT_LFE_OPTS, [
    return,
    verbose,
    report_errors,
    report_warnings
]).

%%% Error macros
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

%%% Logging helpers (use rebar_api, not io:format)
-define(DEBUG(Fmt, Args), rebar_api:debug(Fmt, Args)).
-define(INFO(Fmt, Args), rebar_api:info(Fmt, Args)).
-define(WARN(Fmt, Args), rebar_api:warn(Fmt, Args)).
-define(ERROR(Fmt, Args), rebar_api:error(Fmt, Args)).

%%% Type specifications for better dialyzer checks
-type compile_result() :: ok | {ok, [warning()]} | {error, [error()], [warning()]}.
-type error() :: {file:filename(), [{integer(), module(), term()}]}.
-type warning() :: {file:filename(), [{integer(), module(), term()}]}.

-endif.
```

### Task 1.3: Implement Configuration Management

**File: `src/rb3lfe_config.erl`**

```erlang
-module(rb3lfe_config).

%% API exports
-export([
    get_lfe_opts/1,
    get_src_dirs/1,
    get_include_dirs/1,
    get_out_dir/1,
    get_first_files/1,
    is_verbose/1,
    merge_opts/2
]).

%% For testing
-export([
    normalize_src_dirs/2,
    normalize_include_dirs/2
]).

-include("rebar3_lfe/include/rb3lfe.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @doc Get LFE compiler options from rebar state/app config
-spec get_lfe_opts(rebar_app_info:t() | rebar_state:t()) -> [term()].
get_lfe_opts(AppInfo) when is_tuple(AppInfo) ->
    case rebar_app_info:opts(AppInfo) of
        undefined ->
            ?DEFAULT_LFE_OPTS;
        Opts ->
            LfeOpts = rebar_opts:get(Opts, lfe_opts, []),
            ErlOpts = rebar_opts:get(Opts, erl_opts, []),
            merge_opts(?DEFAULT_LFE_OPTS, LfeOpts ++ ErlOpts)
    end;
get_lfe_opts(State) ->
    Opts = rebar_state:opts(State),
    LfeOpts = rebar_opts:get(Opts, lfe_opts, []),
    ErlOpts = rebar_opts:get(Opts, erl_opts, []),
    merge_opts(?DEFAULT_LFE_OPTS, LfeOpts ++ ErlOpts).

%% @doc Get source directories for an application
-spec get_src_dirs(rebar_app_info:t()) -> [file:filename()].
get_src_dirs(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    %% Get additional source directories from config
    ExtraDirs = rebar_dir:src_dirs(Opts, []),

    %% Normalize and return absolute paths
    normalize_src_dirs(AppDir, [?DEFAULT_SRC_DIR | ExtraDirs]).

%% @doc Get include directories for an application
-spec get_include_dirs(rebar_app_info:t()) -> [file:filename()].
get_include_dirs(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    %% Get additional include directories
    ExtraDirs = rebar_opts:get(Opts, lfe_include_dirs, []),

    normalize_include_dirs(AppDir, [?DEFAULT_INCLUDE_DIR | ExtraDirs]).

%% @doc Get output directory for compiled beams
-spec get_out_dir(rebar_app_info:t()) -> file:filename().
get_out_dir(AppInfo) ->
    rebar_app_info:ebin_dir(AppInfo).

%% @doc Get list of files that must be compiled first
-spec get_first_files(rebar_app_info:t()) -> [file:filename()].
get_first_files(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    RelativeFiles = rebar_opts:get(Opts, lfe_first_files, []),
    [filename:join(AppDir, File) || File <- RelativeFiles].

%% @doc Check if verbose output is enabled
-spec is_verbose(rebar_app_info:t() | rebar_state:t()) -> boolean().
is_verbose(AppInfo) when is_tuple(AppInfo) ->
    Opts = case rebar_app_info:opts(AppInfo) of
        undefined -> rebar_opts:new();
        O -> O
    end,
    lists:member(verbose, get_lfe_opts(AppInfo)) orelse
        rebar_opts:get(Opts, verbose, false);
is_verbose(State) ->
    lists:member(verbose, get_lfe_opts(State)).

%% @doc Merge compiler options, with later options taking precedence
-spec merge_opts([term()], [term()]) -> [term()].
merge_opts(Defaults, Overrides) ->
    %% Build a map of options, later entries override earlier ones
    %% Special handling for tuples (like {i, Dir}, {outdir, Dir})
    Merged = lists:foldl(
        fun(Opt, Acc) ->
            Key = option_key(Opt),
            maps:put(Key, Opt, Acc)
        end,
        maps:new(),
        Defaults ++ Overrides
    ),
    maps:values(Merged).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Extract a key from an option for deduplication
-spec option_key(term()) -> term().
option_key({Key, _Value}) -> Key;
option_key(Atom) when is_atom(Atom) -> Atom;
option_key(Other) -> Other.

%% @doc Normalize source directories to absolute paths
-spec normalize_src_dirs(file:filename(), [file:filename()]) -> [file:filename()].
normalize_src_dirs(AppDir, Dirs) ->
    AbsDirs = [begin
        AbsPath = filename:absname(Dir, AppDir),
        case filelib:is_dir(AbsPath) of
            true -> AbsPath;
            false ->
                ?DEBUG("Source directory does not exist: ~s", [AbsPath]),
                AbsPath  % Return anyway, might be created later
        end
    end || Dir <- Dirs],

    %% Remove duplicates while preserving order
    lists:usort(AbsDirs).

%% @doc Normalize include directories to absolute paths
-spec normalize_include_dirs(file:filename(), [file:filename()]) -> [file:filename()].
normalize_include_dirs(AppDir, Dirs) ->
    AbsDirs = [begin
        AbsPath = filename:absname(Dir, AppDir),
        case filelib:is_dir(AbsPath) of
            true -> AbsPath;
            false ->
                ?DEBUG("Include directory does not exist: ~s", [AbsPath]),
                AbsPath
        end
    end || Dir <- Dirs],

    lists:usort(AbsDirs).
```

### Task 1.4: Implement Path Management

**File: `src/rb3lfe_paths.erl`**

```erlang
-module(rb3lfe_paths).

%% API exports
-export([
    set_paths/1,
    set_paths/2,
    unset_paths/1,
    unset_paths/2,
    with_paths/2,
    ensure_dir/1
]).

-include("rebar3_lfe/include/rb3lfe.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @doc Set code paths for dependencies
%% This replaces manual code:add_path* calls with proper rebar3 API
-spec set_paths(rebar_state:t()) -> ok.
set_paths(State) ->
    set_paths([deps, plugins], State).

%% @doc Set specific code paths
-spec set_paths([deps | plugins], rebar_state:t()) -> ok.
set_paths(Types, State) ->
    try
        rebar_paths:set_paths(Types, State),
        ok
    catch
        error:undef ->
            %% Fallback for older rebar3 versions
            fallback_set_paths(Types, State)
    end.

%% @doc Unset code paths
-spec unset_paths(rebar_state:t()) -> ok.
unset_paths(State) ->
    unset_paths([deps, plugins], State).

%% @doc Unset specific code paths
-spec unset_paths([deps | plugins], rebar_state:t()) -> ok.
unset_paths(Types, State) ->
    try
        rebar_paths:unset_paths(Types, State),
        ok
    catch
        error:undef ->
            %% Older rebar3 doesn't need explicit unset
            ok
    end.

%% @doc Execute a function with paths set, then unset them
-spec with_paths(fun(() -> Result), rebar_state:t()) -> Result when
    Result :: term().
with_paths(Fun, State) ->
    set_paths(State),
    try
        Fun()
    after
        unset_paths(State)
    end.

%% @doc Ensure a directory exists
%% Unlike filelib:ensure_dir/1, this ensures the directory itself exists,
%% not just its parent
-spec ensure_dir(file:filename()) -> ok | {error, term()}.
ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            case file:make_dir(Dir) of
                ok ->
                    ?DEBUG("Created directory: ~s", [Dir]),
                    ok;
                {error, eexist} ->
                    %% Race condition - directory was created between check and make_dir
                    ok;
                {error, Reason} = Error ->
                    ?ERROR("Failed to create directory ~s: ~p", [Dir, Reason]),
                    Error
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Fallback path setting for older rebar3 versions
-spec fallback_set_paths([deps | plugins], rebar_state:t()) -> ok.
fallback_set_paths(Types, State) ->
    lists:foreach(
        fun(Type) ->
            Paths = case Type of
                deps -> rebar_state:code_paths(State, all_deps);
                plugins -> rebar_state:code_paths(State, all_plugin_deps)
            end,
            code:add_pathsa(Paths)
        end,
        Types
    ),
    ok.
```

### Task 1.5: Implement Compiler Module Skeleton

**File: `src/rb3lfe_compiler_mod.erl`**

```erlang
-module(rb3lfe_compiler_mod).
-behaviour(rebar_compiler).

%% rebar_compiler callbacks
-export([
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2
]).

-include("rebar3_lfe/include/rb3lfe.hrl").

%%====================================================================
%% rebar_compiler callbacks
%%====================================================================

%% @doc Define the compiler context
%% This tells rebar3 where our source files are, what extensions to look for,
%% and where to put compiled output
-spec context(rebar_app_info:t()) -> #{
    src_dirs => [file:filename()],
    include_dirs => [file:filename()],
    src_ext => string(),
    out_mappings => [{string(), file:filename()}]
}.
context(AppInfo) ->
    SrcDirs = rb3lfe_config:get_src_dirs(AppInfo),
    IncludeDirs = rb3lfe_config:get_include_dirs(AppInfo),
    OutDir = rb3lfe_config:get_out_dir(AppInfo),

    ?DEBUG("Compiler context for ~s:", [rebar_app_info:name(AppInfo)]),
    ?DEBUG("  Source dirs: ~p", [SrcDirs]),
    ?DEBUG("  Include dirs: ~p", [IncludeDirs]),
    ?DEBUG("  Output dir: ~s", [OutDir]),

    #{
        src_dirs => SrcDirs,
        include_dirs => IncludeDirs,
        src_ext => ?LFE_SRC_EXTENSION,
        out_mappings => [{?BEAM_EXTENSION, OutDir}]
    }.

%% @doc Determine which files need compilation
%% This is called by rebar3 to filter the file list based on timestamps
%% and dependencies. We'll implement full logic in Phase 2.
-spec needed_files(
    rebar_digraph:t(),
    [file:filename()],
    [{string(), file:filename()}],
    rebar_app_info:t()
) -> {{[file:filename()], term()}, {{[file:filename()], [file:filename()]}, term()}}.
needed_files(_G, FoundFiles, _OutMappings, AppInfo) ->
    %% Phase 1: Simple implementation - compile everything
    %% Phase 2 will add proper dependency checking via DAG

    FirstFiles = rb3lfe_config:get_first_files(AppInfo),

    %% Separate first files from regular files
    {First, Rest} = lists:partition(
        fun(File) -> lists:member(File, FirstFiles) end,
        FoundFiles
    ),

    ?DEBUG("Files to compile: ~p first, ~p regular",
           [length(First), length(Rest)]),

    %% Return format: {{Sequential, Opts}, {{Sequential, Parallel}, Opts}}
    %% For now, everything is sequential
    {{First, []}, {{Rest, []}, []}}.

%% @doc Extract dependencies from a source file
%% Returns list of header files this source depends on
%% Full implementation in Phase 2
-spec dependencies(file:filename(), file:filename(), rebar_app_info:t()) ->
    [file:filename()].
dependencies(_Source, _SourceDir, _AppInfo) ->
    %% Phase 1: No dependency tracking yet
    %% Phase 2 will implement header file scanning
    [].

%% @doc Compile a source file
%% Full implementation in Phase 3
-spec compile(file:filename(), [{string(), file:filename()}],
              rebar_dict:t(), list()) -> ok.
compile(Source, _OutMappings, _Dict, _Opts) ->
    ?INFO("Would compile: ~s", [Source]),
    %% Phase 1: Stub implementation
    %% Phase 3 will add actual lfe_comp:file calls
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
```

### Task 1.6: Implement Plugin Entry Point

**File: `src/rb3lfe.erl`**

```erlang
-module(rb3lfe).

%% Plugin API
-export([init/1]).

-include("rebar3_lfe/include/rb3lfe.hrl").

%%====================================================================
%% Plugin API
%%====================================================================

%% @doc Initialize the rebar3 plugin
%% This is called by rebar3 when the plugin is loaded
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ?DEBUG("Initializing rb3lfe plugin...", []),

    %% Register our compiler module with rebar3
    %% This integrates us into rebar3's compilation pipeline
    State1 = rebar_state:append_compilers(State, [rb3lfe_compiler_mod]),

    ?DEBUG("Registered rb3lfe_compiler_mod with rebar3", []),

    %% Future phases will register providers here
    %% For now, just return the updated state
    {ok, State1}.
```

### Task 1.7: Create Test Utilities

**File: `test/test_utils.erl`**

```erlang
-module(test_utils).

%% Test utility exports
-export([
    create_temp_dir/0,
    create_temp_dir/1,
    cleanup_temp_dir/1,
    create_test_app/1,
    create_test_app/2,
    write_file/2,
    mock_app_info/1,
    mock_state/0
]).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Test Utilities
%%====================================================================

%% @doc Create a temporary directory for testing
create_temp_dir() ->
    create_temp_dir("rb3lfe_test").

create_temp_dir(Prefix) ->
    Rand = integer_to_list(erlang:unique_integer([positive])),
    Dir = filename:join([
        proplists:get_value(priv_dir, ct:get_config(ct_opts, [])),
        Prefix ++ "_" ++ Rand
    ]),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    ok = file:make_dir(Dir),
    Dir.

%% @doc Clean up a temporary directory
cleanup_temp_dir(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} ->
            ct:pal("Warning: Failed to cleanup ~s: ~p", [Dir, Reason]),
            ok
    end.

%% @doc Create a minimal test application structure
create_test_app(Dir) ->
    create_test_app(Dir, "test_app").

create_test_app(Dir, AppName) ->
    %% Create directory structure
    SrcDir = filename:join(Dir, "src"),
    IncludeDir = filename:join(Dir, "include"),
    EbinDir = filename:join(Dir, "ebin"),

    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(IncludeDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create .app.src file
    AppSrc = io_lib:format(
        "{application, ~s, [~n"
        "  {description, \"Test Application\"},~n"
        "  {vsn, \"0.1.0\"},~n"
        "  {modules, []},~n"
        "  {registered, []},~n"
        "  {applications, [kernel, stdlib]}~n"
        "]}.~n",
        [AppName]
    ),
    write_file(filename:join(SrcDir, AppName ++ ".app.src"), AppSrc),

    %% Create rebar.config
    RebarConfig =
        "{erl_opts, [debug_info]}.\n"
        "{deps, [{lfe, \"2.2.0\"}]}.\n",
    write_file(filename:join(Dir, "rebar.config"), RebarConfig),

    #{
        dir => Dir,
        app_name => AppName,
        src_dir => SrcDir,
        include_dir => IncludeDir,
        ebin_dir => EbinDir
    }.

%% @doc Write content to a file
write_file(Path, Content) when is_list(Content) ->
    write_file(Path, list_to_binary(Content));
write_file(Path, Content) when is_binary(Content) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Content).

%% @doc Create a mock rebar_app_info structure for testing
mock_app_info(AppDir) ->
    %% This is a simplified mock - real tests should use rebar_app_info:new()
    #{
        dir => AppDir,
        ebin_dir => filename:join(AppDir, "ebin"),
        opts => rebar_opts:new()
    }.

%% @doc Create a mock rebar_state for testing
mock_state() ->
    rebar_state:new().
```

### Task 1.8: Create Configuration Tests

**File: `test/rb3lfe_config_SUITE.erl`**

```erlang
-module(rb3lfe_config_SUITE).

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
    get_lfe_opts_defaults/1,
    get_lfe_opts_merged/1,
    get_src_dirs_default/1,
    get_src_dirs_custom/1,
    get_include_dirs_default/1,
    get_include_dirs_custom/1,
    merge_opts_simple/1,
    merge_opts_override/1,
    normalize_src_dirs/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        get_lfe_opts_defaults,
        get_lfe_opts_merged,
        get_src_dirs_default,
        get_src_dirs_custom,
        get_include_dirs_default,
        get_include_dirs_custom,
        merge_opts_simple,
        merge_opts_override,
        normalize_src_dirs
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

get_lfe_opts_defaults(_Config) ->
    %% Create a minimal app info with no custom opts
    AppInfo = rebar_app_info:new(test_app, "0.1.0", "/tmp/test"),

    Opts = rb3lfe_config:get_lfe_opts(AppInfo),

    %% Should contain default options
    ?assert(lists:member(return, Opts)),
    ?assert(lists:member(verbose, Opts)),
    ok.

get_lfe_opts_merged(_Config) ->
    %% Create app info with custom LFE opts
    RebarOpts = rebar_opts:new(),
    RebarOpts1 = rebar_opts:set(RebarOpts, lfe_opts, [{debug_info, true}]),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", "/tmp/test"),
    AppInfo1 = rebar_app_info:opts(AppInfo, RebarOpts1),

    Opts = rb3lfe_config:get_lfe_opts(AppInfo1),

    %% Should contain both defaults and custom opts
    ?assert(lists:member(return, Opts)),
    ?assert(lists:keymember(debug_info, 1, Opts)),
    ok.

get_src_dirs_default(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),

    SrcDirs = rb3lfe_config:get_src_dirs(AppInfo),

    %% Should have default src directory
    ?assertEqual(1, length(SrcDirs)),
    ?assert(lists:any(
        fun(Dir) -> filename:basename(Dir) =:= "src" end,
        SrcDirs
    )),
    ok.

get_src_dirs_custom(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    %% Create additional source directory
    ExtraDir = filename:join(AppDir, "extra_src"),
    ok = filelib:ensure_dir(filename:join(ExtraDir, "dummy")),

    %% Configure custom source directories
    RebarOpts = rebar_opts:new(),
    RebarOpts1 = rebar_opts:set(RebarOpts, src_dirs, ["src", "extra_src"]),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),
    AppInfo1 = rebar_app_info:opts(AppInfo, RebarOpts1),

    SrcDirs = rb3lfe_config:get_src_dirs(AppInfo1),

    %% Should have both directories
    ?assertEqual(2, length(SrcDirs)),
    ok.

get_include_dirs_default(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),

    IncludeDirs = rb3lfe_config:get_include_dirs(AppInfo),

    %% Should have default include directory
    ?assert(length(IncludeDirs) >= 1),
    ?assert(lists:any(
        fun(Dir) -> filename:basename(Dir) =:= "include" end,
        IncludeDirs
    )),
    ok.

get_include_dirs_custom(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    %% Configure custom include directories
    RebarOpts = rebar_opts:new(),
    RebarOpts1 = rebar_opts:set(RebarOpts, lfe_include_dirs, ["other_include"]),

    AppInfo = rebar_app_info:new(test_app, "0.1.0", AppDir),
    AppInfo1 = rebar_app_info:opts(AppInfo, RebarOpts1),

    IncludeDirs = rb3lfe_config:get_include_dirs(AppInfo1),

    %% Should have custom directory
    ?assert(lists:any(
        fun(Dir) -> filename:basename(Dir) =:= "other_include" end,
        IncludeDirs
    )),
    ok.

merge_opts_simple(_Config) ->
    Defaults = [return, verbose],
    Overrides = [debug_info],

    Merged = rb3lfe_config:merge_opts(Defaults, Overrides),

    ?assert(lists:member(return, Merged)),
    ?assert(lists:member(verbose, Merged)),
    ?assert(lists:member(debug_info, Merged)),
    ok.

merge_opts_override(_Config) ->
    Defaults = [{outdir, "/tmp/default"}],
    Overrides = [{outdir, "/tmp/override"}],

    Merged = rb3lfe_config:merge_opts(Defaults, Overrides),

    %% Override should win
    ?assertEqual([{outdir, "/tmp/override"}], Merged),
    ok.

normalize_src_dirs(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create some test directories
    Dir1 = filename:join(TestDir, "src1"),
    Dir2 = filename:join(TestDir, "src2"),
    ok = filelib:ensure_dir(filename:join(Dir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(Dir2, "dummy")),

    Normalized = rb3lfe_config:normalize_src_dirs(TestDir, ["src1", "src2"]),

    ?assertEqual(2, length(Normalized)),
    ?assert(lists:all(fun filelib:is_dir/1, Normalized)),
    ok.
```

### Task 1.9: Create Path Management Tests

**File: `test/rb3lfe_paths_SUITE.erl`**

```erlang
-module(rb3lfe_paths_SUITE).

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

    ok = rb3lfe_paths:ensure_dir(NewDir),

    ?assert(filelib:is_dir(NewDir)),
    ok.

ensure_dir_existing_directory(Config) ->
    TestDir = ?config(test_dir, Config),

    %% TestDir already exists
    ok = rb3lfe_paths:ensure_dir(TestDir),

    ?assert(filelib:is_dir(TestDir)),
    ok.

ensure_dir_nested_directory(Config) ->
    TestDir = ?config(test_dir, Config),
    NestedDir = filename:join([TestDir, "a", "b", "c"]),

    ?assertNot(filelib:is_dir(NestedDir)),

    %% ensure_dir should only create the final directory
    %% Parent directories must exist
    ParentDir = filename:join([TestDir, "a", "b"]),
    ok = filelib:ensure_dir(filename:join(ParentDir, "dummy")),
    ok = file:make_dir(filename:join(TestDir, "a")),
    ok = file:make_dir(ParentDir),

    ok = rb3lfe_paths:ensure_dir(NestedDir),

    ?assert(filelib:is_dir(NestedDir)),
    ok.

with_paths_executes_function(_Config) ->
    State = test_utils:mock_state(),

    Result = rb3lfe_paths:with_paths(
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
        rb3lfe_paths:with_paths(
            fun() -> error(test_error) end,
            State
        )
    ),

    ok.
```

## Testing Instructions

### Running the Tests

```bash
# Compile the project
rebar3 compile

# Run all Common Test suites
rebar3 ct

# Run specific suite
rebar3 ct --suite=test/rb3lfe_config_SUITE

# Run with coverage
rebar3 as test do ct, cover

# Run dialyzer
rebar3 dialyzer

# Run full check
rebar3 check
```

### Expected Outcomes

At the end of Phase 1, you should have:

1. ✅ Modern project structure with proper rebar.config
2. ✅ Shared header file with common definitions
3. ✅ Configuration management module with tests
4. ✅ Path management module with tests
5. ✅ Compiler module skeleton registered with rebar3
6. ✅ Plugin entry point that registers the compiler
7. ✅ Test infrastructure with CT suites
8. ✅ All tests passing
9. ✅ Dialyzer clean

### Integration Checklist

- [ ] Project compiles without warnings
- [ ] All CT tests pass
- [ ] Dialyzer finds no issues
- [ ] Can run `rebar3 compile` on a test LFE project (won't actually compile yet)
- [ ] Code coverage > 80%

## Next Steps

Phase 2 will implement:

- Dependency scanner for header files
- DAG integration for proper incremental builds
- Full implementation of `needed_files/4` and `dependencies/3`

## Notes for Claude Code

- Pay careful attention to the rebar3 API versions - some functions changed between 3.22 and 3.25
- All file operations should use `file:` and `filelib:` - never shell out to OS commands
- Every function needs proper `-spec` declarations for dialyzer
- Use `?DEBUG`, `?INFO`, `?WARN`, `?ERROR` macros consistently, never `io:format`
- Tests should be independent and clean up after themselves
- Mock functions in tests should be clearly labeled as mocks
