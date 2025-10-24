# Phase 5: Provider Layer Implementation

## Overview

This phase implements all the rebar3 command providers that users interact with. These are thin wrappers around our core functionality, following rebar3's provider pattern. We'll implement compile, clean, REPL, test, release, and all other commands with proper error handling and user experience.

**Goal**: Complete, professional rebar3 commands that users love to use.

## Prerequisites

- Phase 1-4 completed with all tests passing
- Understanding of rebar3 provider pattern
- Knowledge of rebar3's hooks and state management
- Familiarity with LFE REPL startup requirements

## Provider Architecture

```
Provider Pattern (for each command):

1. init/1 - Register provider with rebar3
2. do/1 - Execute the command
3. format_error/1 - Format errors for display

Providers are thin wrappers that:
- Validate inputs
- Set up paths
- Call core modules
- Format output
- Handle errors gracefully
```

## Implementation Tasks

### Task 5.1: Implement Compile Provider

**File: `src/r3lfe_prv_compile.erl`**

```erlang
-module(r3lfe_prv_compile).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, compile).
-define(NAMESPACE, lfe).
-define(DEPS, [{default, lock}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Compile LFE source files",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe compile"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE compile provider starting", []),

    %% Set up code paths
    rebar_paths:set_paths([deps], State),

    try
        %% Get all project apps
        Apps = case rebar_state:current_app(State) of
            undefined ->
                %% Compiling all apps (umbrella or top-level)
                rebar_state:project_apps(State);
            AppInfo ->
                %% Compiling single app (via hook)
                [AppInfo]
        end,

        ?INFO("Compiling ~p LFE application(s)", [length(Apps)]),

        %% Compile each app
        lists:foreach(
            fun(AppInfo) ->
                compile_app(AppInfo, State)
            end,
            Apps
        ),

        {ok, State}
    catch
        throw:{error, Reason} ->
            {error, format_error(Reason)};
        error:Reason:Stack ->
            ?ERROR("Compilation failed with error: ~p", [Reason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({compilation_error, Reason})}
    end.

-spec format_error(term()) -> iolist().
format_error({compilation_error, Reason}) ->
    io_lib:format("Compilation failed: ~p", [Reason]);
format_error({package_error, Reason}) ->
    io_lib:format("Package preparation failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec compile_app(rebar_app_info:t(), rebar_state:t()) -> ok.
compile_app(AppInfo, State) ->
    AppName = rebar_app_info:name(AppInfo),
    ?DEBUG("Compiling LFE app: ~s", [AppName]),

    %% Ensure the .app.src is compiled to .app first
    rebar_otp_app:compile(State, AppInfo),

    %% Get source directories
    SrcDirs = r3lfe_config:get_src_dirs(AppInfo),
    OutDir = r3lfe_config:get_out_dir(AppInfo),

    %% Ensure output directory exists
    ok = r3lfe_paths:ensure_dir(OutDir),

    %% Discover all LFE files
    AllFiles = lists:flatmap(
        fun r3lfe_package:discover_files/1,
        SrcDirs
    ),

    case AllFiles of
        [] ->
            ?DEBUG("No LFE files found in ~s", [AppName]),
            ok;
        _ ->
            ?DEBUG("Found ~p LFE files in ~s", [length(AllFiles), AppName]),

            %% Prepare package files
            case r3lfe_package:prepare_packages(AllFiles) of
                {ok, PackageInfos} ->
                    try
                        compile_files(AllFiles, PackageInfos, AppInfo, State)
                    after
                        %% Always cleanup packages
                        r3lfe_package:cleanup_packages(PackageInfos)
                    end;
                {error, Reason} ->
                    throw({error, {package_error, Reason}})
            end
    end.

-spec compile_files([file:filename()], [map()],
                    rebar_app_info:t(), rebar_state:t()) -> ok.
compile_files(AllFiles, PackageInfos, AppInfo, _State) ->
    %% Build map of source -> temp for packages
    PackageMap = maps:from_list([
        {maps:get(source_file, Info), maps:get(temp_file, Info)}
        || Info <- PackageInfos
    ]),

    %% Determine which files to compile
    %% For package files, compile the temp file instead
    FilesToCompile = lists:map(
        fun(File) ->
            case maps:get(File, PackageMap, undefined) of
                undefined -> File;  % Not a package, use original
                TempFile -> TempFile  % Package, use temp
            end
        end,
        AllFiles
    ),

    %% Get first files
    FirstFiles = r3lfe_config:get_first_files(AppInfo),

    %% Separate first files from rest
    {First, Rest} = lists:partition(
        fun(File) ->
            %% Check if base name matches any first file
            BaseName = filename:basename(File),
            lists:any(
                fun(FirstFile) ->
                    filename:basename(FirstFile) =:= BaseName
                end,
                FirstFiles
            )
        end,
        FilesToCompile
    ),

    %% Compile in order: first files, then rest
    OrderedFiles = First ++ Rest,

    %% Get compiler options
    LfeOpts = r3lfe_config:get_lfe_opts(AppInfo),
    OutDir = r3lfe_config:get_out_dir(AppInfo),

    %% Initialize progress
    Progress = r3lfe_progress:init(length(OrderedFiles)),
    r3lfe_progress:report_start(length(OrderedFiles), AppInfo),

    %% Compile each file
    {_FinalProgress, Results} = lists:foldl(
        fun(File, {Prog, Acc}) ->
            Result = r3lfe_compile_worker:compile_file(File, OutDir, LfeOpts),
            NewProg = r3lfe_progress:report_file(Prog),
            {NewProg, [Result | Acc]}
        end,
        {Progress, []},
        OrderedFiles
    ),

    r3lfe_progress:report_complete(Progress),

    %% Check for errors
    Errors = [R || R <- Results, element(1, R) =:= error],

    case Errors of
        [] ->
            ok;
        _ ->
            throw({error, {compilation_failed, length(Errors)}})
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Compiles all LFE source files in the project. Supports:~n"
        "  - Flat module structure (src/*.lfe)~n"
        "  - Nested packages (src/my/package/*.lfe)~n"
        "  - Mixed structures~n"
        "  - Header file dependency tracking~n"
        "  - Incremental compilation~n"
        "~n"
        "LFE compiler options can be specified via 'lfe_opts' or 'erl_opts'~n"
        "in rebar.config.~n",
        [Description]
    ).
```

### Task 5.2: Implement Clean Providers

**File: `src/r3lfe_prv_clean.erl`**

```erlang
-module(r3lfe_prv_clean).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, clean).
-define(NAMESPACE, lfe).
-define(DEPS, []).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Clean compiled LFE files",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe clean"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE clean provider starting", []),

    Apps = case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        AppInfo -> [AppInfo]
    end,

    lists:foreach(fun clean_app/1, Apps),

    %% Also clean up any temporary package files
    r3lfe_package_tracker:cleanup_all(),

    {ok, State}.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec clean_app(rebar_app_info:t()) -> ok.
clean_app(AppInfo) ->
    AppName = rebar_app_info:name(AppInfo),
    ?INFO("Cleaning ~s", [AppName]),

    EbinDir = rebar_app_info:ebin_dir(AppInfo),

    %% Find all .beam files
    BeamFiles = filelib:wildcard(
        filename:join(EbinDir, "*.beam")
    ),

    %% Delete each beam file
    lists:foreach(
        fun(File) ->
            case file:delete(File) of
                ok ->
                    ?DEBUG("Deleted: ~s", [File]);
                {error, enoent} ->
                    ok;
                {error, Reason} ->
                    ?WARN("Failed to delete ~s: ~p", [File, Reason])
            end
        end,
        BeamFiles
    ),

    ok.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Removes all compiled .beam files from the ebin directory.~n",
        [Description]
    ).
```

### Task 5.3: Implement REPL Provider

**File: `src/r3lfe_prv_repl.erl`**

```erlang
-module(r3lfe_prv_repl).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, repl).
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Start an LFE REPL with project apps loaded",

    Opts = [
        {name, undefined, "name", atom,
         "Give a long name to the node"},
        {sname, undefined, "sname", atom,
         "Give a short name to the node"},
        {setcookie, undefined, "setcookie", atom,
         "Set the cookie for distributed node"},
        {apps, undefined, "apps", string,
         "List of apps to start (comma-separated)"},
        {script, undefined, "script", string,
         "Script to run before starting REPL"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe repl"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE REPL provider starting", []),

    %% Set up code paths
    rebar_paths:set_paths([deps, plugins], State),

    %% Get REPL configuration
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    LfeConfig = rebar_state:get(State, lfe, []),
    ReplConfig = proplists:get_value(repl, LfeConfig, []),

    %% Merge options
    MergedOpts = merge_repl_opts(ReplConfig, Opts),

    %% Start apps if requested
    maybe_start_apps(MergedOpts, State),

    %% Run script if provided
    maybe_run_script(MergedOpts),

    %% Check Erlang version and start appropriate REPL
    OTPRelease = erlang:system_info(otp_release),

    if
        OTPRelease >= "26" ->
            start_modern_repl(MergedOpts, State);
        true ->
            start_legacy_repl(MergedOpts, State)
    end,

    {ok, State}.

-spec format_error(term()) -> iolist().
format_error({app_start_failed, App, Reason}) ->
    io_lib:format("Failed to start application ~s: ~p", [App, Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec merge_repl_opts(proplists:proplist(), proplists:proplist()) -> map().
merge_repl_opts(ConfigOpts, CmdOpts) ->
    %% Command line options take precedence
    maps:merge(
        maps:from_list(ConfigOpts),
        maps:from_list(CmdOpts)
    ).

-spec maybe_start_apps(map(), rebar_state:t()) -> ok.
maybe_start_apps(Opts, State) ->
    case maps:get(apps, Opts, undefined) of
        undefined ->
            %% Start project apps by default
            Apps = rebar_state:project_apps(State),
            lists:foreach(
                fun(AppInfo) ->
                    App = rebar_app_info:name(AppInfo),
                    case is_binary(App) of
                        true ->
                            start_app(binary_to_atom(App, utf8));
                        false ->
                            start_app(App)
                    end
                end,
                Apps
            );
        AppsStr ->
            %% Parse comma-separated list
            AppNames = string:split(AppsStr, ",", all),
            lists:foreach(
                fun(AppStr) ->
                    App = list_to_atom(string:trim(AppStr)),
                    start_app(App)
                end,
                AppNames
            )
    end.

-spec start_app(atom()) -> ok.
start_app(App) ->
    case application:ensure_all_started(App) of
        {ok, Started} ->
            ?DEBUG("Started applications: ~p", [Started]),
            ok;
        {error, Reason} ->
            ?WARN("Failed to start ~s: ~p", [App, Reason]),
            ok
    end.

-spec maybe_run_script(map()) -> ok.
maybe_run_script(Opts) ->
    case maps:get(script, Opts, undefined) of
        undefined ->
            ok;
        ScriptPath ->
            ?INFO("Running script: ~s", [ScriptPath]),
            case lfescript:run([ScriptPath]) of
                ok -> ok;
                {error, Reason} ->
                    ?WARN("Script failed: ~p", [Reason]),
                    ok
            end
    end.

-spec start_modern_repl(map(), rebar_state:t()) -> ok.
start_modern_repl(Opts, State) ->
    ?DEBUG("Starting modern REPL (OTP 26+)", []),

    %% For OTP 26+, use shell:start_interactive/1
    ShellArgs = build_shell_args(Opts),

    %% Update state with shell configuration
    State1 = rebar_state:set(State, shell, ShellArgs),

    %% Use rebar3's shell provider infrastructure
    rebar_prv_shell:do(State1),

    ok.

-spec start_legacy_repl(map(), rebar_state:t()) -> ok.
start_legacy_repl(Opts, State) ->
    ?DEBUG("Starting legacy REPL (OTP < 26)", []),

    %% For older OTP, use custom REPL starter
    ReplModule = maps:get(start_module, Opts, lfe_shell),
    NoBanner = maps:get(nobanner, Opts, false),

    %% Build banner
    Banner = case NoBanner of
        true -> "";
        false -> build_banner()
    end,

    %% Start LFE REPL
    case NoBanner of
        false ->
            io:put_chars(Banner);
        true ->
            ok
    end,

    %% Start the REPL module
    ReplModule:start(),

    ok.

-spec build_shell_args(map()) -> proplists:proplist().
build_shell_args(Opts) ->
    ReplModule = maps:get(start_module, Opts, lfe_shell),
    NoBanner = maps:get(nobanner, Opts, false),

    [{shell_args, [{ReplModule, start, []}]},
     {nobanner, NoBanner}].

-spec build_banner() -> string().
build_banner() ->
    LfeVersion = lfe_version(),

    "\n"
    "   ..-~.~_~---..   \n"
    "  (      \\     )    |   A Lisp-2+ on the Erlang VM\n"
    "  |`-.._/_\\_.-':    |   Type (help) for usage info.\n"
    "  |         g |_ \\   |\n"
    "  |        n    | |  |   Docs: http://docs.lfe.io/\n"
    "  |       a    / /   |   Source: http://github.com/lfe/lfe\n"
    "   \\     l    |_/    |\n"
    "    \\   r     /      |   LFE v" ++ LfeVersion ++ "\n"
    "     `-E___.-'       \n\n".

-spec lfe_version() -> string().
lfe_version() ->
    case application:get_key(lfe, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Starts an LFE REPL with the project and its dependencies on~n"
        "the code path. Project applications can optionally be started.~n"
        "~n"
        "Options:~n"
        "  --name NAME       Give the node a long name~n"
        "  --sname NAME      Give the node a short name~n"
        "  --apps APPS       Comma-separated list of apps to start~n"
        "  --script PATH     Script to run before REPL starts~n"
        "~n"
        "Configuration via rebar.config:~n"
        "  {lfe, [{repl, [{start_module, Module},~n"
        "                 {nobanner, true}]}]}.~n",
        [Description]
    ).
```

### Task 5.4: Implement Test Provider

**File: `src/r3lfe_prv_ltest.erl`**

```erlang
-module(r3lfe_prv_ltest).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, ltest).
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Run LFE tests using ltest",

    Opts = [
        {suite, $s, "suite", string,
         "Test suite to run"},
        {test, $t, "test", string,
         "Specific test to run"},
        {verbose, $v, "verbose", boolean,
         "Verbose output"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe ltest"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE ltest provider starting", []),

    %% Ensure ltest is available
    case code:ensure_loaded(ltest) of
        {module, ltest} ->
            run_tests(State);
        {error, _} ->
            {error, "ltest not found. Add {ltest, \"~> 0.13\"} to deps in rebar.config"}
    end.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec run_tests(rebar_state:t()) -> {ok, rebar_state:t()}.
run_tests(State) ->
    %% Set up code paths including test
    rebar_paths:set_paths([deps, plugins], State),
    add_test_paths(State),

    %% Get test options
    {Opts, _Args} = rebar_state:command_parsed_args(State),

    %% Run ltest
    TestOpts = build_test_opts(Opts),

    ?INFO("Running LFE tests...", []),

    Result = ltest:run(TestOpts),

    case Result of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, format_error(Reason)}
    end.

-spec add_test_paths(rebar_state:t()) -> ok.
add_test_paths(State) ->
    Apps = rebar_state:project_apps(State),

    lists:foreach(
        fun(AppInfo) ->
            AppDir = rebar_app_info:dir(AppInfo),
            TestDir = filename:join(AppDir, "test"),
            case filelib:is_dir(TestDir) of
                true ->
                    code:add_patha(TestDir);
                false ->
                    ok
            end
        end,
        Apps
    ),

    ok.

-spec build_test_opts(proplists:proplist()) -> map().
build_test_opts(Opts) ->
    DefaultOpts = ltest:'default-opts'(),

    %% Override with command line options
    maps:merge(
        DefaultOpts,
        maps:from_list(Opts)
    ).

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Runs LFE tests using the ltest framework.~n"
        "~n"
        "Requires ltest as a dependency:~n"
        "  {deps, [{ltest, \"~> 0.13\"}]}.~n"
        "~n"
        "Options:~n"
        "  --suite SUITE    Run specific test suite~n"
        "  --test TEST      Run specific test~n"
        "  --verbose        Enable verbose output~n",
        [Description]
    ).
```

### Task 5.5: Implement Release Provider

**File: `src/r3lfe_prv_release.erl`**

```erlang
-module(r3lfe_prv_release).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, release).
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Build an LFE release",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe release"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE release provider starting", []),

    %% Update app files with modules list
    update_app_files(State),

    %% Delegate to rebar3's release provider
    try
        rebar_relx:do(rlx_prv_release, "release", release, State),
        {ok, State}
    catch
        error:undef ->
            %% Try older API
            rebar_relx:do(release, State),
            {ok, State}
    end.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec update_app_files(rebar_state:t()) -> ok.
update_app_files(State) ->
    %% Get all apps
    Apps = rebar_state:project_apps(State),

    %% Update each app file
    lists:foreach(fun update_app_file/1, Apps),

    ok.

-spec update_app_file(rebar_app_info:t()) -> ok.
update_app_file(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    AppFile = rebar_app_info:app_file(AppInfo),

    %% Find all beam files
    BeamFiles = filelib:wildcard(filename:join(EbinDir, "*.beam")),

    %% Extract module names
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- BeamFiles],

    %% Read current app file
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppData}]} ->
            %% Update modules list
            AppData1 = lists:keystore(modules, 1, AppData, {modules, Modules}),

            %% Write updated app file
            Content = io_lib:format("~p.~n", [{application, AppName, AppData1}]),

            ok = file:write_file(AppFile, Content),

            ?DEBUG("Updated ~s with ~p modules", [AppFile, length(Modules)]),
            ok;

        {error, Reason} ->
            ?WARN("Failed to read app file ~s: ~p", [AppFile, Reason]),
            ok
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Builds a release for the LFE project using relx.~n"
        "~n"
        "Requires relx configuration in rebar.config:~n"
        "  {relx, [~n"
        "    {release, {myapp, \"0.1.0\"}, [myapp]},~n"
        "    {dev_mode, false},~n"
        "    {include_erts, true}~n"
        "  ]}.~n",
        [Description]
    ).
```

### Task 5.6: Implement Versions Provider

**File: `src/r3lfe_prv_versions.erl`**

```erlang
-module(r3lfe_prv_versions).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, versions).
-define(NAMESPACE, lfe).
-define(DEPS, []).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Display version information",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe versions"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Apps = rebar_state:project_apps(State),

    %% Collect version information
    VersionInfo = #{
        apps => get_app_versions(Apps),
        languages => get_language_versions(),
        tools => get_tool_versions()
    },

    %% Display nicely formatted output
    display_versions(VersionInfo),

    {ok, State}.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_app_versions([rebar_app_info:t()]) -> [{atom(), string()}].
get_app_versions(Apps) ->
    lists:map(
        fun(AppInfo) ->
            Name = rebar_app_info:name(AppInfo),
            Vsn = rebar_app_info:original_vsn(AppInfo),

            AppName = case is_binary(Name) of
                true -> binary_to_atom(Name, utf8);
                false -> Name
            end,

            {AppName, Vsn}
        end,
        Apps
    ).

-spec get_language_versions() -> [{atom(), string()}].
get_language_versions() ->
    [
        {lfe, get_version(lfe)},
        {erlang, erlang:system_info(otp_release)},
        {erts, erlang:system_info(version)}
    ].

-spec get_tool_versions() -> [{atom(), string()}].
get_tool_versions() ->
    [
        {rebar3, get_rebar3_version()},
        {r3lfe, get_version(r3lfe)}
    ].

-spec get_version(atom()) -> string().
get_version(App) ->
    case application:get_key(App, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.

-spec get_rebar3_version() -> string().
get_rebar3_version() ->
    case application:get_key(rebar, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.

-spec display_versions(map()) -> ok.
display_versions(#{apps := Apps, languages := Langs, tools := Tools}) ->
    io:format("~n=== Project Applications ===~n"),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Apps
    ),

    io:format("~n=== Languages ===~n"),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Langs
    ),

    io:format("~n=== Build Tools ===~n"),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Tools
    ),

    io:format("~n"),
    ok.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Displays version information for:~n"
        "  - Project applications~n"
        "  - LFE and Erlang/OTP~n"
        "  - Build tools (rebar3, r3lfe)~n",
        [Description]
    ).
```

### Task 5.7: Register All Providers

**File: `src/r3lfe.erl` (UPDATE)**

```erlang
-module(r3lfe).

-export([init/1]).

-include("rebar3_lfe/include/r3lfe.hrl").

%%====================================================================
%% Plugin API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ?DEBUG("Initializing r3lfe plugin...", []),

    %% Initialize all caches and trackers
    ok = r3lfe_dep_cache:init(),
    ok = r3lfe_compile_opts:init(),
    ok = r3lfe_package_tracker:init(),

    %% Register our compiler module with rebar3
    State1 = rebar_state:append_compilers(State, [r3lfe_compiler_mod]),

    ?DEBUG("Registered r3lfe_compiler_mod with rebar3", []),

    %% Register all providers
    Providers = [
        r3lfe_prv_compile,
        r3lfe_prv_clean,
        r3lfe_prv_repl,
        r3lfe_prv_ltest,
        r3lfe_prv_release,
        r3lfe_prv_versions
    ],

    State2 = lists:foldl(
        fun(Provider, StateAcc) ->
            {ok, StateAcc1} = Provider:init(StateAcc),
            StateAcc1
        end,
        State1,
        Providers
    ),

    ?DEBUG("Registered ~p providers", [length(Providers)]),

    {ok, State2}.
```

### Task 5.8: Update .app.src

**File: `src/r3lfe.app.src` (UPDATE)**

```erlang
{application, r3lfe, [
    {description, "Modern rebar3 plugin for LFE projects"},
    {vsn, "0.5.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        lfe
    ]},
    {env, []},
    {modules, []},

    %% Hex.pm metadata
    {licenses, ["Apache-2.0"]},
    {links, [
        {"GitHub", "https://github.com/lfe-rebar3/rebar3_lfe"},
        {"Hex", "https://hex.pm/packages/r3lfe"},
        {"LFE", "https://lfe.io"}
    ]},
    {files, [
        "src",
        "README.md",
        "LICENSE",
        "rebar.config"
    ]},

    %% Breaking changes notice
    {notes, "v0.5.0 is a complete rewrite with breaking changes from v0.4.x"}
]}.
```

### Task 5.9: Create Provider Tests

**File: `test/providers_SUITE.erl`**

```erlang
-module(providers_SUITE).

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
    compile_provider_registers/1,
    clean_provider_registers/1,
    compile_provider_works/1,
    clean_provider_works/1,
    versions_provider_works/1,
    provider_namespace_correct/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_provider_registers,
        clean_provider_registers,
        compile_provider_works,
        clean_provider_works,
        versions_provider_works,
        provider_namespace_correct
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
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

compile_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_compile:init(State),

    %% Check provider is registered
    Providers = rebar_state:providers(State1),

    Found = lists:any(
        fun(P) ->
            providers:get_provider_name(P) =:= compile andalso
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(Found, "Compile provider should be registered"),
    ok.

clean_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_clean:init(State),

    Providers = rebar_state:providers(State1),

    Found = lists:any(
        fun(P) ->
            providers:get_provider_name(P) =:= clean andalso
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(Found, "Clean provider should be registered"),
    ok.

compile_provider_works(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir, "testapp"),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create a simple LFE file
    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule simple)\n"
        "(defun test () 'ok)\n"),

    %% Create rebar state with this app
    State = rebar_state:new(),
    AppInfo = rebar_app_info:new(testapp, "0.1.0", AppDir),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    %% Initialize and run compile provider
    {ok, State2} = r3lfe_prv_compile:init(State1),

    Result = r3lfe_prv_compile:do(State2),

    ?assertMatch({ok, _}, Result),

    %% Check beam file was created
    BeamFile = filename:join(maps:get(ebin_dir, AppData), "simple.beam"),
    ?assert(filelib:is_file(BeamFile)),

    ok.

clean_provider_works(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir, "testapp"),
    AppDir = maps:get(dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create a fake beam file
    BeamFile = filename:join(EbinDir, "fake.beam"),
    test_utils:write_file(BeamFile, <<>>),

    ?assert(filelib:is_file(BeamFile)),

    %% Create state and run clean
    State = rebar_state:new(),
    AppInfo = rebar_app_info:new(testapp, "0.1.0", AppDir),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    {ok, State2} = r3lfe_prv_clean:init(State1),
    {ok, _State3} = r3lfe_prv_clean:do(State2),

    %% Beam file should be deleted
    ?assertNot(filelib:is_file(BeamFile)),

    ok.

versions_provider_works(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_versions:init(State),

    %% Should not error
    Result = r3lfe_prv_versions:do(State1),

    ?assertMatch({ok, _}, Result),
    ok.

provider_namespace_correct(_Config) ->
    State = rebar_state:new(),

    %% Initialize all providers
    {ok, State1} = r3lfe:init(State),

    %% Get all providers
    Providers = rebar_state:providers(State1),

    %% Find our providers
    OurProviders = lists:filter(
        fun(P) ->
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    %% Should have at least our core providers
    ?assert(length(OurProviders) >= 5),

    ok.
```

## Testing Instructions

### Running Tests

```bash
# Run all provider tests
rebar3 ct --suite=test/providers_SUITE

# Run all tests
rebar3 ct

# Full check
rebar3 check
```

### Manual End-to-End Test

```bash
# Create new LFE project
mkdir my_lfe_app
cd my_lfe_app

cat > rebar.config <<EOF
{plugins, [{r3lfe, "0.5.0"}]}.
{deps, [{lfe, "2.2.0"}]}.
EOF

# Create source
mkdir -p src/myapp

cat > src/myapp.lfe <<EOF
(defmodule myapp)
(defun start () 'ok)
EOF

cat > src/myapp/utils.lfe <<EOF
(defmodule myapp.utils)
(defun helper () 'helper)
EOF

# Test compile
rebar3 lfe compile
# Should compile both files

# Test clean
rebar3 lfe clean
# Should remove beam files

# Recompile
rebar3 lfe compile

# Test REPL
rebar3 lfe repl
# Should start LFE REPL with modules loaded

# Test versions
rebar3 lfe versions
# Should display version info
```

## Expected Outcomes

At the end of Phase 5, you should have:

1. ✅ Complete provider layer for all commands
2. ✅ Compile provider integrated with all core modules
3. ✅ Clean provider removing build artifacts
4. ✅ REPL provider working on OTP 24-28
5. ✅ Test provider for ltest integration
6. ✅ Release provider for OTP releases
7. ✅ Versions provider for diagnostics
8. ✅ All providers properly namespaced under `lfe`
9. ✅ Comprehensive test coverage
10. ✅ Manual verification successful

### Integration Checklist

- [ ] All previous tests pass
- [ ] `rebar3 lfe compile` works
- [ ] `rebar3 lfe clean` removes beam files
- [ ] `rebar3 lfe repl` starts LFE REPL
- [ ] `rebar3 lfe ltest` runs tests (with ltest dep)
- [ ] `rebar3 lfe release` builds releases
- [ ] `rebar3 lfe versions` shows version info
- [ ] All commands work in umbrella projects
- [ ] Package system integrated correctly
- [ ] Header dependencies tracked through full flow
- [ ] Dialyzer clean
- [ ] Code coverage > 90%

## Next Steps

Phase 6 will implement:

- Complete test infrastructure
- Property-based tests for complex scenarios
- Performance benchmarks
- Stress testing

## Notes for Claude Code

- Providers are always thin wrappers - logic goes in core modules
- Use `rebar_state:current_app/1` to support both regular and umbrella projects
- Always clean up resources in providers (use try...after)
- Error messages should be user-friendly, not technical
- The REPL provider handles OTP version differences transparently
- Test providers can be called programmatically in tests
- Provider registration order doesn't matter
- Provider dependencies ensure correct execution order
- Use `?INFO` for user-facing messages, `?DEBUG` for developer info
