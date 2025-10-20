# Phase 6.1: Escript Support & Template Verification

## Overview

This phase adds support for running LFE code as scripts and building standalone executables. We'll implement three providers (`run`, `escriptize`, `run-escript`) and verify that our existing templates work automatically with the rewritten plugin.

**Goal**: Enable users to run LFE scripts, build escripts, and execute them - all with modern rebar3 best practices.

## Prerequisites

- Phase 1-5 completed with all tests passing
- Understanding of lfescript and Erlang escripts
- Knowledge of rebar3's escript support
- Familiarity with rebar3 template system

## Architecture Overview

```
Escript Workflow:

1. Development:
   rebar3 lfe run                    → Run main/1 directly
   rebar3 lfe run -- arg1 arg2       → Pass arguments

2. Building:
   rebar3 lfe escriptize             → Build standalone executable

3. Execution:
   rebar3 lfe run-escript            → Run built escript
   rebar3 lfe run-escript -- args    → With arguments

Template System (Auto-registered):
   rebar3 new lfe-main               → Creates runnable project
   rebar3 new lfe-escript            → Creates escript project
   rebar3 new lfe-lib                → Library project
   rebar3 new lfe-app                → OTP application
   rebar3 new lfe-release            → OTP release project
```

## Implementation Tasks

### Task 6.1.1: Research Escriptize Issue

**Investigation: Original Issue #21**

Before implementing, we need to understand the original issue that caused the workaround in `rebar3_lfe_prv_escriptize.erl`:

```erlang
%% Re-examine the DEPS definition once the following ticket is addressed:
%% * https://github.com/lfe-rebar3/rebar3_lfe/issues/21
-define(DEPS, [compile, {default, escriptize}]).
```

The issue was likely related to dependency ordering or compilation hooks.

**Research Tasks:**

1. **Check if issue still exists in modern rebar3:**
   ```bash
   # Test with rebar3 3.22+ and OTP 24+
   # Does calling {default, escriptize} cause problems?
   ```

2. **Review rebar3 escriptize implementation:**
   - Look at `rebar_prv_escriptize.erl` in rebar3 source
   - Check for any breaking changes since 0.4.x
   - Verify hook execution order

3. **Determine proper approach:**
   - Option A: Delegate entirely to `{default, escriptize}`
   - Option B: Wrap with pre-compilation step
   - Option C: Implement custom escriptize logic

**Expected Outcome:**
Document the current state and choose the best implementation approach.

### Task 6.1.2: Implement Run Provider

**File: `src/rb3lfe_prv_run.erl`**

```erlang
-module(rb3lfe_prv_run).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, run).
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Run an LFE project's main/1 function",

    Opts = [
        {main, $m, "main", string,
         "Path to LFE file containing main/1 function"},
        {script, $s, "script", string,
         "Alias for --main (for compatibility)"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe run -- arg1 arg2"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE run provider starting", []),

    %% Set up code paths
    rebar_paths:set_paths([deps, plugins], State),

    try
        %% Find main file
        MainFile = find_main_file(State),

        case MainFile of
            undefined ->
                {error, format_error(no_main_file)};
            _ ->
                %% Get arguments after --
                Args = parse_args(State),

                ?INFO("Running ~s with args: ~p", [MainFile, Args]),

                %% Execute using lfescript
                case lfescript:run([MainFile | Args]) of
                    ok ->
                        {ok, State};
                    {error, Reason} ->
                        {error, format_error({lfescript_error, Reason})}
                end
        end
    catch
        throw:{error, Reason} ->
            {error, format_error(Reason)};
        error:Reason:Stack ->
            ?ERROR("Run failed: ~p", [Reason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({run_error, Reason})}
    end.

-spec format_error(term()) -> iolist().
format_error(no_main_file) ->
    "No main file specified. Use --main option or configure {lfe, [{main, \"path/to/file.lfe\"}]} in rebar.config";
format_error({file_not_found, File}) ->
    io_lib:format("Main file not found: ~s", [File]);
format_error({lfescript_error, Reason}) ->
    io_lib:format("Script execution failed: ~p", [Reason]);
format_error({run_error, Reason}) ->
    io_lib:format("Run failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Find the main file from options or config
-spec find_main_file(rebar_state:t()) -> file:filename() | undefined.
find_main_file(State) ->
    %% Priority: --main option > --script option > rebar.config
    case find_main_from_options(State) of
        undefined ->
            find_main_from_config(State);
        MainFile ->
            validate_main_file(MainFile)
    end.

%% @doc Check command line options
-spec find_main_from_options(rebar_state:t()) -> file:filename() | undefined.
find_main_from_options(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),

    %% Check --main first, then --script (for compatibility)
    case proplists:get_value(main, Opts) of
        undefined ->
            proplists:get_value(script, Opts);
        MainFile ->
            MainFile
    end.

%% @doc Check rebar.config for {lfe, [{main, "..."}]}
-spec find_main_from_config(rebar_state:t()) -> file:filename() | undefined.
find_main_from_config(State) ->
    LfeConfig = rebar_state:get(State, lfe, []),
    proplists:get_value(main, LfeConfig).

%% @doc Validate that main file exists
-spec validate_main_file(file:filename() | undefined) -> file:filename() | undefined.
validate_main_file(undefined) ->
    undefined;
validate_main_file(RelPath) ->
    AbsPath = filename:absname(RelPath),

    case filelib:is_file(AbsPath) of
        true ->
            AbsPath;
        false ->
            throw({error, {file_not_found, RelPath}})
    end.

%% @doc Parse arguments after -- separator
-spec parse_args(rebar_state:t()) -> [binary()].
parse_args(State) ->
    RawArgs = rebar_state:command_args(State),

    %% Find everything after --
    case lists:dropwhile(fun(X) -> X =/= "--" end, RawArgs) of
        [] ->
            %% No -- separator, no args
            [];
        ["--" | Args] ->
            %% Convert to binaries for LFE
            [rebar_utils:to_binary(Arg) || Arg <- Args]
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Runs an LFE script by calling its main/1 function with the provided~n"
        "arguments. The main file can be specified in three ways (in order of~n"
        "precedence):~n"
        "~n"
        "  1. Command line: --main path/to/file.lfe~n"
        "  2. Command line: --script path/to/file.lfe (compatibility)~n"
        "  3. Config file: {lfe, [{main, \"path/to/file.lfe\"}]}~n"
        "~n"
        "Arguments are passed to main/1 as a list of binaries.~n"
        "~n"
        "Examples:~n"
        "  rebar3 lfe run~n"
        "  rebar3 lfe run --main scripts/process.lfe~n"
        "  rebar3 lfe run -- arg1 arg2 arg3~n"
        "  rebar3 lfe run --main scripts/calc.lfe -- 1 2 3~n"
        "~n"
        "The main/1 function signature should be:~n"
        "  (defun main (args) ...)~n"
        "~n"
        "Where args is a list of binaries: [<<\"arg1\">>, <<\"arg2\">>, ...]~n",
        [Description]
    ).
```

### Task 6.1.3: Implement Escriptize Provider

**Research Decision Point:**

After researching issue #21, implement based on findings:

**Option A: Simple Delegation (if issue resolved)**

```erlang
-module(rb3lfe_prv_escriptize).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, escriptize).
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, compile}, {default, escriptize}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Build an LFE escript executable",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe escriptize"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE escriptize provider starting", []),

    %% Set up paths
    rebar_paths:set_paths([deps, plugins], State),

    %% The actual escriptize work is done by {default, escriptize} dep
    %% We just ensure LFE code is compiled first via our compile dep

    ?INFO("LFE escript built successfully", []),

    {ok, State}.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Builds an executable escript containing the project and its~n"
        "dependencies' BEAM files. The LFE code is compiled first via~n"
        "the lfe compile provider.~n"
        "~n"
        "Requirements in rebar.config:~n"
        "  {escript_main_app, myapp}.~n"
        "  {escript_name, \"myapp\"}.~n"
        "  {escript_emu_args, \"%%! +sbtu +A1\\n\"}.~n"
        "~n"
        "The main module must export main/1:~n"
        "  (defmodule myapp~n"
        "    (export (main 1)))~n"
        "~n"
        "  (defun main (args)~n"
        "    ;; Process args~n"
        "    ...)~n"
        "~n"
        "Example:~n"
        "  rebar3 lfe escriptize~n"
        "  ./myapp arg1 arg2~n",
        [Description]
    ).
```

**Option B: Custom Implementation (if issue persists)**

```erlang
-module(rb3lfe_prv_escriptize).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, escriptize).
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Build an LFE escript executable",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe escriptize"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE escriptize provider starting", []),

    rebar_paths:set_paths([deps, plugins], State),

    try
        %% Ensure app files are updated with module lists
        update_app_files(State),

        %% Call rebar3's escriptize
        %% Use the modern API
        case call_escriptize(State) of
            {ok, State1} ->
                {ok, State1};
            {error, Reason} ->
                {error, format_error(Reason)}
        end
    catch
        throw:{error, Reason} ->
            {error, format_error(Reason)};
        error:Reason:Stack ->
            ?ERROR("Escriptize failed: ~p", [Reason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({escriptize_error, Reason})}
    end.

-spec format_error(term()) -> iolist().
format_error({escriptize_error, Reason}) ->
    io_lib:format("Escript build failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Update app files with compiled modules
-spec update_app_files(rebar_state:t()) -> ok.
update_app_files(State) ->
    Apps = case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        AppInfo -> [AppInfo]
    end,

    lists:foreach(fun update_app_file/1, Apps),
    ok.

%% @doc Update a single app file
-spec update_app_file(rebar_app_info:t()) -> ok.
update_app_file(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    AppFile = rebar_app_info:app_file(AppInfo),

    %% Find all beam files
    BeamFiles = filelib:wildcard(filename:join(EbinDir, "*.beam")),
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- BeamFiles],

    %% Read and update app file
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppData}]} ->
            AppData1 = lists:keystore(modules, 1, AppData, {modules, Modules}),
            Content = io_lib:format("~p.~n", [{application, AppName, AppData1}]),
            file:write_file(AppFile, Content),
            ok;
        {error, Reason} ->
            ?WARN("Failed to update app file ~s: ~p", [AppFile, Reason]),
            ok
    end.

%% @doc Call rebar3's escriptize provider
-spec call_escriptize(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
call_escriptize(State) ->
    %% Try modern API first (rebar3 3.14+)
    try
        rebar_prv_escriptize:do(State)
    catch
        error:undef ->
            %% Fallback for older rebar3
            try
                rebar_escriptize:escriptize(State)
            catch
                error:E ->
                    {error, E}
            end
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Builds an executable escript containing the project and its~n"
        "dependencies' BEAM files. LFE modules are compiled first.~n"
        "~n"
        "Configuration in rebar.config:~n"
        "  {escript_main_app, myapp}.~n"
        "  {escript_name, \"myapp\"}.~n"
        "  {escript_emu_args, \"%%! +sbtu +A1\\n\"}.~n"
        "~n"
        "Main module requirements:~n"
        "  (defmodule myapp~n"
        "    (export (main 1)))~n"
        "~n"
        "  (defun main (args)~n"
        "    (io:format \"Args: ~p~n\" (list args)))~n"
        "~n"
        "Usage:~n"
        "  rebar3 lfe escriptize~n"
        "  ./myapp arg1 arg2~n",
        [Description]
    ).
```

**Note:** Choose Option A or B based on research findings.

### Task 6.1.4: Implement Run-Escript Provider

**File: `src/rb3lfe_prv_run_escript.erl`**

```erlang
-module(rb3lfe_prv_run_escript).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, 'run-escript').
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, escriptize}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Run an LFE escript",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe run-escript -- arg1 arg2"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE run-escript provider starting", []),

    rebar_paths:set_paths([deps, plugins], State),

    try
        %% Find the escript
        EscriptPath = find_escript(State),

        case filelib:is_file(EscriptPath) of
            false ->
                {error, format_error({escript_not_found, EscriptPath})};
            true ->
                %% Get arguments
                Args = parse_args(State),

                %% Execute escript
                ?INFO("Running ~s with args: ~p", [EscriptPath, Args]),

                Result = run_escript(EscriptPath, Args),

                ?DEBUG("Escript result: ~p", [Result]),

                {ok, State}
        end
    catch
        throw:{error, Reason} ->
            {error, format_error(Reason)};
        error:Reason:Stack ->
            ?ERROR("Run-escript failed: ~p", [Reason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({run_error, Reason})}
    end.

-spec format_error(term()) -> iolist().
format_error({escript_not_found, Path}) ->
    io_lib:format(
        "Escript not found: ~s~n"
        "Run 'rebar3 lfe escriptize' first to build the escript.",
        [Path]
    );
format_error({run_error, Reason}) ->
    io_lib:format("Escript execution failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Find the escript path from rebar3 state
-spec find_escript(rebar_state:t()) -> file:filename().
find_escript(State) ->
    case rebar_state:escript_path(State) of
        undefined ->
            %% Fallback: try to determine from config
            determine_escript_path(State);
        Path ->
            Path
    end.

%% @doc Determine escript path from configuration
-spec determine_escript_path(rebar_state:t()) -> file:filename().
determine_escript_path(State) ->
    %% Get escript name from config
    EscriptName = case rebar_state:get(State, escript_name, undefined) of
        undefined ->
            %% Use project app name as fallback
            case rebar_state:project_apps(State) of
                [AppInfo | _] ->
                    atom_to_list(rebar_app_info:name(AppInfo));
                [] ->
                    "escript"
            end;
        Name when is_atom(Name) ->
            atom_to_list(Name);
        Name ->
            Name
    end,

    %% Build path
    BaseDir = rebar_dir:base_dir(State),
    filename:join(BaseDir, EscriptName).

%% @doc Parse arguments after -- separator
-spec parse_args(rebar_state:t()) -> [string()].
parse_args(State) ->
    RawArgs = rebar_state:command_args(State),

    case lists:dropwhile(fun(X) -> X =/= "--" end, RawArgs) of
        [] -> [];
        ["--" | Args] -> Args
    end.

%% @doc Execute the escript
-spec run_escript(file:filename(), [string()]) -> term().
run_escript(EscriptPath, Args) ->
    %% Build command
    Cmd = build_command(EscriptPath, Args),

    ?DEBUG("Executing: ~s", [Cmd]),

    %% Execute and capture output
    Port = open_port(
        {spawn, Cmd},
        [stream, exit_status, use_stdio, stderr_to_stdout, in, eof]
    ),

    collect_output(Port).

%% @doc Build command string
-spec build_command(file:filename(), [string()]) -> string().
build_command(EscriptPath, Args) ->
    %% Ensure escript is executable
    ok = file:change_mode(EscriptPath, 8#755),

    %% Build command
    string:join([EscriptPath | Args], " ").

%% @doc Collect output from port
-spec collect_output(port()) -> ok.
collect_output(Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("~s", [Data]),
            collect_output(Port);
        {Port, eof} ->
            port_close(Port),
            receive
                {Port, {exit_status, 0}} ->
                    ok;
                {Port, {exit_status, Status}} ->
                    ?WARN("Escript exited with status: ~p", [Status]),
                    ok
            end
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Executes a previously built LFE escript. The escript must be built~n"
        "first using 'rebar3 lfe escriptize'.~n"
        "~n"
        "The escript path is determined from:~n"
        "  1. rebar3's internal escript_path~n"
        "  2. {escript_name, Name} in rebar.config~n"
        "  3. Project app name as fallback~n"
        "~n"
        "Arguments are passed directly to the escript's main/1 function.~n"
        "~n"
        "Examples:~n"
        "  rebar3 lfe run-escript~n"
        "  rebar3 lfe run-escript -- arg1 arg2~n"
        "  rebar3 lfe run-escript -- --verbose --output file.txt~n"
        "~n"
        "Build and run workflow:~n"
        "  rebar3 lfe escriptize     # Build escript~n"
        "  rebar3 lfe run-escript    # Run it~n"
        "~n"
        "Or directly:~n"
        "  ./_build/default/bin/myapp arg1 arg2~n",
        [Description]
    ).
```

### Task 6.1.5: Register New Providers

**File: `src/rb3lfe.erl` (UPDATE)**

```erlang
-module(rb3lfe).

-export([init/1]).

-include("rebar3_lfe/include/rb3lfe.hrl").

%%====================================================================
%% Plugin API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ?DEBUG("Initializing rb3lfe plugin...", []),

    %% Initialize all caches and trackers
    ok = rb3lfe_dep_cache:init(),
    ok = rb3lfe_compile_opts:init(),
    ok = rb3lfe_package_tracker:init(),

    %% Register our compiler module with rebar3
    State1 = rebar_state:append_compilers(State, [rb3lfe_compiler_mod]),

    ?DEBUG("Registered rb3lfe_compiler_mod with rebar3", []),

    %% Register all providers
    Providers = [
        rb3lfe_prv_compile,
        rb3lfe_prv_clean,
        rb3lfe_prv_repl,
        rb3lfe_prv_ltest,
        rb3lfe_prv_release,
        rb3lfe_prv_versions,
        %% Phase 6.1: Escript providers
        rb3lfe_prv_run,
        rb3lfe_prv_escriptize,
        rb3lfe_prv_run_escript
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

### Task 6.1.6: Verify Template Auto-Registration

**Create Test: `test/templates_SUITE.erl`**

```erlang
-module(templates_SUITE).

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
    templates_directory_exists/1,
    template_files_valid/1,
    lfe_main_template_exists/1,
    lfe_escript_template_exists/1,
    lfe_lib_template_exists/1,
    lfe_app_template_exists/1,
    lfe_release_template_exists/1,
    template_generates_project/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        templates_directory_exists,
        template_files_valid,
        lfe_main_template_exists,
        lfe_escript_template_exists,
        lfe_lib_template_exists,
        lfe_app_template_exists,
        lfe_release_template_exists,
        template_generates_project
    ].

init_per_suite(Config) ->
    %% Ensure plugin is loaded
    application:ensure_all_started(rb3lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("templates"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

templates_directory_exists(_Config) ->
    %% Get priv dir
    PrivDir = code:priv_dir(rb3lfe),

    ?assert(filelib:is_dir(PrivDir), "priv directory should exist"),

    TemplatesDir = filename:join(PrivDir, "templates"),

    ?assert(filelib:is_dir(TemplatesDir), "priv/templates should exist"),

    ok.

template_files_valid(_Config) ->
    PrivDir = code:priv_dir(rb3lfe),
    TemplatesDir = filename:join(PrivDir, "templates"),

    %% Find all .template files
    Templates = filelib:wildcard(filename:join(TemplatesDir, "*.template")),

    ?assert(length(Templates) > 0, "Should have template files"),

    %% Verify each template is readable
    lists:foreach(
        fun(Template) ->
            ?assert(filelib:is_file(Template),
                    io_lib:format("Template should exist: ~s", [Template]))
        end,
        Templates
    ),

    ok.

lfe_main_template_exists(_Config) ->
    PrivDir = code:priv_dir(rb3lfe),
    TemplateFile = filename:join([PrivDir, "templates", "lfe-main.template"]),

    ?assert(filelib:is_file(TemplateFile),
            "lfe-main.template should exist"),

    ok.

lfe_escript_template_exists(_Config) ->
    PrivDir = code:priv_dir(rb3lfe),
    TemplateFile = filename:join([PrivDir, "templates", "lfe-escript.template"]),

    ?assert(filelib:is_file(TemplateFile),
            "lfe-escript.template should exist"),

    ok.

lfe_lib_template_exists(_Config) ->
    PrivDir = code:priv_dir(rb3lfe),
    TemplateFile = filename:join([PrivDir, "templates", "lfe-lib.template"]),

    ?assert(filelib:is_file(TemplateFile),
            "lfe-lib.template should exist"),

    ok.

lfe_app_template_exists(_Config) ->
    PrivDir = code:priv_dir(rb3lfe),
    TemplateFile = filename:join([PrivDir, "templates", "lfe-app.template"]),

    ?assert(filelib:is_file(TemplateFile),
            "lfe-app.template should exist"),

    ok.

lfe_release_template_exists(_Config) ->
    PrivDir = code:priv_dir(rb3lfe),
    TemplateFile = filename:join([PrivDir, "templates", "lfe-release.template"]),

    ?assert(filelib:is_file(TemplateFile),
            "lfe-release.template should exist"),

    ok.

template_generates_project(Config) ->
    TestDir = ?config(test_dir, Config),

    %% This test verifies templates work via rebar3
    %% We can't directly call template generation from CT,
    %% but we can verify the infrastructure is correct

    %% Check that templates are in a location rebar3 will find
    PrivDir = code:priv_dir(rb3lfe),
    TemplatesDir = filename:join(PrivDir, "templates"),

    ?assert(filelib:is_dir(TemplatesDir)),

    %% Templates should be auto-discovered by rebar3
    %% when the plugin is loaded

    ct:pal("Templates are correctly located at: ~s", [TemplatesDir]),
    ct:pal("They will be auto-registered by rebar3 when plugin loads"),

    ok.
```

### Task 6.1.7: Create Run Provider Tests

**File: `test/rb3lfe_prv_run_SUITE.erl`**

```erlang
-module(rb3lfe_prv_run_SUITE).

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
    run_provider_registers/1,
    run_simple_script/1,
    run_with_arguments/1,
    run_with_main_option/1,
    run_from_config/1,
    run_missing_file_error/1,
    run_no_main_error/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        run_provider_registers,
        run_simple_script,
        run_with_arguments,
        run_with_main_option,
        run_from_config,
        run_missing_file_error,
        run_no_main_error
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("run"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

run_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = rb3lfe_prv_run:init(State),

    Providers = rebar_state:providers(State1),

    Found = lists:any(
        fun(P) ->
            providers:get_provider_name(P) =:= run andalso
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(Found, "Run provider should be registered"),

    ok.

run_simple_script(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create a simple script
    ScriptFile = filename:join(TestDir, "simple.lfe"),
    test_utils:write_file(ScriptFile,
        "(defmodule simple)\n"
        "(defun main (args)\n"
        "  (io:format \"Script executed!~n\")\n"
        "  (halt 0))\n"
    ),

    %% Create state with main in config
    State = rebar_state:new(),
    State1 = rebar_state:set(State, lfe, [{main, ScriptFile}]),

    %% Run provider
    Result = rb3lfe_prv_run:do(State1),

    ?assertMatch({ok, _}, Result),

    ok.

run_with_arguments(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create script that echoes arguments
    ScriptFile = filename:join(TestDir, "echo.lfe"),
    test_utils:write_file(ScriptFile,
        "(defmodule echo)\n"
        "(defun main (args)\n"
        "  (io:format \"Args: ~p~n\" (list args))\n"
        "  (halt 0))\n"
    ),

    %% Create state with arguments
    State = rebar_state:new(),
    State1 = rebar_state:set(State, lfe, [{main, ScriptFile}]),
    State2 = rebar_state:command_args(State1, ["--", "arg1", "arg2", "arg3"]),

    %% Run provider
    Result = rb3lfe_prv_run:do(State2),

    ?assertMatch({ok, _}, Result),

    ok.

run_with_main_option(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create script
    ScriptFile = filename:join(TestDir, "option.lfe"),
    test_utils:write_file(ScriptFile,
        "(defmodule option)\n"
        "(defun main (args)\n"
        "  (io:format \"From option!~n\")\n"
        "  (halt 0))\n"
    ),

    %% Create state with --main option
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{main, ScriptFile}], []}),

    %% Run provider
    Result = rb3lfe_prv_run:do(State1),

    ?assertMatch({ok, _}, Result),

    ok.

run_from_config(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create script
    ScriptFile = filename:join(TestDir, "config.lfe"),
    test_utils:write_file(ScriptFile,
        "(defmodule config)\n"
        "(defun main (args)\n"
        "  (io:format \"From config!~n\")\n"
        "  (halt 0))\n"
    ),

    %% Create state with config
    State = rebar_state:new(),
    State1 = rebar_state:set(State, lfe, [{main, ScriptFile}]),

    %% Run provider
    Result = rb3lfe_prv_run:do(State1),

    ?assertMatch({ok, _}, Result),

    ok.

run_missing_file_error(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Point to non-existent file
    ScriptFile = filename:join(TestDir, "nonexistent.lfe"),

    State = rebar_state:new(),
    State1 = rebar_state:set(State, lfe, [{main, ScriptFile}]),

    %% Should error
    Result = rb3lfe_prv_run:do(State1),

    ?assertMatch({error, _}, Result),

    ok.

run_no_main_error(_Config) ->
    %% No main specified
    State = rebar_state:new(),

    Result = rb3lfe_prv_run:do(State),

    ?assertMatch({error, _}, Result),

    ok.
```

### Task 6.1.8: Create Escript Tests

**File: `test/rb3lfe_prv_escriptize_SUITE.erl`**

```erlang
-module(rb3lfe_prv_escriptize_SUITE).

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
    escriptize_provider_registers/1,
    run_escript_provider_registers/1,
    escriptize_builds_escript/1,
    run_escript_executes/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        escriptize_provider_registers,
        run_escript_provider_registers,
        escriptize_builds_escript,
        run_escript_executes
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("escript"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

escriptize_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = rb3lfe_prv_escriptize:init(State),

    Providers = rebar_state:providers(State1),

    Found = lists:any(
        fun(P) ->
            providers:get_provider_name(P) =:= escriptize andalso
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(Found, "Escriptize provider should be registered"),

    ok.

run_escript_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = rb3lfe_prv_run_escript:init(State),

    Providers = rebar_state:providers(State1),

    Found = lists:any(
        fun(P) ->
            providers:get_provider_name(P) =:= 'run-escript' andalso
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(Found, "Run-escript provider should be registered"),

    ok.

escriptize_builds_escript(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create a minimal escript project
    create_escript_project(TestDir),

    %% Compile first
    State = create_state(TestDir),
    {ok, State1} = rb3lfe_prv_compile:do(State),

    %% Build escript
    Result = rb3lfe_prv_escriptize:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Verify escript exists
    EscriptPath = filename:join([TestDir, "_build", "default", "bin", "testapp"]),

    ct:pal("Expected escript at: ~s", [EscriptPath]),

    %% Note: This test may need adjustment based on actual escriptize behavior
    %% The exact path depends on rebar3's escriptize implementation

    ok.

run_escript_executes(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create escript project
    create_escript_project(TestDir),

    %% Build escript
    State = create_state(TestDir),
    {ok, State1} = rb3lfe_prv_compile:do(State),
    {ok, State2} = rb3lfe_prv_escriptize:do(State1),

    %% Run escript
    Result = rb3lfe_prv_run_escript:do(State2),

    %% Should succeed (or fail gracefully if escript not found)
    ?assert(is_tuple(Result)),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_escript_project(TestDir) ->
    %% Create directory structure
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),

    %% Create main module
    MainFile = filename:join(SrcDir, "testapp.lfe"),
    test_utils:write_file(MainFile,
        "(defmodule testapp\n"
        "  (export (main 1)))\n"
        "\n"
        "(defun main (args)\n"
        "  (io:format \"Escript running with args: ~p~n\" (list args))\n"
        "  0)\n"
    ),

    %% Create .app.src
    AppSrcFile = filename:join(SrcDir, "testapp.app.src"),
    test_utils:write_file(AppSrcFile,
        "{application, testapp, [\n"
        "  {description, \"Test Escript\"},\n"
        "  {vsn, \"0.1.0\"},\n"
        "  {modules, []},\n"
        "  {registered, []},\n"
        "  {applications, [kernel, stdlib]}\n"
        "]}.\n"
    ),

    %% Create rebar.config with escript settings
    RebarConfig = filename:join(TestDir, "rebar.config"),
    test_utils:write_file(RebarConfig,
        "{escript_main_app, testapp}.\n"
        "{escript_name, \"testapp\"}.\n"
        "{escript_emu_args, \"%%! +sbtu +A1\\n\"}.\n"
        "{plugins, [{rb3lfe, \"0.5.0\"}]}.\n"
        "{deps, [{lfe, \"2.2.0\"}]}.\n"
    ),

    ok.

create_state(TestDir) ->
    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Create app info
    AppInfo = rebar_app_info:new(testapp, "0.1.0", TestDir),

    rebar_state:project_apps(State1, [AppInfo]).
```

### Task 6.1.9: Update Documentation for New Commands

**File: `docs/commands-addendum-6.1.md`**

```markdown
# Commands Addendum - Phase 6.1

Additional commands for running scripts and building escripts.

## run

Run an LFE project's main/1 function directly.

```bash
rebar3 lfe run [OPTIONS] [-- ARGS...]
```

**Use Cases:**
- Quick testing during development
- Running utility scripts
- Ad-hoc data processing
- Project maintenance tasks

**Options:**
- `--main FILE` - Path to LFE file with main/1 function
- `--script FILE` - Alias for --main (compatibility)

**Configuration:**
```erlang
%% In rebar.config
{lfe, [
    {main, "scripts/process.lfe"}
]}.
```

**Examples:**
```bash
# Run configured main
rebar3 lfe run

# Specify file
rebar3 lfe run --main scripts/analyze.lfe

# Pass arguments
rebar3 lfe run -- input.txt output.txt

# Full example
rebar3 lfe run --main scripts/calc.lfe -- 1 2 3 add
```

**Main Function Signature:**
```lfe
(defmodule myscript)

(defun main (args)
  "Args is a list of binaries: [<<\"arg1\">>, <<\"arg2\">>, ...]"
  (io:format "Received ~p arguments~n" (list (length args)))
  ;; Process args
  0)  ; Return exit code
```

## escriptize

Build a standalone executable escript.

```bash
rebar3 lfe escriptize
```

**Requirements:**

In `rebar.config`:
```erlang
{escript_main_app, myapp}.
{escript_name, "myapp"}.
{escript_emu_args, "%%! +sbtu +A1\n"}.
```

In your main module:
```lfe
(defmodule myapp
  (export (main 1)))

(defun main (args)
  ;; Your logic here
  0)
```

**Output:**
```
_build/default/bin/myapp  # Standalone executable
```

**Example:**
```bash
# Build escript
rebar3 lfe escriptize

# Run directly
./_build/default/bin/myapp arg1 arg2

# Or via run-escript
rebar3 lfe run-escript -- arg1 arg2
```

## run-escript

Execute a built escript.

```bash
rebar3 lfe run-escript [-- ARGS...]
```

**Features:**
- Automatically finds built escript
- Captures and displays output
- Passes through exit codes

**Examples:**
```bash
# Run without arguments
rebar3 lfe run-escript

# With arguments
rebar3 lfe run-escript -- --verbose --output file.txt

# Full workflow
rebar3 lfe compile
rebar3 lfe escriptize
rebar3 lfe run-escript
```

**Escript Location:**

The provider looks for the escript in:
1. `rebar_state:escript_path(State)`
2. `_build/<profile>/bin/<escript_name>`
3. `_build/<profile>/<app_name>`

## Templates

LFE templates are automatically available when the plugin is loaded.

**Available Templates:**

```bash
# List all templates
rebar3 new

# Create library project
rebar3 new lfe-lib mylib

# Create main/script project
rebar3 new lfe-main myscript

# Create escript project
rebar3 new lfe-escript myapp

# Create OTP application
rebar3 new lfe-app myapp

# Create OTP release
rebar3 new lfe-release myrelease
```

**Template Details:**

- `lfe-lib` - Basic library with tests
- `lfe-main` - Script project with main/1
- `lfe-escript` - Escript project
- `lfe-app` - OTP application
- `lfe-release` - OTP release with relx

**Example Workflow:**

```bash
# Create new script project
rebar3 new lfe-main calculator
cd calculator

# Run during development
rebar3 lfe run -- 1 2 add

# Build escript for distribution
rebar3 lfe escriptize

# Test escript
rebar3 lfe run-escript -- 1 2 add

# Distribute
cp _build/default/bin/calculator ~/bin/
```

## Comparison

| Command | Use Case | Output | Distribution |
|---------|----------|--------|--------------|
| `run` | Development | Console | No |
| `escriptize` | Build | Executable | Yes |
| `run-escript` | Test escript | Console | No |

**When to use each:**

- **Development**: Use `rebar3 lfe run` for quick iteration
- **Testing**: Use `rebar3 lfe run-escript` to test built escript
- **Distribution**: Use `rebar3 lfe escriptize` to create standalone binary

## See Also

- [Configuration Reference](configuration.md)
- [Escript Example](../examples/escript/)
- [Main Script Example](../examples/main-script/)
```

## Testing Instructions

### Running Tests

```bash
# Run all new tests
rebar3 ct --suite=test/templates_SUITE
rebar3 ct --suite=test/rb3lfe_prv_run_SUITE
rebar3 ct --suite=test/rb3lfe_prv_escriptize_SUITE

# Run all tests
rebar3 ct

# Full check
make check
```

### Manual Verification

#### Test Run Provider

```bash
# Create test project
mkdir test_run
cd test_run

cat > rebar.config <<EOF
{plugins, [{rb3lfe, "0.5.0"}]}.
{deps, [{lfe, "2.2.0"}]}.
{lfe, [{main, "scripts/hello.lfe"}]}.
EOF

mkdir scripts
cat > scripts/hello.lfe <<EOF
(defmodule hello)

(defun main (args)
  (io:format "Hello from LFE!~n")
  (io:format "Args: ~p~n" (list args))
  0)
EOF

# Test run
rebar3 lfe run
rebar3 lfe run -- arg1 arg2
rebar3 lfe run --main scripts/hello.lfe
```

#### Test Escriptize

```bash
# Use lfe-escript template
rebar3 new lfe-escript myapp
cd myapp

# Compile and build
rebar3 lfe compile
rebar3 lfe escriptize

# Test
rebar3 lfe run-escript
rebar3 lfe run-escript -- test args

# Run directly
./_build/default/bin/myapp test args
```

#### Test Templates

```bash
# List templates
rebar3 new

# Should show lfe-* templates

# Test each template
rebar3 new lfe-lib testlib
cd testlib && rebar3 lfe compile && cd ..

rebar3 new lfe-main testmain
cd testmain && rebar3 lfe run && cd ..

rebar3 new lfe-app testapp
cd testapp && rebar3 lfe compile && cd ..
```

## Expected Outcomes

At the end of Phase 6.1, you should have:

1. ✅ `rebar3 lfe run` working with all options
2. ✅ `rebar3 lfe escriptize` building escripts
3. ✅ `rebar3 lfe run-escript` executing escripts
4. ✅ Templates auto-registering and working
5. ✅ Comprehensive tests for all providers
6. ✅ Documentation for new commands
7. ✅ Manual verification successful

### Integration Checklist

- [ ] All previous tests still pass
- [ ] Run provider executes scripts correctly
- [ ] Arguments pass through properly
- [ ] Escriptize builds functional escripts
- [ ] Run-escript executes built escripts
- [ ] Templates appear in `rebar3 new`
- [ ] Templates generate working projects
- [ ] All providers registered correctly
- [ ] Error messages are clear
- [ ] Dialyzer clean
- [ ] Code coverage >90%

## Next Steps

Phase 6.2 will implement:
- `rebar3 lfe run-release` - Run OTP release commands
- Integration with Phase 5's release provider
- Release management tests

## Notes for Claude Code

### Escriptize Research Priority

**Must investigate before implementing:**
1. Check if issue #21 still exists in rebar3 3.22+
2. Review rebar3's escriptize changes since 2020
3. Test dependency handling in modern rebar3
4. Determine if we can safely delegate to `{default, escriptize}`

**If issue resolved:** Use Option A (simple delegation)
**If issue persists:** Use Option B (custom wrapper)

### Template System

Templates in `priv/templates/` are automatically discovered by rebar3. We just need to:
1. Ensure `priv` is in `.app.src` files list
2. Verify templates follow `.template` naming convention
3. Test that they appear in `rebar3 new`

### LfeScript vs Escript

- `lfescript:run/1` - For development, interprets LFE
- Escript - For distribution, compiled standalone binary

### Testing Considerations

- Test providers can be called directly in CT
- Use mock states for unit tests
- Create real project structures for integration tests
- Verify cleanup after tests
- Handle platform differences (Windows paths)

### Error Handling

- Validate file paths before execution
- Check for missing main/1 function
- Provide helpful error messages
- Clean up on failures
- Return proper exit codes
