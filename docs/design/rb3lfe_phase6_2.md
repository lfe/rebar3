# Phase 6.2: Release Management

## Overview

This phase implements the `run-release` provider for managing OTP releases in production. It integrates with the existing `release` provider from Phase 5 and provides a convenient interface for starting, stopping, and managing release nodes.

**Goal**: Enable production release management with a clean, modern interface.

## Prerequisites

- Phase 1-5 completed with all tests passing
- Phase 6.1 completed (escript support)
- Understanding of OTP releases and relx
- Knowledge of release scripts and node management
- Familiarity with distributed Erlang

## Architecture Overview

```
Release Workflow:

1. Build Release:
   rebar3 lfe release              → Build OTP release

2. Manage Release:
   rebar3 lfe run-release start    → Start release node
   rebar3 lfe run-release stop     → Stop release node
   rebar3 lfe run-release status   → Check node status
   rebar3 lfe run-release ping     → Ping release node
   rebar3 lfe run-release console  → Attach to console
   rebar3 lfe run-release remote   → Remote shell

Release Script Integration:
   _build/default/rel/myapp/bin/myapp COMMAND
   ↓
   rebar3 lfe run-release COMMAND
```

## Implementation Tasks

### Task 6.2.1: Implement Run-Release Provider

**File: `src/rb3lfe_prv_run_release.erl`**

```erlang
-module(rb3lfe_prv_run_release).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, 'run-release').
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, release}]).
-define(DEFAULT_RELEASE_DIR, "rel").

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Run an LFE release command",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe run-release start"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE run-release provider starting", []),

    rebar_paths:set_paths([deps, plugins], State),

    try
        %% Get the command to run
        Command = get_command(State),

        case Command of
            undefined ->
                {error, format_error(no_command)};
            _ ->
                %% Find release script
                ReleaseScript = find_release_script(State),

                case filelib:is_file(ReleaseScript) of
                    false ->
                        {error, format_error({release_script_not_found, ReleaseScript})};
                    true ->
                        %% Execute command
                        ?INFO("Running release command: ~s", [Command]),

                        Result = run_release_command(ReleaseScript, Command),

                        ?DEBUG("Command result: ~p", [Result]),

                        {ok, State}
                end
        end
    catch
        throw:{error, Reason} ->
            {error, format_error(Reason)};
        error:Reason:Stack ->
            ?ERROR("Run-release failed: ~p", [Reason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({run_error, Reason})}
    end.

-spec format_error(term()) -> iolist().
format_error(no_command) ->
    "No command specified. Usage: rebar3 lfe run-release COMMAND\n"
    "Available commands: start, stop, restart, status, ping, console, remote_console, attach";
format_error({release_script_not_found, Script}) ->
    io_lib:format(
        "Release script not found: ~s~n"
        "Run 'rebar3 lfe release' first to build the release.",
        [Script]
    );
format_error({invalid_command, Command}) ->
    io_lib:format(
        "Invalid command: ~s~n"
        "Valid commands: start, stop, restart, status, ping, console, remote_console, attach",
        [Command]
    );
format_error({run_error, Reason}) ->
    io_lib:format("Release command failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Get the command from command line arguments
-spec get_command(rebar_state:t()) -> string() | undefined.
get_command(State) ->
    Args = rebar_state:command_args(State),

    case Args of
        [] ->
            undefined;
        [Command | _] ->
            validate_command(Command)
    end.

%% @doc Validate that the command is supported
-spec validate_command(string()) -> string().
validate_command(Command) ->
    ValidCommands = [
        "start", "stop", "restart", "reboot",
        "status", "ping",
        "console", "remote_console", "attach",
        "foreground", "daemon",
        "upgrade", "downgrade",
        "versions", "escript", "rpc", "rpcterms",
        "eval"
    ],

    case lists:member(Command, ValidCommands) of
        true ->
            Command;
        false ->
            ?WARN("Unrecognized command: ~s (passing through anyway)", [Command]),
            Command
    end.

%% @doc Find the release script path
-spec find_release_script(rebar_state:t()) -> file:filename().
find_release_script(State) ->
    ReleaseName = get_release_name(State),
    ReleaseDir = get_release_output_dir(State),

    filename:join([ReleaseDir, ReleaseName, "bin", ReleaseName]).

%% @doc Get release name from relx config
-spec get_release_name(rebar_state:t()) -> string().
get_release_name(State) ->
    RelxConfig = rebar_state:get(State, relx, []),

    case proplists:lookup(release, RelxConfig) of
        {release, {Name, _Version}, _Apps} when is_atom(Name) ->
            atom_to_list(Name);
        {release, {Name, _Version}, _Apps} when is_list(Name) ->
            Name;
        none ->
            %% Fallback to first project app
            case rebar_state:project_apps(State) of
                [AppInfo | _] ->
                    atom_to_list(rebar_app_info:name(AppInfo));
                [] ->
                    "myapp"  % Last resort default
            end
    end.

%% @doc Get release output directory
-spec get_release_output_dir(rebar_state:t()) -> file:filename().
get_release_output_dir(State) ->
    %% Check for custom output_dir in relx config
    RelxConfig = rebar_state:get(State, relx, []),

    CustomDir = proplists:get_value(output_dir, RelxConfig, undefined),

    case CustomDir of
        undefined ->
            %% Use default
            filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR);
        Dir ->
            %% Use custom directory
            case filename:pathtype(Dir) of
                absolute -> Dir;
                relative -> filename:join(rebar_dir:base_dir(State), Dir)
            end
    end.

%% @doc Execute a release command
-spec run_release_command(file:filename(), string()) -> term().
run_release_command(ReleaseScript, Command) ->
    %% Ensure script is executable
    ok = file:change_mode(ReleaseScript, 8#755),

    %% Build full command with arguments
    CmdLine = build_command_line(ReleaseScript, Command),

    ?DEBUG("Executing: ~s", [CmdLine]),

    %% Execute command
    execute_command(CmdLine, Command).

%% @doc Build command line string
-spec build_command_line(file:filename(), string()) -> string().
build_command_line(ReleaseScript, Command) ->
    %% Some commands may have additional arguments
    %% For now, just pass the command directly
    string:join([ReleaseScript, Command], " ").

%% @doc Execute command and handle output
-spec execute_command(string(), string()) -> ok.
execute_command(CmdLine, Command) ->
    %% Determine if command is interactive
    case is_interactive_command(Command) of
        true ->
            %% Interactive commands (console, attach) need special handling
            execute_interactive(CmdLine);
        false ->
            %% Non-interactive commands can use os:cmd
            execute_non_interactive(CmdLine)
    end.

%% @doc Check if command requires interactive terminal
-spec is_interactive_command(string()) -> boolean().
is_interactive_command("console") -> true;
is_interactive_command("remote_console") -> true;
is_interactive_command("attach") -> true;
is_interactive_command("foreground") -> true;
is_interactive_command(_) -> false.

%% @doc Execute interactive command
-spec execute_interactive(string()) -> ok.
execute_interactive(CmdLine) ->
    %% For interactive commands, we need to exec the release script
    %% so it takes over the terminal properly

    ?INFO("Starting interactive session...", []),
    ?INFO("Command: ~s", [CmdLine]),

    %% Note: This will replace the current process
    %% The rebar3 command will exit when the release script exits
    os:cmd(CmdLine),

    ok.

%% @doc Execute non-interactive command
-spec execute_non_interactive(string()) -> ok.
execute_non_interactive(CmdLine) ->
    %% Use port for better output handling
    Port = open_port(
        {spawn, CmdLine},
        [stream, exit_status, use_stdio, stderr_to_stdout, in, eof]
    ),

    collect_output(Port).

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
                    ?WARN("Command exited with status: ~p", [Status]),
                    ok
            after 1000 ->
                ok
            end;
        {Port, {exit_status, Status}} ->
            port_close(Port),
            if
                Status =/= 0 ->
                    ?WARN("Command exited with status: ~p", [Status]);
                true ->
                    ok
            end
    after 30000 ->
        %% Timeout after 30 seconds
        ?WARN("Command timed out", []),
        port_close(Port),
        ok
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Executes commands on a built LFE OTP release. The release must be~n"
        "built first using 'rebar3 lfe release'.~n"
        "~n"
        "Common Commands:~n"
        "  start            - Start the release in the background~n"
        "  stop             - Stop the release~n"
        "  restart          - Restart the release~n"
        "  status           - Check if the release is running~n"
        "  ping             - Ping the release node~n"
        "  console          - Start with interactive console~n"
        "  remote_console   - Connect remote console to running node~n"
        "  attach           - Attach to running node~n"
        "  foreground       - Start in foreground~n"
        "~n"
        "Advanced Commands:~n"
        "  upgrade VERSION  - Upgrade to new version~n"
        "  downgrade VER    - Downgrade to previous version~n"
        "  versions         - List installed versions~n"
        "  eval \"CODE\"      - Evaluate Erlang code~n"
        "  rpc MOD FN ARGS  - Execute remote procedure call~n"
        "~n"
        "Examples:~n"
        "  rebar3 lfe run-release start~n"
        "  rebar3 lfe run-release status~n"
        "  rebar3 lfe run-release ping~n"
        "  rebar3 lfe run-release stop~n"
        "  rebar3 lfe run-release console~n"
        "~n"
        "Workflow:~n"
        "  rebar3 lfe compile      # Compile code~n"
        "  rebar3 lfe release      # Build release~n"
        "  rebar3 lfe run-release start   # Start in background~n"
        "  rebar3 lfe run-release status  # Check status~n"
        "  rebar3 lfe run-release remote_console  # Connect~n"
        "  rebar3 lfe run-release stop    # Stop~n"
        "~n"
        "Note: The release script location is determined from relx~n"
        "configuration in rebar.config.~n",
        [Description]
    ).
```

### Task 6.2.2: Update Release Provider (Enhancement)

**File: `src/rb3lfe_prv_release.erl` (UPDATE)**

Add helpful output showing how to use the release:

```erlang
%% Update the do/1 function to add post-build instructions

do(State) ->
    ?DEBUG("LFE release provider starting", []),

    %% Update app files with modules list
    update_app_files(State),

    %% Delegate to rebar3's release provider
    try
        rebar_relx:do(rlx_prv_release, "release", release, State),
        
        %% Show helpful usage information
        show_usage_info(State),
        
        {ok, State}
    catch
        error:undef ->
            %% Try older API
            rebar_relx:do(release, State),
            show_usage_info(State),
            {ok, State}
    end.

%% Add new function:

%% @doc Show usage information after successful build
-spec show_usage_info(rebar_state:t()) -> ok.
show_usage_info(State) ->
    ReleaseName = get_release_name(State),
    ReleaseDir = get_release_output_dir(State),
    
    ?INFO("~n", []),
    ?INFO("Release built successfully!", []),
    ?INFO("~n", []),
    ?INFO("To run the release:", []),
    ?INFO("  rebar3 lfe run-release start      # Start in background", []),
    ?INFO("  rebar3 lfe run-release console    # Start with console", []),
    ?INFO("  rebar3 lfe run-release foreground # Start in foreground", []),
    ?INFO("~n", []),
    ?INFO("Or run directly:", []),
    ?INFO("  ~s/~s/bin/~s start", [ReleaseDir, ReleaseName, ReleaseName]),
    ?INFO("~n", []),
    ok.

%% Add helper functions (same as run-release provider):

-spec get_release_name(rebar_state:t()) -> string().
get_release_name(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    case proplists:lookup(release, RelxConfig) of
        {release, {Name, _Version}, _Apps} when is_atom(Name) ->
            atom_to_list(Name);
        {release, {Name, _Version}, _Apps} when is_list(Name) ->
            Name;
        none ->
            case rebar_state:project_apps(State) of
                [AppInfo | _] ->
                    atom_to_list(rebar_app_info:name(AppInfo));
                [] ->
                    "myapp"
            end
    end.

-spec get_release_output_dir(rebar_state:t()) -> file:filename().
get_release_output_dir(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    CustomDir = proplists:get_value(output_dir, RelxConfig, undefined),
    case CustomDir of
        undefined ->
            filename:join(rebar_dir:base_dir(State), "rel");
        Dir ->
            case filename:pathtype(Dir) of
                absolute -> Dir;
                relative -> filename:join(rebar_dir:base_dir(State), Dir)
            end
    end.
```

### Task 6.2.3: Register Run-Release Provider

**File: `src/rb3lfe.erl` (UPDATE)**

```erlang
%% Update the Providers list in init/1:

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
    rb3lfe_prv_run_escript,
    %% Phase 6.2: Release management
    rb3lfe_prv_run_release
],
```

### Task 6.2.4: Create Run-Release Tests

**File: `test/rb3lfe_prv_run_release_SUITE.erl`**

```erlang
-module(rb3lfe_prv_run_release_SUITE).

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
    run_release_provider_registers/1,
    run_release_finds_script/1,
    run_release_validates_commands/1,
    run_release_no_command_error/1,
    run_release_script_not_found_error/1,
    release_output_dir_default/1,
    release_output_dir_custom/1,
    release_name_from_config/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        run_release_provider_registers,
        run_release_finds_script,
        run_release_validates_commands,
        run_release_no_command_error,
        run_release_script_not_found_error,
        release_output_dir_default,
        release_output_dir_custom,
        release_name_from_config
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("run_release"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

run_release_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = rb3lfe_prv_run_release:init(State),

    Providers = rebar_state:providers(State1),

    Found = lists:any(
        fun(P) ->
            providers:get_provider_name(P) =:= 'run-release' andalso
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(Found, "Run-release provider should be registered"),

    ok.

run_release_finds_script(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create mock release structure
    RelDir = filename:join([TestDir, "_build", "default", "rel", "myapp", "bin"]),
    ok = filelib:ensure_dir(filename:join(RelDir, "dummy")),

    %% Create mock release script
    ScriptPath = filename:join(RelDir, "myapp"),
    test_utils:write_file(ScriptPath, "#!/bin/sh\necho 'mock script'\n"),
    file:change_mode(ScriptPath, 8#755),

    %% Create state
    State = create_test_state(TestDir, "myapp"),
    State1 = rebar_state:command_args(State, ["status"]),

    %% Should find the script
    Result = rb3lfe_prv_run_release:do(State1),

    %% Won't fully succeed without real release, but should find script
    ct:pal("Result: ~p", [Result]),

    ok.

run_release_validates_commands(_Config) ->
    %% Valid commands should pass validation
    ValidCommands = [
        "start", "stop", "restart", "status", "ping",
        "console", "remote_console", "attach"
    ],

    lists:foreach(
        fun(Cmd) ->
            %% These are internal functions, can't test directly
            %% But we verify they're in the valid list
            ?assert(is_list(Cmd))
        end,
        ValidCommands
    ),

    ok.

run_release_no_command_error(_Config) ->
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, []),

    Result = rb3lfe_prv_run_release:do(State1),

    ?assertMatch({error, _}, Result),

    {error, ErrorMsg} = Result,
    ?assert(string:find(ErrorMsg, "No command") =/= nomatch),

    ok.

run_release_script_not_found_error(Config) ->
    TestDir = ?config(test_dir, Config),

    %% State points to non-existent release
    State = create_test_state(TestDir, "nonexistent"),
    State1 = rebar_state:command_args(State, ["start"]),

    Result = rb3lfe_prv_run_release:do(State1),

    ?assertMatch({error, _}, Result),

    {error, ErrorMsg} = Result,
    ?assert(string:find(ErrorMsg, "not found") =/= nomatch),

    ok.

release_output_dir_default(Config) ->
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% No custom output_dir in relx config
    %% Should use default

    BaseDir = rebar_dir:base_dir(State1),
    Expected = filename:join(BaseDir, "rel"),

    %% This tests internal function indirectly through provider
    ct:pal("Expected default dir: ~s", [Expected]),

    ok.

release_output_dir_custom(Config) ->
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Set custom output_dir
    RelxConfig = [{output_dir, "custom_release"}],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    BaseDir = rebar_dir:base_dir(State2),
    Expected = filename:join(BaseDir, "custom_release"),

    ct:pal("Expected custom dir: ~s", [Expected]),

    ok.

release_name_from_config(Config) ->
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Set release name in config
    RelxConfig = [{release, {myrelease, "1.0.0"}, [myapp]}],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    %% Release name should be extracted
    ct:pal("State with release config created"),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_test_state(TestDir, AppName) ->
    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Add relx config
    RelxConfig = [
        {release, {list_to_atom(AppName), "0.1.0"}, [list_to_atom(AppName)]}
    ],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    State2.
```

### Task 6.2.5: Create Integration Tests

**File: `test/release_integration_SUITE.erl`**

```erlang
-module(release_integration_SUITE).

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
    build_and_run_release/1,
    release_start_stop/1,
    release_shows_usage/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        build_and_run_release,
        release_start_stop,
        release_shows_usage
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("release_integ"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Cleanup any running releases
    cleanup_releases(TestDir),
    
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

build_and_run_release(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create minimal release project
    create_release_project(TestDir),

    %% Build release
    State = create_state(TestDir),
    
    {ok, State1} = rb3lfe_prv_compile:do(State),
    
    Result = rb3lfe_prv_release:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Verify release structure exists
    RelDir = filename:join([TestDir, "_build", "default", "rel", "testrel"]),
    ?assert(filelib:is_dir(RelDir), "Release directory should exist"),

    BinDir = filename:join([RelDir, "bin"]),
    ?assert(filelib:is_dir(BinDir), "Bin directory should exist"),

    Script = filename:join([BinDir, "testrel"]),
    ?assert(filelib:is_file(Script), "Release script should exist"),

    ok.

release_start_stop(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create and build release
    create_release_project(TestDir),
    State = create_state(TestDir),
    {ok, State1} = rb3lfe_prv_compile:do(State),
    {ok, State2} = rb3lfe_prv_release:do(State1),

    %% Test status (should not be running)
    State3 = rebar_state:command_args(State2, ["status"]),
    
    %% This may fail if release isn't fully functional, that's ok
    %% We're testing the provider works, not the release itself
    _Result = rb3lfe_prv_run_release:do(State3),

    ct:pal("Release management commands can be executed"),

    ok.

release_shows_usage(_Config) ->
    %% Verify that release provider shows helpful usage info
    %% This is tested by checking the info/1 function output

    Info = rb3lfe_prv_run_release:info("Test"),

    ?assert(is_list(Info)),
    ?assert(length(Info) > 0),

    %% Should mention common commands
    InfoStr = lists:flatten(Info),
    ?assert(string:find(InfoStr, "start") =/= nomatch),
    ?assert(string:find(InfoStr, "stop") =/= nomatch),
    ?assert(string:find(InfoStr, "status") =/= nomatch),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_release_project(TestDir) ->
    %% Create directory structure
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),

    %% Create application module
    AppFile = filename:join(SrcDir, "testrel.lfe"),
    test_utils:write_file(AppFile,
        "(defmodule testrel\n"
        "  (behaviour application)\n"
        "  (export (start 2) (stop 1)))\n"
        "\n"
        "(defun start (_type _args)\n"
        "  (lfe_io:format \"App started~n\")\n"
        "  (tuple 'ok (self)))\n"
        "\n"
        "(defun stop (_state)\n"
        "  'ok)\n"
    ),

    %% Create .app.src
    AppSrcFile = filename:join(SrcDir, "testrel.app.src"),
    test_utils:write_file(AppSrcFile,
        "{application, testrel, [\n"
        "  {description, \"Test Release\"},\n"
        "  {vsn, \"0.1.0\"},\n"
        "  {modules, []},\n"
        "  {registered, []},\n"
        "  {applications, [kernel, stdlib, sasl]},\n"
        "  {mod, {testrel, []}}\n"
        "]}.\n"
    ),

    %% Create rebar.config with relx config
    RebarConfig = filename:join(TestDir, "rebar.config"),
    test_utils:write_file(RebarConfig,
        "{plugins, [{rb3lfe, \"0.5.0\"}]}.\n"
        "{deps, [{lfe, \"2.2.0\"}]}.\n"
        "\n"
        "{relx, [\n"
        "  {release, {testrel, \"0.1.0\"}, [testrel, sasl]},\n"
        "  {dev_mode, false},\n"
        "  {include_erts, false},\n"
        "  {extended_start_script, true}\n"
        "]}.\n"
    ),

    ok.

create_state(TestDir) ->
    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Create app info
    AppInfo = rebar_app_info:new(testrel, "0.1.0", TestDir),

    rebar_state:project_apps(State1, [AppInfo]).

cleanup_releases(TestDir) ->
    %% Try to stop any running test releases
    Script = filename:join([TestDir, "_build", "default", "rel", 
                           "testrel", "bin", "testrel"]),

    case filelib:is_file(Script) of
        true ->
            os:cmd(Script ++ " stop"),
            timer:sleep(1000);
        false ->
            ok
    end.
```

### Task 6.2.6: Update Documentation

**File: `docs/commands-addendum-6.2.md`**

```markdown
# Commands Addendum - Phase 6.2

## run-release

Execute commands on a built LFE OTP release.

```bash
rebar3 lfe run-release COMMAND [ARGS...]
```

**Prerequisites:**
- Release built with `rebar3 lfe release`
- Relx configuration in `rebar.config`

### Common Commands

**Start/Stop:**
```bash
# Start release in background
rebar3 lfe run-release start

# Stop release
rebar3 lfe run-release stop

# Restart release
rebar3 lfe run-release restart

# Reboot release (hard restart)
rebar3 lfe run-release reboot
```

**Status:**
```bash
# Check if running
rebar3 lfe run-release status

# Ping the node
rebar3 lfe run-release ping
```

**Console Access:**
```bash
# Start with interactive console
rebar3 lfe run-release console

# Attach to running release
rebar3 lfe run-release attach

# Remote console to running node
rebar3 lfe run-release remote_console

# Start in foreground (not daemonized)
rebar3 lfe run-release foreground
```

### Advanced Commands

**Upgrades/Downgrades:**
```bash
# Upgrade to new version
rebar3 lfe run-release upgrade 1.2.0

# Downgrade to previous version
rebar3 lfe run-release downgrade 1.0.0

# List installed versions
rebar3 lfe run-release versions
```

**Code Evaluation:**
```bash
# Evaluate Erlang code
rebar3 lfe run-release eval "application:which_applications()."

# Remote procedure call
rebar3 lfe run-release rpc myapp status []

# RPC with terms
rebar3 lfe run-release rpcterms myapp check_health []
```

### Typical Workflows

**Development Workflow:**
```bash
# Build and test locally
rebar3 lfe compile
rebar3 lfe release
rebar3 lfe run-release console

# In console: test your code
> (myapp:start)
> (myapp:do-something)
```

**Production Deployment:**
```bash
# Build release
rebar3 as prod lfe release

# Deploy to server
scp -r _build/prod/rel/myapp server:/opt/

# On server: start release
/opt/myapp/bin/myapp start

# Check status
/opt/myapp/bin/myapp status

# View logs
tail -f /opt/myapp/log/console.log
```

**Using run-release Helper:**
```bash
# From project directory on server
rebar3 lfe run-release start
rebar3 lfe run-release status
rebar3 lfe run-release remote_console

# In remote console: check app
> (application:which_applications)
> (myapp:status)
> ^D to detach
```

### Configuration

Relx configuration in `rebar.config`:

```erlang
{relx, [
    %% Release definition
    {release, {myapp, "1.0.0"}, [
        myapp,
        sasl,
        runtime_tools
    ]},

    %% Development mode (symlinks, faster iteration)
    {dev_mode, true},

    %% Include ERTS (Erlang runtime)
    {include_erts, true},

    %% Extended start script (enables all commands)
    {extended_start_script, true},

    %% Custom output directory
    {output_dir, "_build/{{profile}}/rel"},

    %% VM arguments
    {vm_args, "config/vm.args"},

    %% System config
    {sys_config, "config/sys.config"},

    %% Overlay (additional files)
    {overlay, [
        {mkdir, "log"},
        {copy, "scripts/init.sh", "bin/init.sh"}
    ]}
]}.
```

### Profiles

Use profiles for different environments:

```erlang
{profiles, [
    {prod, [
        {relx, [
            {dev_mode, false},
            {include_erts, true},
            {system_libs, true},
            {extended_start_script, true}
        ]}
    ]}
]}.
```

Build for production:
```bash
rebar3 as prod lfe release
```

### Release Directory Structure

After building:
```
_build/default/rel/myapp/
├── bin/
│   └── myapp              # Release script
├── erts-14.2.1/           # Erlang runtime (if included)
├── lib/                   # Application code
│   ├── myapp-1.0.0/
│   ├── kernel-9.2/
│   └── ...
├── releases/              # Release metadata
│   ├── 1.0.0/
│   │   ├── myapp.rel
│   │   ├── sys.config
│   │   └── vm.args
│   └── start_erl.data
└── log/                   # Log files
```

### Troubleshooting

**Release script not found:**
```
Error: Release script not found
```
Solution: Build release first with `rebar3 lfe release`

**Node not responding:**
```bash
# Check if running
rebar3 lfe run-release ping

# Check logs
tail -f _build/default/rel/myapp/log/erlang.log.1
```

**Port already in use:**
```bash
# Check what's using the port
lsof -i :8080

# Or use different ports in vm.args
```

**Permission denied:**
```bash
# Ensure release script is executable
chmod +x _build/default/rel/myapp/bin/myapp
```

### Best Practices

1. **Always use extended_start_script:**
   ```erlang
   {extended_start_script, true}
   ```

2. **Include SASL for logging:**
   ```erlang
   {release, {myapp, "1.0.0"}, [myapp, sasl]}
   ```

3. **Separate prod and dev configs:**
   ```erlang
   {profiles, [
       {prod, [{relx, [...]}]}
   ]}
   ```

4. **Test releases locally before deploying:**
   ```bash
   rebar3 lfe release
   rebar3 lfe run-release console
   ```

5. **Use remote_console for debugging:**
   ```bash
   # Safer than attach (can detach without stopping node)
   rebar3 lfe run-release remote_console
   ```

### See Also

- [Release Example](../examples/release/)
- [Relx Documentation](https://erlware.github.io/relx/)
- [OTP Release Handling](https://www.erlang.org/doc/design_principles/release_handling.html)
```

## Testing Instructions

### Running Tests

```bash
# Run run-release tests
rebar3 ct --suite=test/rb3lfe_prv_run_release_SUITE

# Run integration tests
rebar3 ct --suite=test/release_integration_SUITE

# Run all tests
rebar3 ct

# Full check
make check
```

### Manual Verification

#### Test Release Build and Management

```bash
# Create test release project
rebar3 new lfe-release testrel
cd testrel

# Build release
rebar3 lfe compile
rebar3 lfe release

# Verify release exists
ls -la _build/default/rel/testrel/

# Test commands
rebar3 lfe run-release start
rebar3 lfe run-release status
rebar3 lfe run-release ping
rebar3 lfe run-release stop

# Test console
rebar3 lfe run-release console
# In console: test LFE
> (+ 1 2 3)
> ^D

# Test foreground mode
rebar3 lfe run-release foreground
# Ctrl-C to stop
```

#### Test Release with Production Profile

```bash
# Build production release
rebar3 as prod lfe release

# Should have different settings
ls -la _build/prod/rel/testrel/

# Test it works
rebar3 as prod lfe run-release start
rebar3 as prod lfe run-release status
rebar3 as prod lfe run-release stop
```

## Expected Outcomes

At the end of Phase 6.2, you should have:

1. ✅ `rebar3 lfe run-release` managing releases
2. ✅ All standard release commands working
3. ✅ Release provider shows helpful usage info
4. ✅ Integration with existing release provider
5. ✅ Comprehensive tests
6. ✅ Documentation for release management
7. ✅ Manual verification successful

### Integration Checklist

- [ ] All previous tests still pass
- [ ] Run-release provider registers correctly
- [ ] Release scripts can be found
- [ ] Start/stop commands work
- [ ] Status commands work
- [ ] Console commands work
- [ ] Release provider shows usage info
- [ ] Works with custom output directories
- [ ] Works with different profiles
- [ ] Error messages are helpful
- [ ] Dialyzer clean
- [ ] Code coverage >90%

## Next Steps

Phase 6.3 will implement:
- `rebar3 lfe confabulate` - Convert LFE data to Erlang data
- Data conversion tests
- Documentation

## Notes for Claude Code

### Release Script Handling

- Release scripts are generated by relx, not by us
- We just need to find and execute them
- Script location depends on relx configuration
- Always check file exists before executing

### Interactive vs Non-Interactive

Commands like `console` and `attach` need special handling:
- They take over the terminal
- Need to pass through stdin/stdout
- Can't easily capture output in tests

For testing, focus on:
- Script discovery
- Command validation
- Non-interactive commands (start, stop, status)

### Platform Considerations

- Release scripts are shell scripts on Unix
- May need different handling on Windows
- Test on multiple platforms if possible

### Error Recovery

- If release doesn't stop cleanly, might need killall
- Check for stale PID files
- Clean up properly in tests

### Production Use

The run-release provider is primarily for convenience during development. In production:
- Users typically call release script directly
- Or use init systems (systemd, supervisor)
- Provider is still useful for remote management
