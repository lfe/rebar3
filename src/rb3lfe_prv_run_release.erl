-module(rb3lfe_prv_run_release).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1,
    info/1
]).

-include_lib("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, 'run-release').
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
