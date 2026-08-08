-module(r3lfe_prv_run_release).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1,
    info/1
]).

%% Exported for testing
-ifdef(TEST).
-export([
    get_command/1,
    get_command_args/1,
    validate_command/1,
    find_release_script/1,
    get_release_name/1,
    get_release_output_dir/1,
    build_command_line/2,
    is_interactive_command/1
]).
-endif.

-include("r3lfe.hrl").

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
        CommandArgs = get_command_args(State),

        case CommandArgs of
            [] ->
                {error, format_error(no_command)};
            [_Command | _] ->
                %% Find release script
                ReleaseScript = find_release_script(State),

                case filelib:is_file(ReleaseScript) of
                    false ->
                        {error, format_error({release_script_not_found, ReleaseScript})};
                    true ->
                        %% Execute command
                        ?INFO("Running release command: ~s",
                              [format_command_args(CommandArgs)]),

                        Result = run_release_command(ReleaseScript, CommandArgs),

                        ?DEBUG("Command result: ~p", [Result]),

                        case Result of
                            ok ->
                                {ok, State};
                            {error, RunReason} ->
                                {error, lists:flatten(format_error(RunReason))}
                        end
                end
        end
    catch
        throw:{error, ThrowReason} ->
            {error, format_error(ThrowReason)};
        error:ErrorReason:Stack ->
            ?ERROR("Run-release failed: ~p", [ErrorReason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({run_error, ErrorReason})}
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
format_error({release_command_failed, Status}) ->
    io_lib:format("Release command failed with status: ~p", [Status]);
format_error(release_command_timeout) ->
    "Release command timed out";
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

%% @doc Get the full command vector from command line arguments.
-spec get_command_args(rebar_state:t()) -> [string()].
get_command_args(State) ->
    Args = rebar_state:command_args(State),

    case Args of
        [] ->
            [];
        [_Command | Rest] ->
            [get_command(State) | Rest]
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
-spec find_release_script(rebar_state:t()) -> file:filename_all().
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
                    Name = rebar_app_info:name(AppInfo),
                    %% Name might be binary or atom
                    case is_binary(Name) of
                        true -> binary_to_list(Name);
                        false -> atom_to_list(Name)
                    end;
                [] ->
                    "myapp"  % Last resort default
            end
    end.

%% @doc Get release output directory
-spec get_release_output_dir(rebar_state:t()) -> file:filename_all().
get_release_output_dir(State) ->
    %% Check for custom output_dir in relx config
    RelxConfig = rebar_state:get(State, relx, []),

    CustomDir = proplists:get_value(output_dir, RelxConfig, undefined),

    case CustomDir of
        undefined ->
            %% Use default
            filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR);
        Dir when is_atom(Dir) ->
            atom_to_list(Dir);
        Dir ->
            %% Use custom directory
            case filename:pathtype(Dir) of
                absolute -> Dir;
                relative -> filename:join(rebar_dir:base_dir(State), Dir)
            end
    end.

%% @doc Execute a release command
-spec run_release_command(string(), [string()]) ->
    ok | {error, {release_command_failed, integer()} | release_command_timeout}.
run_release_command(ReleaseScript, CommandArgs) ->
    %% Ensure script is executable
    ok = file:change_mode(ReleaseScript, 8#755),

    ?DEBUG("Executing: ~s", [build_command_line(ReleaseScript, CommandArgs)]),

    %% Execute command
    execute_command(ReleaseScript, CommandArgs).

%% @doc Build command line string
-spec build_command_line(file:filename(), string() | [string()]) -> string().
build_command_line(ReleaseScript, CommandOrArgs) ->
    CommandArgs = normalize_command_args(CommandOrArgs),
    string:join([shell_quote(ReleaseScript) |
                 [shell_quote(Arg) || Arg <- CommandArgs]], " ").

%% @doc Execute command and handle output
-spec execute_command(string(), [string()]) ->
    ok | {error, {release_command_failed, integer()} | release_command_timeout}.
execute_command(ReleaseScript, [Command | _] = CommandArgs) ->
    %% Determine if command is interactive
    case is_interactive_command(Command) of
        true ->
            %% Interactive commands (console, attach) need special handling
            execute_interactive(build_command_line(ReleaseScript, CommandArgs));
        false ->
            execute_non_interactive(ReleaseScript, CommandArgs)
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
    _ = os:cmd(CmdLine),

    ok.

%% @doc Execute non-interactive command
-spec execute_non_interactive(string(), [string()]) ->
    ok | {error, {release_command_failed, integer()} | release_command_timeout}.
execute_non_interactive(ReleaseScript, CommandArgs) ->
    %% Use port for better output handling
    Port = open_port(
        {spawn_executable, ReleaseScript},
        [stream, exit_status, use_stdio, stderr_to_stdout, in, eof,
         {args, CommandArgs}]
    ),

    collect_output(Port).

%% @doc Collect output from port
-spec collect_output(port()) ->
    ok | {error, {release_command_failed, integer()} | release_command_timeout}.
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
                    {error, {release_command_failed, Status}}
            after 1000 ->
                ok
            end;
        {Port, {exit_status, Status}} ->
            port_close(Port),
            case Status of
                0 ->
                    ok;
                _ ->
                    ?WARN("Command exited with status: ~p", [Status]),
                    {error, {release_command_failed, Status}}
            end
    after 30000 ->
        %% Timeout after 30 seconds
        ?WARN("Command timed out", []),
        port_close(Port),
        {error, release_command_timeout}
    end.

-spec normalize_command_args(string() | [string()]) -> [string()].
normalize_command_args([]) ->
    [];
normalize_command_args([First | _] = Command) when is_integer(First) ->
    [Command];
normalize_command_args(CommandArgs) ->
    CommandArgs.

-spec shell_quote(string()) -> string().
shell_quote(Str) ->
    Escaped = re:replace(Str, "'", "'\\\\''", [global, {return, list}]),
    "'" ++ Escaped ++ "'".

-spec format_command_args([string()]) -> string().
format_command_args(CommandArgs) ->
    string:join(CommandArgs, " ").

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
