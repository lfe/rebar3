-module(r3lfe_prv_run_escript).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

%% Exported for testing
-ifdef(TEST).
-export([
    find_escript/1,
    determine_escript_path/1,
    parse_args/1,
    build_command/2
]).
-endif.

-include("r3lfe.hrl").

-define(PROVIDER, 'run-escript').
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
                    Name = rebar_app_info:name(AppInfo),
                    %% Name might be binary or atom
                    case is_binary(Name) of
                        true -> binary_to_list(Name);
                        false -> atom_to_list(Name)
                    end;
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

