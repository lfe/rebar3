-module(r3lfe_prv_run).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

%% Exported for testing
-ifdef(TEST).
-export([
    find_main_file/1,
    find_main_from_options/1,
    find_main_from_config/1,
    validate_main_file/1,
    parse_args/1,
    info/1
]).
-endif.

-include("r3lfe.hrl").

-dialyzer({no_extra_return, validate_main_file/1}).
-dialyzer({no_match, validate_main_file/1}).

-define(PROVIDER, run).
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

    %% Set up code paths (deps, plugins, and project apps)
    rebar_paths:set_paths([deps, plugins, runtime], State),

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
        throw:{error, ErrorReason} ->
            {error, format_error(ErrorReason)};
        error:ErrorReason:Stack ->
            ?ERROR("Run failed: ~p", [ErrorReason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({run_error, ErrorReason})}
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
-spec find_main_file(rebar_state:t()) -> file:filename_all() | undefined.
find_main_file(State) ->
    %% Priority: --main option > --script option > rebar.config
    case find_main_from_options(State) of
        undefined ->
            find_main_from_config(State);
        MainFile ->
            validate_main_file(MainFile)
    end.

%% @doc Check command line options
-spec find_main_from_options(rebar_state:t()) -> file:filename_all() | undefined.
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
-spec find_main_from_config(rebar_state:t()) -> file:filename_all() | undefined.
find_main_from_config(State) ->
    LfeConfig = rebar_state:get(State, lfe, []),
    proplists:get_value(main, LfeConfig).

%% @doc Validate that main file exists
-spec validate_main_file(file:filename_all() | undefined) -> file:filename_all() | undefined.
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
-spec parse_args(rebar_state:t()) -> [string()].
parse_args(State) ->
    RawArgs = rebar_state:command_args(State),

    %% Find everything after --
    case lists:dropwhile(fun(X) -> X =/= "--" end, RawArgs) of
        [] ->
            %% No -- separator, no args
            [];
        ["--" | Args] ->
            %% Return as strings (lfescript will handle conversion)
            Args
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
