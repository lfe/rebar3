-module(rebar3_lfe_prv_confabulate).

-export([init/1,
         do/1,
         format_error/1]).

-define(NAMESPACE, lfe).
-define(PROVIDER, confabulate).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Convert the given LFE data file(s) to Erlang file(s).",
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                {namespace, ?NAMESPACE},
                {name, ?PROVIDER},
                {module, ?MODULE},
                {bare, true},
                {deps, ?DEPS},
                {example, "rebar3 lfe repl"},
                {short_desc, Description},
                {desc, info(Description)},
                {opts, opts()}
            ])
    ),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Config) ->
    {ok, confabulate(Config)}.

-spec format_error(any()) -> iolist().
format_error({unknown_app, Unknown}) ->
    io_lib:format("Applications list for confabulate contains an unrecognizable application definition: ~p", [Unknown]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

opts() ->
    [{path, undefined, "path", string,
        "Provide the path to the file or files to convert (wildcard/globbing "
        ++ "is supported)."}].

info(Description) ->
  io_lib:format(
        "~n~s~n"
        "~n",
        [Description]).

confabulate(State) ->
    rebar_api:debug("\tProcessing LFE file(s) ...", []),
    rebar_api:debug("\t\tPlain args: ~p", [init:get_plain_arguments()]),
    %% Get files to convert
    Path = find_path_option(State),
    rebar_api:debug("\tGot path: ~p", [Path]),
    Files = filelib:wildcard(Path),
    rebar_api:debug("\tGot files: ~p", [Files]),
    %% Iterate over list of files, performing conversion
    lists:map(fun convert_file/1, Files),
    State.

find_path_option(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    rebar_api:debug("\tGot opts: ~p", [Opts]),
    debug_get_value(path, Opts, no_value,
                    "Found path from command line option.").
                    
debug_get_value(Key, List, Default, Description) ->
    case proplists:get_value(Key, List, Default) of
        Default -> Default;
        Value ->
            rebar_api:debug(Description, []),
            Value
    end.

convert_file(LfeFilename) ->
    {ok, Data} = lfe_io:parse_file(LfeFilename),
    ErlFilename = filename:rootname(LfeFilename) ++ ".erl",
    lists:map(fun(D) -> append_datum(ErlFilename, D) end, Data).

append_datum(Filename, Datum) ->
    case file:read_file_info(Filename) of
        {ok, _FileInfo} ->
            file:write_file(Filename, Datum, [append]);
        {error, Err} ->
            rebar_api:debug("Could not write to ~p: ~p", [Filename, Err])
    end.
  
