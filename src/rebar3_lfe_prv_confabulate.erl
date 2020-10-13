-module(rebar3_lfe_prv_confabulate).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_lfe.hrl").

-define(PROVIDER, confabulate).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

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

do(Config) ->
    {ok, confabulate(Config)}.

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

append_datum(Filename, RawDatum) ->
    {Datum, _Line} = RawDatum,
    rebar_api:debug("\tGot datum: ~p", [Datum]),
    DatumBytes = list_to_binary(io_lib:format("~p.~n", [Datum])),
    rebar_api:debug("\tGot datum bytes: ~p", [DatumBytes]),
    case file:read_file_info(Filename) of
        {ok, _FileInfo} ->
            rebar_api:debug("Appending data to file ...", []),
            file:write_file(Filename, DatumBytes, [append]);
        {error, enoent} ->
            rebar_api:debug("File doesn't exist; creating it and adding data ...", []),
            case file:write_file(Filename, DatumBytes) of
                ok -> ok;
                {error, Err} -> rebar_api:debug("Could not write to ~p: ~p",
                    [Filename, Err])
            end;
        {error, Err} ->
            rebar_api:debug("Could not write to ~p: ~p", [Filename, Err])
    end.

delete_file(Filename) ->
     case file:read_file_info(Filename) of
         {ok, _FileInfo} ->
             rebar_api:debug("Deleting file ~p ...", [Filename]),
             file:delete(Filename);
         _ -> ok
     end.

convert_file(LfeFilename) ->
    {ok, Data} = lfe_io:parse_file(LfeFilename),
    rebar_api:debug("Parsed LFE data: ~p", [Data]),
    ErlFilename = filename:rootname(LfeFilename) ++ ".erl",
    delete_file(ErlFilename),
    rebar_api:debug("Writing parsed data from ~p to ~p ...", [LfeFilename,
        ErlFilename]),
    lists:map(fun(D) -> append_datum(ErlFilename, D) end, Data).
