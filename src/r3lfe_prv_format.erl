-module(r3lfe_prv_format).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("r3lfe.hrl").

-define(PROVIDER, format).
-define(DEPS, [{default, app_discovery}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Format LFE source files",

    Opts = [
        {dry_run, $n, "dry-run", boolean,
         "Do not write; print the formatted result to stdout"},
        {check, $c, "check", boolean,
         "Do not write; exit non-zero if any file is not already formatted"},
        {path, $p, "path", string,
         "Format only this file or directory, ignoring configured source dirs"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe format"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE format provider starting", []),

    {Opts, _} = rebar_state:command_parsed_args(State),

    DryRun = proplists:get_value(dry_run, Opts, false),
    Check  = proplists:get_value(check, Opts, false),
    Path   = proplists:get_value(path, Opts, undefined),

    case {DryRun, Check} of
        {true, true} ->
            {error, "--dry-run and --check are mutually exclusive"};
        _ ->
            case resolve_files(Path, State) of
                {error, _} = Err -> Err;
                {ok, Files} ->
                    if
                        DryRun -> run_dry_run(Files, State);
                        Check  -> run_check(Files, State);
                        true   -> run_inplace(Files, State)
                    end
            end
    end.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec resolve_files(string() | undefined, rebar_state:t()) ->
    {ok, [file:filename()]} | {error, string()}.
resolve_files(undefined, State) ->
    Apps = case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        App       -> [App]
    end,
    Files = lists:flatmap(
        fun(AppInfo) ->
            SrcDirs = r3lfe_config:get_src_dirs(AppInfo),
            lists:flatmap(fun r3lfe_package:discover_files/1, SrcDirs)
        end,
        Apps
    ),
    {ok, Files};
resolve_files(Path, _State) ->
    case filelib:is_regular(Path) of
        true ->
            {ok, [Path]};
        false ->
            case filelib:is_dir(Path) of
                true ->
                    {ok, r3lfe_package:discover_files(Path)};
                false ->
                    {error, lists:flatten(
                        io_lib:format("path does not exist: ~s", [Path]))}
            end
    end.

-spec run_inplace([file:filename()], rebar_state:t()) ->
    {ok, rebar_state:t()} | {error, string()}.
run_inplace(Files, State) ->
    {NFmt, NUnchanged, NFailed} = lists:foldl(
        fun(File, {Fmt, Unch, Fail}) ->
            case format_file(File) of
                formatted  -> {Fmt + 1, Unch,     Fail};
                unchanged  -> {Fmt,     Unch + 1, Fail};
                {error, _} -> {Fmt,     Unch,     Fail + 1}
            end
        end,
        {0, 0, 0},
        Files
    ),
    ?INFO("Formatted ~p file(s), ~p unchanged, ~p failed",
          [NFmt, NUnchanged, NFailed]),
    case NFailed of
        0 ->
            {ok, State};
        _ ->
            {error, lists:flatten(
                io_lib:format("~p file(s) failed to format", [NFailed]))}
    end.

-spec run_dry_run([file:filename()], rebar_state:t()) ->
    {ok, rebar_state:t()} | {error, string()}.
run_dry_run(Files, State) ->
    Single = length(Files) =:= 1,
    {NFailed, _} = lists:foldl(
        fun(File, {Fail, IsFirst}) ->
            case read_and_format(File) of
                {error, _} ->
                    {Fail + 1, false};
                {ok, _Original, Out} ->
                    case Single of
                        true ->
                            io:format("~ts", [Out]);
                        false ->
                            case IsFirst of
                                true  -> ok;
                                false -> io:format("~n")
                            end,
                            io:format(";; ==> ~s~n~ts", [File, Out])
                    end,
                    {Fail, false}
            end
        end,
        {0, true},
        Files
    ),
    case NFailed of
        0 ->
            {ok, State};
        _ ->
            {error, lists:flatten(
                io_lib:format("~p file(s) failed to format", [NFailed]))}
    end.

-spec run_check([file:filename()], rebar_state:t()) ->
    {ok, rebar_state:t()} | {error, string()}.
run_check(Files, State) ->
    NFiles = length(Files),
    {Unformatted, NFailed} = lists:foldl(
        fun(File, {Changes, Fail}) ->
            case read_and_format(File) of
                {error, _} ->
                    {Changes, Fail + 1};
                {ok, Original, Out} ->
                    case Out =:= Original of
                        true  -> {Changes, Fail};
                        false -> {[File | Changes], Fail}
                    end
            end
        end,
        {[], 0},
        Files
    ),
    NUnformatted = length(Unformatted),
    case {NUnformatted, NFailed} of
        {0, 0} ->
            ?INFO("All ~p file(s) are formatted", [NFiles]),
            {ok, State};
        _ ->
            lists:foreach(
                fun(F) -> ?ERROR("Unformatted: ~s", [F]) end,
                lists:reverse(Unformatted)
            ),
            Msg = case {NUnformatted, NFailed} of
                {N, 0} -> io_lib:format("~p file(s) need formatting", [N]);
                {0, F} -> io_lib:format("~p file(s) failed to parse", [F]);
                {N, F} ->
                    io_lib:format("~p file(s) need formatting; ~p failed to parse",
                                  [N, F])
            end,
            {error, lists:flatten(Msg)}
    end.

-spec read_and_format(file:filename()) ->
    {ok, binary(), binary()} | {error, term()}.
read_and_format(File) ->
    case file:read_file(File) of
        {error, ReadReason} ->
            ?ERROR("Failed to read ~s: ~p", [File, ReadReason]),
            {error, ReadReason};
        {ok, Original} ->
            case r3lfe_formatter:format(Original) of
                {error, FmtReason} ->
                    ?ERROR("Failed to format ~s: ~p", [File, FmtReason]),
                    {error, FmtReason};
                {ok, IO} ->
                    Out = unicode:characters_to_binary(IO),
                    {ok, Original, Out}
            end
    end.

-spec format_file(file:filename()) -> formatted | unchanged | {error, term()}.
format_file(File) ->
    case read_and_format(File) of
        {error, _} = Err ->
            Err;
        {ok, Original, Out} ->
            case Out =:= Original of
                true ->
                    unchanged;
                false ->
                    case file:write_file(File, Out) of
                        ok ->
                            ?INFO("Formatted ~s", [File]),
                            formatted;
                        {error, WReason} ->
                            ?ERROR("Failed to write ~s: ~p", [File, WReason]),
                            {error, WReason}
                    end
            end
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Formats LFE source files in-place.~n"
        "Without --path, formats all .lfe files in configured source dirs.~n",
        [Description]
    ).
