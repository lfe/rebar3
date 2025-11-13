-module(r3lfe_prv_clean).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

%% Exported for testing
-ifdef(TEST).
-export([
    clean_app/1,
    info/1
]).
-endif.

-include("r3lfe.hrl").

-define(PROVIDER, clean).
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
