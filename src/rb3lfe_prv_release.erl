-module(rb3lfe_prv_release).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include_lib("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, release).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Build an LFE release",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe release"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE release provider starting", []),

    %% Update app files with modules list
    update_app_files(State),

    %% Delegate to rebar3's release provider
    try
        rebar_relx:do(rlx_prv_release, "release", release, State),
        {ok, State}
    catch
        error:undef ->
            %% Try older API
            rebar_relx:do(release, State),
            {ok, State}
    end.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec update_app_files(rebar_state:t()) -> ok.
update_app_files(State) ->
    %% Get all apps
    Apps = rebar_state:project_apps(State),

    %% Update each app file
    lists:foreach(fun update_app_file/1, Apps),

    ok.

-spec update_app_file(rebar_app_info:t()) -> ok.
update_app_file(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    AppFile = rebar_app_info:app_file(AppInfo),

    %% Find all beam files
    BeamFiles = filelib:wildcard(filename:join(EbinDir, "*.beam")),

    %% Extract module names
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- BeamFiles],

    %% Read current app file
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppData}]} ->
            %% Update modules list
            AppData1 = lists:keystore(modules, 1, AppData, {modules, Modules}),

            %% Write updated app file
            Content = io_lib:format("~p.~n", [{application, AppName, AppData1}]),

            ok = file:write_file(AppFile, Content),

            ?DEBUG("Updated ~s with ~p modules", [AppFile, length(Modules)]),
            ok;

        {error, Reason} ->
            ?WARN("Failed to read app file ~s: ~p", [AppFile, Reason]),
            ok
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Builds a release for the LFE project using relx.~n"
        "~n"
        "Requires relx configuration in rebar.config:~n"
        "  {relx, [~n"
        "    {release, {myapp, \"0.1.0\"}, [myapp]},~n"
        "    {dev_mode, false},~n"
        "    {include_erts, true}~n"
        "  ]}.~n",
        [Description]
    ).
