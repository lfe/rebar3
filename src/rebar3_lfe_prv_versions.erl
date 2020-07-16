-module(rebar3_lfe_prv_versions).

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, versions).
-define(NAMESPACE, lfe).
-define(DEPS, [compile]).

%% =============================================================================
%% Plugin API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description = "Get various versions",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe version"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar_state:project_apps(State),
    lfe_io:format("~p~n", [versions(Apps)]),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Get comprehensive versions of the project, LFE, Erlang, and others.~n",
        [Description]).

versions(Apps) ->
    rebar_api:debug("Getting versions ...", []),
    [{apps, [app_version_data(AppInfo) || AppInfo <- Apps]},
     {languages, language_versions()},
     {tooling, rebar_versions()}].

app_version_data(AppInfo) ->
  rebar_api:debug("AppInfo: ~p", [AppInfo]),
  
  AppFile = rebar_app_info:app_file(AppInfo),
  rebar_api:debug("AppFile: ~p", [AppFile]),
  AppDetails = rebar_app_info:app_details(AppInfo),
  rebar_api:debug("AppDetails: ~p", [AppDetails]),
  AppVersOrig = rebar_app_info:original_vsn(AppInfo),
  rebar_api:debug("AppVersOrig: ~p", [AppVersOrig]),
  % AppVers = rebar_app_info:vsn(AppInfo),
  % rebar_api:debug("AppVers: ~p", [AppVers]),
  {rebar3_lfe_utils:app_name(AppInfo), AppVersOrig}.

language_versions() ->
    [{lfe, app_version(lfe)},
     {erlang, erlang:system_info(otp_release)},
     {emulator, erlang:system_info(version)},
     {driver_version, erlang:system_info(driver_version)}
    ].

rebar_versions() ->
    [{rebar, app_version(rebar)},
     {rebar3_lfe, app_version(rebar3_lfe)}
    ].

app_version(AppName) ->
  application:load(AppName),
  case application:get_key(AppName, vsn) of
    {ok, Vsn} -> Vsn;
    Default -> Default
  end.
