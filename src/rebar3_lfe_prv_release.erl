-module(rebar3_lfe_prv_release).

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, release).
-define(NAMESPACE, lfe).
-define(DEPS, [{?NAMESPACE, compile}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description = "Build a release for the LFE project",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 release"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  update_all_app_files(State),
  %% Generate release
  rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State).

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
        "Prepares an LFE release-based project for an actual release and~n"
        "then generates all the files necessary to run that release,~n"
        "e.g., inproduction environments.~n",
        [Description]).

-spec update_all_app_files(rebar_state:t()) -> ok.
update_all_app_files(State) ->
  %% Update .app file for all apps
  DepsPaths   = rebar_state:code_paths(State, all_deps),
  ProjectApps = rebar_state:project_apps(State),
  AppsPaths   = [rebar_app_info:ebin_dir(AppInfo) || AppInfo <- ProjectApps],
  AllPaths    = DepsPaths ++ AppsPaths,
  [rebar3_lfe_utils:update_app_file(Dir)|| Dir <- AllPaths],
  ok.
