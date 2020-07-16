-module(rebar3_lfe_prv_clean_build).

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, 'clean-build').
-define(NAMESPACE, lfe).
-define(DEPS, [{default, clean}]).

%% =============================================================================
%% Plugin API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description = "Remove the rebar _build directory",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe clean-build"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar_state:project_apps(State),
    [clean(AppInfo) || AppInfo <- Apps],
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
        "This entirely deletes the ./_build directory created by rebar3.~n",
        [Description]).

clean(AppInfo) ->
    rebar_api:debug("AppInfo: ~p", [AppInfo]),
    BuildDir = rebar_app_info:get(AppInfo, base_dir, "_build"),
    rebar_api:debug("BuildDir: ~p~n", [BuildDir]),
    rebar_file_utils:rm_rf(BuildDir).
