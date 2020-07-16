-module(rebar3_lfe_prv_clean_cache).

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, 'clean-cache').
-define(NAMESPACE, lfe).
-define(DEPS, [{default, clean}]).
-define(LIB, "lib").
-define(PLUGINS, "plugins").
-define(PLUGIN, "rebar3_lfe").

%% =============================================================================
%% Plugin API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description = "Remove the project's cache directories",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe clean-cache"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Opts = rebar_state:opts(State),
    Apps = rebar_state:project_apps(State),
    [clean(Opts, AppInfo) || AppInfo <- Apps],
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
        "This deletes the project's entries in the local and global plugins~n"
        "and/or lib directories. It also deletes the rebar3_lfe plugin~n"
        "caches.~n",
        [Description]).

clean(Opts, AppInfo) ->
    Dirs = cache_dirs(Opts, AppInfo),
    rebar_api:debug("~p~n", [Dirs]),
    [rebar_file_utils:rm_rf(Dir) || Dir <- Dirs].

cache_dirs(Opts, AppInfo) ->
    Global = rebar_dir:global_cache_dir(Opts),
    AppName = rebar3_lfe_utils:app_name(AppInfo),
    rebar_api:debug("Global cache dir: ~p~n", [Global]),
    [filename:join([Global, ?LIB, AppName]),
     filename:join([Global, ?PLUGINS, AppName]),
     filename:join([Global, ?LIB, ?PLUGIN]),
     filename:join([Global, ?PLUGINS, ?PLUGIN])].
