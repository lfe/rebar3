-module(rebar3_lfe_prv_clean_all).

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, 'clean-all').
-define(NAMESPACE, lfe).
-define(DEPS, [{default, clean}]).

%% =============================================================================
%% Plugin API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description = "Execute all clean commands",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe clean-all"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar3_lfe_clean:all(State),
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
        "This deletes the _build directory, all project and plugin caches,~n"
        "as well as the rebar.lock and any crashdump files.~n",
        [Description]).
