-module(rebar3_lfe_prv_run_escript).

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, 'run-escript').
-define(NAMESPACE, lfe).
%% Re-examine the DEPS definition once the following ticket is addressed:
%% * https://github.com/lfe-rebar3/rebar3_lfe/issues/21
-define(DEPS, [{default, escriptize}]).

%% =============================================================================
%% Plugin API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description = "Run an LFE escript",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe run-escript"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    run(State),
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
        "This runs an escriptized LFE escript project for the given~n"
        "rebar3 profile.~n",
        [Description]).

run(State) ->
    Escript = rebar_state:escript_path(State),
    Args = rebar_state:command_args(State),
    Cmd = string:join([Escript | Args], " "),
    Result = os:cmd(Cmd),
    io:format("~s~n", [Result]).
