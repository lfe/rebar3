-module(rebar3_lfe_prv_escriptize).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_lfe.hrl").

-define(PROVIDER, escriptize).
%% Re-examine the DEPS definition once the following ticket is addressed:
%% * https://github.com/lfe-rebar3/rebar3_lfe/issues/21
-define(DEPS, [compile, {default, escriptize}]).

%% =============================================================================
%% Plugin API
%% =============================================================================

init(State) ->
  Description = "Escriptize an LFE escript project",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe escriptize"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    %% We can re-enable this once the following bug is fixed:
    %% * https://github.com/lfe-rebar3/rebar3_lfe/issues/21
    %% rebar_prv_escriptize:do(State).
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Generate an LFE escript executable containing the project's and its~n"
        "dependencies' BEAM files. This command differs from the vanilla rebar3~n"
        "version in that it first executes an LFE compile operation upon the~n"
        "project code.~n",
        [Description]).
