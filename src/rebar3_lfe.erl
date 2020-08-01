-module(rebar3_lfe).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  rebar3_api:debug("Intializing rebar3_lfe plugin ..."),
  Commands = [ fun rebar3_lfe_prv_clean:init/1,
               fun rebar3_lfe_prv_clean_all:init/1,
               fun rebar3_lfe_prv_clean_build:init/1,
               fun rebar3_lfe_prv_clean_cache:init/1,
               fun rebar3_lfe_prv_compile:init/1,
               fun rebar3_lfe_prv_confabulate:init/1,
               fun rebar3_lfe_prv_escriptize:init/1,
               fun rebar3_lfe_prv_ltest:init/1,
               fun rebar3_lfe_prv_release:init/1,
               fun rebar3_lfe_prv_repl:init/1,
               fun rebar3_lfe_prv_run:init/1,
               fun rebar3_lfe_prv_run_escript:init/1,
               fun rebar3_lfe_prv_run_release:init/1,
               fun rebar3_lfe_prv_versions:init/1
             ],
  FoldFun  = fun(F, {ok, StateAcc}) -> F(StateAcc) end,
  lists:foldl(FoldFun, {ok, State}, Commands).

%% Useful for debugging new plugins:
%%
%% {ok, State} = rebar3_lfe:init(rebar_state:new()).
%% rebar3_lfe_prv_ltest:do(State).
%% rebar_state:command_parsed_args(rebar_state:new()).
%% rebar_prv_eunit:eunit_opts(rebar_state:new()).
