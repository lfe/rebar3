-module(rebar3_lfe).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, State}.
