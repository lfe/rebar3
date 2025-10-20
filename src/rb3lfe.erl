-module(rb3lfe).

%% Plugin API
-export([init/1]).

-include_lib("rebar3_lfe/include/rb3lfe.hrl").

%%====================================================================
%% Plugin API
%%====================================================================

%% @doc Initialize the rebar3 plugin
%% This is called by rebar3 when the plugin is loaded
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ?DEBUG("Initializing rb3lfe plugin...", []),

    %% Initialize dependency cache
    ok = rb3lfe_dep_cache:init(),

    %% Register our compiler module with rebar3
    %% This integrates us into rebar3's compilation pipeline
    State1 = rebar_state:append_compilers(State, [rb3lfe_compiler_mod]),

    ?DEBUG("Registered rb3lfe_compiler_mod with rebar3", []),

    %% Future phases will register providers here
    {ok, State1}.
