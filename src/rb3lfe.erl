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

    %% Initialize all caches and trackers
    ok = rb3lfe_dep_cache:init(),
    ok = rb3lfe_compile_opts:init(),
    ok = rb3lfe_package_tracker:init(),

    %% Register our compiler module with rebar3
    %% This integrates us into rebar3's compilation pipeline
    State1 = rebar_state:append_compilers(State, [rb3lfe_compiler_mod]),

    ?DEBUG("Registered rb3lfe_compiler_mod with rebar3", []),

    %% Register all providers
    Providers = [
        rb3lfe_prv_compile,
        rb3lfe_prv_clean,
        rb3lfe_prv_repl,
        rb3lfe_prv_ltest,
        rb3lfe_prv_release,
        rb3lfe_prv_versions,
        %% Phase 6.1: Escript providers
        rb3lfe_prv_run,
        rb3lfe_prv_escriptize,
        rb3lfe_prv_run_escript
    ],

    State2 = lists:foldl(
        fun(Provider, StateAcc) ->
            {ok, StateAcc1} = Provider:init(StateAcc),
            StateAcc1
        end,
        State1,
        Providers
    ),

    ?DEBUG("Registered ~p providers", [length(Providers)]),

    {ok, State2}.
