-module(rebar3_lfe).

%% Plugin API
-export([init/1]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

%%====================================================================
%% Plugin API
%%====================================================================

%% @doc Initialize the rebar3 plugin
%% This is called by rebar3 when the plugin is loaded
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ?DEBUG("Initializing r3lfe plugin...", []),

    %% Initialize all caches and trackers
    ok = r3lfe_dep_cache:init(),
    ok = r3lfe_compile_opts:init(),
    ok = r3lfe_package_tracker:init(),

    %% Register our compiler module with rebar3
    %% This integrates us into rebar3's compilation pipeline
    State1 = rebar_state:append_compilers(State, [r3lfe_compiler_mod]),

    ?DEBUG("Registered r3lfe_compiler_mod with rebar3", []),

    %% Register all providers
    Providers = [
        r3lfe_prv_compile,
        r3lfe_prv_clean,
        r3lfe_prv_repl,
        r3lfe_prv_ltest,
        r3lfe_prv_release,
        r3lfe_prv_versions,
        r3lfe_prv_run,
        r3lfe_prv_escriptize,
        r3lfe_prv_run_escript,
        r3lfe_prv_run_release,
        r3lfe_prv_confabulate
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
