-module(rebar3_lfe).

%% Plugin API
-export([init/1]).

-include("r3lfe.hrl").

%%====================================================================
%% Plugin API
%%====================================================================

%% @doc Initialize the rebar3 plugin
%% This is called by rebar3 when the plugin is loaded
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ?DEBUG("Initializing r3lfe plugin...", []),

    %% When rebar3_lfe is loaded as a checkout dep, rebar3 does not
    %% automatically resolve the plugin's own transitive deps (lfmt).
    %% Probe for lfmt's ebin relative to our compiled beam and add it to
    %% the code path if it isn't already findable.  This is a no-op when
    %% lfmt is already on the path (hex install, fat escript, e2e merge).
    ok = ensure_lfmt_on_path(),

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
        r3lfe_prv_eval,
        r3lfe_prv_release,
        r3lfe_prv_versions,
        r3lfe_prv_run,
        r3lfe_prv_escriptize,
        r3lfe_prv_run_escript,
        r3lfe_prv_run_release,
        r3lfe_prv_confabulate,
        r3lfe_prv_format
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

%%====================================================================
%% Internal helpers
%%====================================================================

%% Ensure lfmt's ebin is on the code path.
%%
%% Rebar3 resolves a hex plugin's transitive deps automatically.  For a
%% _checkouts/ plugin it does not.  We probe a small set of canonical paths:
%%
%%   1. _build/default/{checkouts,lib}/lfmt/ebin — resolved in-project (own
%%      dev repo or project that declared lfmt as a dep/checkout)
%%   2. <project_root>/_checkouts/rebar3_lfe/_build/default/checkouts/lfmt/ebin
%%      — the rebar3_lfe source tree reached through the _checkouts/ symlink;
%%      covers "project X has _checkouts/rebar3_lfe -> rebar3_lfe src tree"
%%
%% The function is intentionally idempotent: it short-circuits as soon as
%% lfmt.beam is findable on code:get_path().
-spec ensure_lfmt_on_path() -> ok.
ensure_lfmt_on_path() ->
    case code:where_is_file("lfmt.beam") of
        non_existing ->
            add_lfmt_ebin();
        _Path ->
            ok
    end.

-spec add_lfmt_ebin() -> ok.
add_lfmt_ebin() ->
    case code:which(?MODULE) of
        Beam when is_list(Beam) ->
            ThisEbin  = filename:dirname(Beam),
            %% Both lib/rebar3_lfe/ebin and checkouts/rebar3_lfe/ebin are
            %% exactly 3 levels below _build/default/.
            BldDefault = filename:join([ThisEbin, "..", "..", ".."]),
            ProjRoot   = filename:join([BldDefault, "..", ".."]),
            Candidates = [
                filename:join([BldDefault, "checkouts", "lfmt", "ebin"]),
                filename:join([BldDefault, "lib",       "lfmt", "ebin"]),
                %% Via _checkouts/rebar3_lfe symlink → into the plugin's own
                %% build tree where lfmt is compiled as a checkout dep.
                filename:join([ProjRoot, "_checkouts", "rebar3_lfe",
                               "_build", "default", "checkouts", "lfmt", "ebin"])
            ],
            add_first_lfmt_path(Candidates);
        _ ->
            ok
    end.

-spec add_first_lfmt_path([file:filename()]) -> ok.
add_first_lfmt_path([]) -> ok;
add_first_lfmt_path([Dir | Rest]) ->
    NDir = filename:absname(Dir),
    case filelib:is_regular(filename:join(NDir, "lfmt.beam")) of
        true  -> code:add_patha(NDir), ok;
        false -> add_first_lfmt_path(Rest)
    end.
