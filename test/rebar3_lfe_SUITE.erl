-module(rebar3_lfe_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    plugin_init_succeeds/1,
    plugin_initializes_caches/1,
    plugin_registers_compiler/1,
    plugin_registers_all_providers/1,
    plugin_with_existing_state/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        plugin_init_succeeds,
        plugin_initializes_caches,
        plugin_registers_compiler,
        plugin_registers_all_providers,
        plugin_with_existing_state
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, _Config) ->
    %% Clean up ETS tables
    try ets:delete(r3lfe_dep_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_package_tracker) catch _:_ -> ok end,
    [].

end_per_testcase(_TestCase, _Config) ->
    %% Cleanup
    try ets:delete(r3lfe_dep_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_opts_cache) catch _:_ -> ok end,
    try ets:delete(r3lfe_package_tracker) catch _:_ -> ok end,
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

plugin_init_succeeds(_Config) ->
    State = rebar_state:new(),

    Result = rebar3_lfe:init(State),

    ?assertMatch({ok, _}, Result),
    ok.

plugin_initializes_caches(_Config) ->
    State = rebar_state:new(),

    %% Caches should not exist
    ?assertEqual(undefined, ets:info(r3lfe_dep_cache)),
    ?assertEqual(undefined, ets:info(r3lfe_opts_cache)),
    ?assertEqual(undefined, ets:info(r3lfe_package_tracker)),

    %% Initialize plugin
    {ok, _State1} = rebar3_lfe:init(State),

    %% All caches should now exist
    ?assertNotEqual(undefined, ets:info(r3lfe_dep_cache)),
    ?assertNotEqual(undefined, ets:info(r3lfe_opts_cache)),
    ?assertNotEqual(undefined, ets:info(r3lfe_package_tracker)),
    ok.

plugin_registers_compiler(_Config) ->
    State = rebar_state:new(),

    %% Get initial compilers
    InitialCompilers = rebar_state:compilers(State),

    %% Initialize plugin
    {ok, State1} = rebar3_lfe:init(State),

    %% Get updated compilers
    UpdatedCompilers = rebar_state:compilers(State1),

    %% Should have more compilers
    ?assert(length(UpdatedCompilers) > length(InitialCompilers)),

    %% Should contain our compiler
    ?assert(lists:member(r3lfe_compiler_mod, UpdatedCompilers)),
    ok.

plugin_registers_all_providers(_Config) ->
    State = rebar_state:new(),

    InitialProviders = rebar_state:providers(State),

    {ok, State1} = rebar3_lfe:init(State),

    UpdatedProviders = rebar_state:providers(State1),

    %% Should have more providers
    ?assert(length(UpdatedProviders) > length(InitialProviders)),

    %% Should have at least 11 LFE providers
    LfeProviders = lists:filter(
        fun(P) ->
            providers:namespace(P) =:= lfe
        end,
        UpdatedProviders
    ),

    ?assert(length(LfeProviders) >= 11),
    ok.

plugin_with_existing_state(_Config) ->
    %% Test that plugin can initialize with state that already
    %% has providers and compilers

    State = rebar_state:new(),

    %% Add some fake state
    State1 = rebar_state:set(State, some_key, some_value),

    %% Initialize plugin
    {ok, State2} = rebar3_lfe:init(State1),

    %% Original state should be preserved
    ?assertEqual(some_value, rebar_state:get(State2, some_key, undefined)),

    %% Plugin state should be added
    ?assert(length(rebar_state:providers(State2)) > 0),
    ok.
