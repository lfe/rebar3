-module(r3lfe_util_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test cases
-export([
    ensure_app_loaded_existing/1,
    ensure_app_loaded_nonexistent/1,
    ensure_app_loaded_already_loaded/1,
    get_all_deps_with_profiles_empty/1,
    get_all_deps_with_profiles_default/1,
    get_all_deps_with_profiles_multiple_profiles/1,
    get_all_plugins_with_profiles_empty/1,
    get_all_plugins_with_profiles_default/1,
    get_all_plugins_with_profiles_multiple_profiles/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        ensure_app_loaded_existing,
        ensure_app_loaded_nonexistent,
        ensure_app_loaded_already_loaded,
        get_all_deps_with_profiles_empty,
        get_all_deps_with_profiles_default,
        get_all_deps_with_profiles_multiple_profiles,
        get_all_plugins_with_profiles_empty,
        get_all_plugins_with_profiles_default,
        get_all_plugins_with_profiles_multiple_profiles
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%%====================================================================
%% Test ensure_app_loaded/1
%%====================================================================

ensure_app_loaded_existing(_Config) ->
    %% Test loading an existing application (stdlib should always exist)
    Result = r3lfe_util:ensure_app_loaded(stdlib),

    ?assertEqual(ok, Result),

    ok.

ensure_app_loaded_nonexistent(_Config) ->
    %% Test loading a non-existent application
    Result = r3lfe_util:ensure_app_loaded(nonexistent_app_xyz_123),

    ?assertEqual(error, Result),

    ok.

ensure_app_loaded_already_loaded(_Config) ->
    %% Test loading an already loaded application
    application:load(kernel),
    Result = r3lfe_util:ensure_app_loaded(kernel),

    ?assertEqual(ok, Result),

    ok.

%%====================================================================
%% Test get_all_deps_with_profiles/1
%%====================================================================

get_all_deps_with_profiles_empty(_Config) ->
    %% Test with empty state (no dependencies)
    State = rebar_state:new(),

    Result = r3lfe_util:get_all_deps_with_profiles(State),

    ?assertEqual([], Result),

    ok.

get_all_deps_with_profiles_default(_Config) ->
    %% Test with top-level dependencies (default profile)
    State0 = rebar_state:new(),
    Deps = [jsx, {cowboy, "2.9.0"}],
    State = rebar_state:set(State0, deps, Deps),

    Result = r3lfe_util:get_all_deps_with_profiles(State),

    %% Should have 2 dependencies
    ?assertEqual(2, length(Result)),

    %% All should be default profile
    lists:foreach(
        fun(#{profile := Profile}) ->
            ?assertEqual(default, Profile)
        end,
        Result
    ),

    %% Check that names are present
    Names = [maps:get(name, D) || D <- Result],
    ?assert(lists:member(jsx, Names)),
    ?assert(lists:member(cowboy, Names)),

    ok.

get_all_deps_with_profiles_multiple_profiles(_Config) ->
    %% Test with dependencies in multiple profiles
    State0 = rebar_state:new(),

    %% Add default deps
    DefaultDeps = [jsx],
    State1 = rebar_state:set(State0, deps, DefaultDeps),

    %% Add profile-specific deps
    Profiles = [
        {test, [{deps, [meck, proper]}]},
        {prod, [{deps, [recon]}]}
    ],
    State = rebar_state:set(State1, profiles, Profiles),

    Result = r3lfe_util:get_all_deps_with_profiles(State),

    %% Should have deps from all profiles
    ?assert(length(Result) >= 4),

    %% Check that different profiles are represented
    Profiles2 = [maps:get(profile, D) || D <- Result],
    ?assert(lists:member(default, Profiles2)),
    ?assert(lists:member(test, Profiles2)),
    ?assert(lists:member(prod, Profiles2)),

    ok.

%%====================================================================
%% Test get_all_plugins_with_profiles/1
%%====================================================================

get_all_plugins_with_profiles_empty(_Config) ->
    %% Test with empty state (no plugins)
    State = rebar_state:new(),

    Result = r3lfe_util:get_all_plugins_with_profiles(State),

    %% Should be empty or minimal
    ?assert(is_list(Result)),

    ok.

get_all_plugins_with_profiles_default(_Config) ->
    %% Test with top-level plugins (default profile)
    State0 = rebar_state:new(),
    Plugins = [rebar3_format, {rebar3_lint, "3.2.5"}],
    State = rebar_state:set(State0, plugins, Plugins),

    Result = r3lfe_util:get_all_plugins_with_profiles(State),

    %% Should have at least 2 plugins
    ?assert(length(Result) >= 2),

    %% Check that names are present
    Names = [maps:get(name, P) || P <- Result],
    ?assert(lists:member(rebar3_format, Names)),
    ?assert(lists:member(rebar3_lint, Names)),

    ok.

get_all_plugins_with_profiles_multiple_profiles(_Config) ->
    %% Test with plugins in multiple profiles
    State0 = rebar_state:new(),

    %% Add default plugins
    DefaultPlugins = [rebar3_format],
    State1 = rebar_state:set(State0, plugins, DefaultPlugins),

    %% Add profile-specific plugins
    Profiles = [
        {test, [{plugins, [rebar3_proper]}]},
        {dev, [{project_plugins, [rebar3_lint]}]}
    ],
    State = rebar_state:set(State1, profiles, Profiles),

    Result = r3lfe_util:get_all_plugins_with_profiles(State),

    %% Should have plugins from all profiles
    ?assert(length(Result) >= 3),

    %% Check that different profiles are represented
    Profiles2 = [maps:get(profile, P) || P <- Result],
    ?assert(lists:member(default, Profiles2)),

    ok.
