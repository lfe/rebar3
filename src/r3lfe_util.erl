-module(r3lfe_util).

-export([
    get_all_deps_with_profiles/1,
    get_all_plugins_with_profiles/1,
    ensure_app_loaded/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @doc Ensure an application is loaded before attempting to get its metadata.
-spec ensure_app_loaded(atom()) -> ok | error.
ensure_app_loaded(AppName) ->
    case application:load(AppName) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        _ -> error
    end.

%% @doc Get all dependencies from all profiles with their version and profile info.
%% Returns a list of maps with keys: name, version, profile.
%% LFE is included in the results (caller should filter if needed).
-spec get_all_deps_with_profiles(rebar_state:t()) -> [#{name => atom(), version => string(), profile => atom()}].
get_all_deps_with_profiles(State) ->
    %% Get top-level dependencies (default profile)
    TopLevelDeps = rebar_state:get(State, deps, []),
    DefaultDeps = extract_deps_info(TopLevelDeps, default),

    %% Get profile-specific dependencies
    Profiles = rebar_state:get(State, profiles, []),
    ProfileDeps = lists:flatten([
        extract_deps_info(proplists:get_value(deps, ProfileConfig, []), ProfileName)
        || {ProfileName, ProfileConfig} <- Profiles
    ]),

    %% Combine and return
    DefaultDeps ++ ProfileDeps.

%% @doc Get all plugins from all profiles with their version and profile info.
%% Returns a list of maps with keys: name, version, profile.
%% Includes both 'plugins' and 'project_plugins'.
-spec get_all_plugins_with_profiles(rebar_state:t()) -> [#{name => atom(), version => string(), profile => atom()}].
get_all_plugins_with_profiles(State) ->
    %% Get top-level plugins (default profile)
    TopLevelPlugins = rebar_state:get(State, plugins, []),
    TopLevelProjectPlugins = rebar_state:get(State, project_plugins, []),
    DefaultPlugins = extract_plugins_info(TopLevelPlugins ++ TopLevelProjectPlugins, default),

    %% Get profile-specific plugins
    Profiles = rebar_state:get(State, profiles, []),
    ProfilePlugins = lists:flatten([
        begin
            ProfilePluginsList = proplists:get_value(plugins, ProfileConfig, []),
            ProfileProjectPluginsList = proplists:get_value(project_plugins, ProfileConfig, []),
            extract_plugins_info(ProfilePluginsList ++ ProfileProjectPluginsList, ProfileName)
        end
        || {ProfileName, ProfileConfig} <- Profiles
    ]),

    %% Combine and return
    DefaultPlugins ++ ProfilePlugins.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Extract dependency information from a list of dependency specifications.
%% Handles various formats: atom, {atom, version}, {atom, {git, ...}}, etc.
-spec extract_deps_info([term()], atom()) -> [#{name => atom(), version => string(), profile => atom()}].
extract_deps_info(Deps, Profile) ->
    lists:map(
        fun(Dep) ->
            Name = extract_name(Dep),
            Version = get_dep_version(Name),
            #{name => Name, version => Version, profile => Profile}
        end,
        Deps
    ).

%% @doc Extract plugin information from a list of plugin specifications.
%% Handles various formats: atom, {atom, version}, {atom, {git, ...}}, etc.
-spec extract_plugins_info([term()], atom()) -> [#{name => atom(), version => string(), profile => atom()}].
extract_plugins_info(Plugins, Profile) ->
    lists:map(
        fun(Plugin) ->
            Name = extract_name(Plugin),
            Version = get_plugin_version(Name),
            #{name => Name, version => Version, profile => Profile}
        end,
        Plugins
    ).

%% @doc Extract the name from a dependency/plugin specification.
%% Handles formats: atom, {atom, _}, etc.
-spec extract_name(term()) -> atom().
extract_name(Name) when is_atom(Name) ->
    Name;
extract_name({Name, _}) when is_atom(Name) ->
    Name;
extract_name(Other) ->
    ?DEBUG("Unexpected dep/plugin format: ~p", [Other]),
    unknown.

%% @doc Get the version of a dependency by loading it and querying its application key.
-spec get_dep_version(atom()) -> string().
get_dep_version(App) ->
    case ensure_app_loaded(App) of
        ok ->
            case application:get_key(App, vsn) of
                {ok, Vsn} -> Vsn;
                undefined -> "unknown"
            end;
        error ->
            "unknown"
    end.

%% @doc Get the version of a plugin by loading it and querying its application key.
-spec get_plugin_version(atom()) -> string().
get_plugin_version(App) ->
    case ensure_app_loaded(App) of
        ok ->
            case application:get_key(App, vsn) of
                {ok, Vsn} -> Vsn;
                undefined -> "unknown"
            end;
        error ->
            "unknown"
    end.
