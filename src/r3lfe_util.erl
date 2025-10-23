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
    %% Get all dependencies from rebar3 state (these are already resolved)
    AllDeps = rebar_state:all_deps(State),

    %% Convert to our format - we'll mark them all as default since rebar3
    %% merges profiles and we can't determine which profile they came from
    lists:map(
        fun(AppInfo) ->
            Name = rebar_app_info:name(AppInfo),
            Vsn = rebar_app_info:original_vsn(AppInfo),
            AppName = case is_binary(Name) of
                true -> binary_to_atom(Name, utf8);
                false -> Name
            end,
            #{name => AppName, version => Vsn, profile => default}
        end,
        AllDeps
    ).

%% @doc Get all plugins from all profiles with their version and profile info.
%% Returns a list of maps with keys: name, version, profile.
%% Includes both 'plugins' and 'project_plugins'.
-spec get_all_plugins_with_profiles(rebar_state:t()) -> [#{name => atom(), version => string(), profile => atom()}].
get_all_plugins_with_profiles(State) ->
    %% Try to get all_plugin_deps from state (list of app_info records)
    %% This contains all loaded plugins across all profiles
    case rebar_state:get(State, all_plugin_deps, undefined) of
        undefined ->
            %% Fallback: parse config manually
            get_plugins_from_config(State);
        PluginDeps ->
            %% Convert plugin app_info records to our format
            lists:map(
                fun(AppInfo) ->
                    Name = rebar_app_info:name(AppInfo),
                    Vsn = rebar_app_info:original_vsn(AppInfo),
                    AppName = case is_binary(Name) of
                        true -> binary_to_atom(Name, utf8);
                        false -> Name
                    end,
                    #{name => AppName, version => Vsn, profile => default}
                end,
                PluginDeps
            )
    end.

%% @doc Fallback method to get plugins from config when all_plugin_deps is not available.
-spec get_plugins_from_config(rebar_state:t()) -> [#{name => atom(), version => string(), profile => atom()}].
get_plugins_from_config(State) ->
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

%% @doc Extract the name from a plugin specification.
%% Handles formats: atom, {atom, _}, etc.
-spec extract_name(term()) -> atom().
extract_name(Name) when is_atom(Name) ->
    Name;
extract_name({Name, _}) when is_atom(Name) ->
    Name;
extract_name(Other) ->
    ?DEBUG("Unexpected plugin format: ~p", [Other]),
    unknown.

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
