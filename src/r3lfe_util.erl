-module(r3lfe_util).

-export([
    get_all_deps_with_profiles/1,
    get_all_plugins_with_profiles/1,
    ensure_app_loaded/1
]).

%% Exported for testing
-ifdef(TEST).
-export([
    get_plugin_version_from_app/2,
    get_plugin_version_from_file/2,
    find_and_read_app_file/2
]).
-endif.

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
    %% Get plugin names from config
    TopLevelPlugins = rebar_state:get(State, plugins, []),
    TopLevelProjectPlugins = rebar_state:get(State, project_plugins, []),

    %% Get profile-specific plugins
    Profiles = rebar_state:get(State, profiles, []),
    ProfilePluginConfigs = lists:flatten([
        begin
            ProfilePluginsList = proplists:get_value(plugins, ProfileConfig, []),
            ProfileProjectPluginsList = proplists:get_value(project_plugins, ProfileConfig, []),
            [{Plugin, ProfileName} || Plugin <- ProfilePluginsList ++ ProfileProjectPluginsList]
        end
        || {ProfileName, ProfileConfig} <- Profiles
    ]),

    %% Combine all plugin specs with profile info
    AllPluginSpecs = [{P, default} || P <- TopLevelPlugins ++ TopLevelProjectPlugins] ++ ProfilePluginConfigs,

    %% Extract name and version for each plugin
    lists:map(
        fun({PluginSpec, Profile}) ->
            Name = extract_name(PluginSpec),
            %% Try to get version from loaded application, then from plugins dir
            Vsn = get_plugin_version_from_app(Name, State),
            #{name => Name, version => Vsn, profile => Profile}
        end,
        AllPluginSpecs
    ).

%%====================================================================
%% Internal functions
%%====================================================================

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

%% @doc Get the version of a plugin from loaded app or from .app file in plugins dir.
-spec get_plugin_version_from_app(atom(), rebar_state:t()) -> string().
get_plugin_version_from_app(AppName, State) ->
    %% First try to get from loaded application
    case ensure_app_loaded(AppName) of
        ok ->
            case application:get_key(AppName, vsn) of
                {ok, Vsn} -> Vsn;
                undefined -> get_plugin_version_from_file(AppName, State)
            end;
        error ->
            %% Try to read from .app file in plugins directory
            get_plugin_version_from_file(AppName, State)
    end.

%% @doc Read plugin version from .app file in the plugins directory.
-spec get_plugin_version_from_file(atom(), rebar_state:t()) -> string().
get_plugin_version_from_file(AppName, State) ->
    try
        BaseDir = rebar_dir:base_dir(State),
        AppNameStr = atom_to_list(AppName),
        AppFileName = AppNameStr ++ ".app",

        %% Try multiple possible locations for the plugin's .app file
        PossiblePaths = [
            %% Current profile's plugins directory
            filename:join([rebar_dir:plugins_dir(State), AppNameStr, "ebin", AppFileName]),
            %% Default profile
            filename:join([BaseDir, "default", "plugins", AppNameStr, "ebin", AppFileName]),
            %% Test profile
            filename:join([BaseDir, "test", "plugins", AppNameStr, "ebin", AppFileName]),
            %% Dev profile
            filename:join([BaseDir, "dev", "plugins", AppNameStr, "ebin", AppFileName]),
            %% Global plugins directory
            filename:join([rebar_dir:global_cache_dir(rebar_state:opts(State)), "plugins", AppNameStr, "ebin", AppFileName])
        ],

        %% Try each path until we find the file
        find_and_read_app_file(AppName, PossiblePaths)
    catch
        _:_ ->
            "unknown"
    end.

%% @doc Try to find and read the .app file from a list of possible paths.
-spec find_and_read_app_file(atom(), [string()]) -> string().
find_and_read_app_file(_AppName, []) ->
    "unknown";
find_and_read_app_file(AppName, [Path | Rest]) ->
    case filelib:is_file(Path) of
        true ->
            case file:consult(Path) of
                {ok, [{application, AppName, AppProps}]} ->
                    proplists:get_value(vsn, AppProps, "unknown");
                _ ->
                    find_and_read_app_file(AppName, Rest)
            end;
        false ->
            find_and_read_app_file(AppName, Rest)
    end.
