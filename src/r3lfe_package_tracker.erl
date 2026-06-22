-module(r3lfe_package_tracker).

%% API exports
-export([
    init/0,
    register_package/1,
    unregister_package/1,
    get_registered_packages/0,
    is_temp_file/1,
    cleanup_all/0
]).

-include("r3lfe.hrl").

-define(TRACKER_TABLE, r3lfe_package_tracker).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize the package tracker
-spec init() -> ok.
init() ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            ?DEBUG("Creating package tracker table", []),
            _Tid = ets:new(?TRACKER_TABLE, [
                named_table,
                public,
                set,
                {read_concurrency, true}
            ]),
            ok;
        _ ->
            %% Already initialized
            ok
    end.

%% @doc Register a package transformation
-spec register_package(#{temp_file := file:filename_all(), _ => _}) -> ok.
register_package(PackageInfo = #{temp_file := TempFile}) ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            ok;
        _ ->
            true = ets:insert(?TRACKER_TABLE, {TempFile, PackageInfo}),
            ?DEBUG("Registered package: ~s", [TempFile]),
            ok
    end.

%% @doc Unregister a package transformation
-spec unregister_package(file:filename()) -> ok.
unregister_package(TempFile) ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            ok;
        _ ->
            true = ets:delete(?TRACKER_TABLE, TempFile),
            ok
    end.

%% @doc Get all registered packages
-spec get_registered_packages() -> [map()].
get_registered_packages() ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            [];
        _ ->
            [Info || {_Key, Info} <- ets:tab2list(?TRACKER_TABLE)]
    end.

%% @doc Check if a file is a temporary package file
-spec is_temp_file(file:filename()) -> boolean().
is_temp_file(File) ->
    case ets:info(?TRACKER_TABLE) of
        undefined ->
            false;
        _ ->
            ets:member(?TRACKER_TABLE, File)
    end.

%% @doc Clean up all registered packages
-spec cleanup_all() -> ok.
cleanup_all() ->
    Packages = get_registered_packages(),
    r3lfe_package:cleanup_packages(Packages),

    case ets:info(?TRACKER_TABLE) of
        undefined ->
            ok;
        _ ->
            true = ets:delete_all_objects(?TRACKER_TABLE),
            ok
    end.
