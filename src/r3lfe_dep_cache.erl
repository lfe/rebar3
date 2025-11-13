-module(r3lfe_dep_cache).

%% API exports
-export([
    init/0,
    get/1,
    put/2,
    invalidate/1,
    clear/0
]).

-include("r3lfe.hrl").

%% ETS table name
-define(CACHE_TABLE, r3lfe_dep_cache).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize the dependency cache
-spec init() -> ok.
init() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            ?DEBUG("Creating dependency cache table", []),
            _Tid = ets:new(?CACHE_TABLE, [
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

%% @doc Get cached dependencies for a file
%% Returns {ok, Deps, Timestamp} if cached and still valid
%% Returns error if not cached or cache is stale
-spec get(file:filename()) ->
    {ok, [file:filename()], file:date_time()} | error.
get(SourceFile) ->
    %% Check if table exists first
    case ets:info(?CACHE_TABLE) of
        undefined ->
            error;
        _ ->
            lookup_cache(SourceFile)
    end.

lookup_cache(SourceFile) ->
    case ets:lookup(?CACHE_TABLE, SourceFile) of
        [] ->
            error;
        [{SourceFile, Deps, CachedTime}] ->
            %% Check if source file has been modified since caching
            CurrentTime = filelib:last_modified(SourceFile),

            if
                CurrentTime =< CachedTime ->
                    {ok, Deps, CachedTime};
                true ->
                    %% Cache is stale
                    ?DEBUG("Cache stale for ~s", [SourceFile]),
                    error
            end
    end.

%% @doc Cache dependencies for a file
-spec put(file:filename(), [file:filename()]) -> ok.
put(SourceFile, Deps) ->
    %% Only cache if table exists
    case ets:info(?CACHE_TABLE) of
        undefined ->
            ok;
        _ ->
            Timestamp = filelib:last_modified(SourceFile),
            true = ets:insert(?CACHE_TABLE, {SourceFile, Deps, Timestamp}),
            ?DEBUG("Cached ~p dependencies for ~s", [length(Deps), SourceFile]),
            ok
    end.

%% @doc Invalidate cache entry for a specific file
-spec invalidate(file:filename()) -> ok.
invalidate(SourceFile) ->
    true = ets:delete(?CACHE_TABLE, SourceFile),
    ok.

%% @doc Clear entire cache
-spec clear() -> ok.
clear() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            ok;
        _ ->
            true = ets:delete_all_objects(?CACHE_TABLE),
            ok
    end.
