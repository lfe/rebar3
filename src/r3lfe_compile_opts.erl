-module(r3lfe_compile_opts).

%% API exports
-export([
    init/0,
    get_opts_hash/1,
    opts_changed/2,
    save_opts_hash/2,
    clear_opts_cache/0
]).

-include("r3lfe.hrl").

%% ETS table for storing compiler option hashes
-define(OPTS_CACHE, r3lfe_opts_cache).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize options cache
-spec init() -> ok.
init() ->
    case ets:info(?OPTS_CACHE) of
        undefined ->
            ?DEBUG("Creating compiler options cache table", []),
            _Tid = ets:new(?OPTS_CACHE, [
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

%% @doc Get hash of compiler options
-spec get_opts_hash([term()]) -> binary().
get_opts_hash(Opts) ->
    %% Sort options for consistent hashing
    SortedOpts = lists:sort(Opts),

    %% Create hash
    crypto:hash(md5, term_to_binary(SortedOpts)).

%% @doc Check if compiler options have changed for a file
-spec opts_changed(file:filename(), [term()]) -> boolean().
opts_changed(File, CurrentOpts) ->
    CurrentHash = get_opts_hash(CurrentOpts),

    case ets:info(?OPTS_CACHE) of
        undefined ->
            %% Cache not initialized, consider changed
            true;
        _ ->
            case ets:lookup(?OPTS_CACHE, File) of
                [] ->
                    %% No cached options, consider changed
                    true;
                [{File, CachedHash}] ->
                    CurrentHash =/= CachedHash
            end
    end.

%% @doc Save current options hash for a file
-spec save_opts_hash(file:filename(), [term()]) -> ok.
save_opts_hash(File, Opts) ->
    case ets:info(?OPTS_CACHE) of
        undefined ->
            ok;
        _ ->
            Hash = get_opts_hash(Opts),
            true = ets:insert(?OPTS_CACHE, {File, Hash}),
            ?DEBUG("Cached compiler options for ~s", [File]),
            ok
    end.

%% @doc Clear all cached option hashes
-spec clear_opts_cache() -> ok.
clear_opts_cache() ->
    case ets:info(?OPTS_CACHE) of
        undefined ->
            ok;
        _ ->
            true = ets:delete_all_objects(?OPTS_CACHE),
            ok
    end.
