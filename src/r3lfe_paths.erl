-module(r3lfe_paths).

%% API exports
-export([
    set_paths/1,
    set_paths/2,
    unset_paths/1,
    unset_paths/2,
    with_paths/2,
    ensure_dir/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @doc Set code paths for dependencies
%% This replaces manual code:add_path* calls with proper rebar3 API
-spec set_paths(rebar_state:t()) -> ok.
set_paths(State) ->
    set_paths([deps, plugins], State).

%% @doc Set specific code paths
-spec set_paths([deps | plugins], rebar_state:t()) -> ok.
set_paths(Types, State) ->
    try
        rebar_paths:set_paths(Types, State),
        ok
    catch
        error:undef ->
            %% Fallback for older rebar3 versions
            fallback_set_paths(Types, State)
    end.

%% @doc Unset code paths
-spec unset_paths(rebar_state:t()) -> ok.
unset_paths(State) ->
    unset_paths([deps, plugins], State).

%% @doc Unset specific code paths
-spec unset_paths([deps | plugins], rebar_state:t()) -> ok.
unset_paths(Types, State) ->
    try
        rebar_paths:unset_paths(Types, State),
        ok
    catch
        error:undef ->
            %% Older rebar3 doesn't need explicit unset
            ok
    end.

%% @doc Execute a function with paths set, then unset them
-spec with_paths(fun(() -> Result), rebar_state:t()) -> Result when
    Result :: term().
with_paths(Fun, State) ->
    set_paths(State),
    try
        Fun()
    after
        unset_paths(State)
    end.

%% @doc Ensure a directory exists
%% Unlike filelib:ensure_dir/1, this ensures the directory itself exists,
%% not just its parent
-spec ensure_dir(file:filename()) -> ok | {error, term()}.
ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            case file:make_dir(Dir) of
                ok ->
                    ?DEBUG("Created directory: ~s", [Dir]),
                    ok;
                {error, eexist} ->
                    %% Race condition - directory was created between check and make_dir
                    ok;
                {error, Reason} = Error ->
                    ?ERROR("Failed to create directory ~s: ~p", [Dir, Reason]),
                    Error
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Fallback path setting for older rebar3 versions
-spec fallback_set_paths([deps | plugins], rebar_state:t()) -> ok.
fallback_set_paths(Types, State) ->
    lists:foreach(
        fun(Type) ->
            Paths = case Type of
                deps -> rebar_state:code_paths(State, all_deps);
                plugins -> rebar_state:code_paths(State, all_plugin_deps)
            end,
            code:add_pathsa(Paths)
        end,
        Types
    ),
    ok.
