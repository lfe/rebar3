-module(r3lfe_config).

%% API exports
-export([
    get_lfe_opts/1,
    get_src_dirs/1,
    get_include_dirs/1,
    get_out_dir/1,
    get_first_files/1,
    is_verbose/1,
    merge_opts/2
]).

%% For testing
-export([
    normalize_src_dirs/2,
    normalize_include_dirs/2
]).

-include("r3lfe.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @doc Get LFE compiler options from rebar state/app config
-spec get_lfe_opts(rebar_app_info:t() | rebar_state:t()) -> [term()].
get_lfe_opts(AppInfo) when is_tuple(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),

    case rebar_app_info:opts(AppInfo) of
        undefined ->
            ?DEFAULT_LFE_OPTS;
        Opts ->
            %% First try to get options from the app's own rebar.config
            %% This is critical for dependencies that have their own erl_opts
            {LfeOpts, ErlOpts} = case read_app_config(AppDir) of
                {ok, AppConfig} ->
                    %% Use the app's own config if available
                    AppLfeOpts = proplists:get_value(lfe_opts, AppConfig, []),
                    AppErlOpts = proplists:get_value(erl_opts, AppConfig, []),
                    {AppLfeOpts, AppErlOpts};
                error ->
                    %% Fallback to rebar3's merged opts
                    MergedLfeOpts = rebar_opts:get(Opts, lfe_opts, []),
                    MergedErlOpts = rebar_opts:get(Opts, erl_opts, []),
                    {MergedLfeOpts, MergedErlOpts}
            end,

            merge_opts(?DEFAULT_LFE_OPTS, LfeOpts ++ ErlOpts)
    end;
get_lfe_opts(State) ->
    Opts = rebar_state:opts(State),
    LfeOpts = rebar_opts:get(Opts, lfe_opts, []),
    ErlOpts = rebar_opts:get(Opts, erl_opts, []),
    merge_opts(?DEFAULT_LFE_OPTS, LfeOpts ++ ErlOpts).

%% @doc Get source directories for an application
-spec get_src_dirs(rebar_app_info:t()) -> [file:filename()].
get_src_dirs(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    %% Get additional source directories from config
    ExtraDirs = rebar_dir:src_dirs(Opts, []),

    %% Normalize and return absolute paths
    normalize_src_dirs(AppDir, [?DEFAULT_SRC_DIR | ExtraDirs]).

%% @doc Get include directories for an application
-spec get_include_dirs(rebar_app_info:t()) -> [file:filename()].
get_include_dirs(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    %% Get additional include directories
    ExtraDirs = rebar_opts:get(Opts, lfe_include_dirs, []),

    normalize_include_dirs(AppDir, [?DEFAULT_INCLUDE_DIR | ExtraDirs]).

%% @doc Get output directory for compiled beams
-spec get_out_dir(rebar_app_info:t()) -> file:filename().
get_out_dir(AppInfo) ->
    rebar_app_info:ebin_dir(AppInfo).

%% @doc Get list of files that must be compiled first
-spec get_first_files(rebar_app_info:t()) -> [file:filename()].
get_first_files(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    RelativeFiles = rebar_opts:get(Opts, lfe_first_files, []),
    [filename:join(AppDir, File) || File <- RelativeFiles].

%% @doc Check if verbose output is enabled
-spec is_verbose(rebar_app_info:t() | rebar_state:t()) -> boolean().
is_verbose(AppInfo) when is_tuple(AppInfo) ->
    Opts = case rebar_app_info:opts(AppInfo) of
        undefined -> rebar_opts:new();
        O -> O
    end,
    lists:member(verbose, get_lfe_opts(AppInfo)) orelse
        rebar_opts:get(Opts, verbose, false);
is_verbose(State) ->
    lists:member(verbose, get_lfe_opts(State)).

%% @doc Merge compiler options, with later options taking precedence
-spec merge_opts([term()], [term()]) -> [term()].
merge_opts(Defaults, Overrides) ->
    %% Build a map of options, later entries override earlier ones
    %% Special handling for tuples (like {i, Dir}, {outdir, Dir})
    Merged = lists:foldl(
        fun(Opt, Acc) ->
            Key = option_key(Opt),
            maps:put(Key, Opt, Acc)
        end,
        maps:new(),
        Defaults ++ Overrides
    ),
    maps:values(Merged).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Read an application's rebar.config file
-spec read_app_config(file:filename()) -> {ok, [term()]} | error.
read_app_config(AppDir) ->
    ConfigFile = filename:join(AppDir, "rebar.config"),
    case filelib:is_file(ConfigFile) of
        true ->
            case file:consult(ConfigFile) of
                {ok, Config} ->
                    {ok, Config};
                {error, Reason} ->
                    ?DEBUG("Failed to read ~s: ~p", [ConfigFile, Reason]),
                    error
            end;
        false ->
            error
    end.

%% @doc Extract a key from an option for deduplication
-spec option_key(term()) -> term().
option_key({Key, _Value}) -> Key;
option_key(Atom) when is_atom(Atom) -> Atom;
option_key(Other) -> Other.

%% @doc Normalize source directories to absolute paths
-spec normalize_src_dirs(file:filename(), [file:filename()]) -> [file:filename()].
normalize_src_dirs(AppDir, Dirs) ->
    AbsDirs = [begin
        AbsPath = filename:absname(Dir, AppDir),
        case filelib:is_dir(AbsPath) of
            true -> AbsPath;
            false ->
                ?DEBUG("Source directory does not exist: ~s", [AbsPath]),
                AbsPath  % Return anyway, might be created later
        end
    end || Dir <- Dirs],

    %% Remove duplicates while preserving order
    lists:usort(AbsDirs).

%% @doc Normalize include directories to absolute paths
-spec normalize_include_dirs(file:filename(), [file:filename()]) -> [file:filename()].
normalize_include_dirs(AppDir, Dirs) ->
    AbsDirs = [begin
        AbsPath = filename:absname(Dir, AppDir),
        case filelib:is_dir(AbsPath) of
            true -> AbsPath;
            false ->
                ?DEBUG("Include directory does not exist: ~s", [AbsPath]),
                AbsPath
        end
    end || Dir <- Dirs],

    lists:usort(AbsDirs).
