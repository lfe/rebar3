-module(rebar3_lfe_prv_compile).

-export([init/1,
         do/1,
         format_error/1]).
-export([compile/1,
         compile_app/1, compile_app/2,
         compile_dir/4,
         compile_file/3]).

%% Internal functions for dependency checking (not exported):
%% should_compile_file/3
%% check_header_dependencies_changed/3
%% get_include_paths_from_source/1
%% extract_include_paths/1
%% is_header_newer_than/3
%% resolve_include_path/2

-include("rebar3_lfe.hrl").

-define(PROVIDER, compile).
-define(NAMESPACE_PROVIDER, {?NAMESPACE, ?PROVIDER}).
-define(DEPS, [{default, lock}, {default, compile}]).

%% =============================================================================
%% Plugin API
%% =============================================================================

init(State) ->
  Description = "Compile LFE project",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe compile"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    rebar_paths:set_paths([deps, plugins], State),
    %%rebar3_lfe_utils:run_prehooks(State, ?PROVIDER),
    compile(State).

format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Public functions
%% =============================================================================

compile(State) ->
    rebar_api:debug("Compiling LFE apps ...", []),
    rebar_paths:set_paths([deps], State),
    Apps = rebar3_lfe_utils:get_apps(State),
    [compile_app(AppInfo, State) || AppInfo <- Apps],
    {ok, State}.

compile_app(AppInfo) ->
    compile_app(AppInfo, []).

compile_app(AppInfo, State) ->
    rebar_api:debug("Compiling ~ts", [rebar_app_info:name(AppInfo)]),
    %%rebar3_lfe_utils:copy_app_src(AppInfo, State),
    rebar_otp_app:compile(State, AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    OtherSrcDirs1 = rebar_dir:src_dirs(Opts),
    OtherSrcDirs = rebar3_lfe_utils:get_src_dirs(AppDir, OtherSrcDirs1),
    SourceDirs1 = rebar3_lfe_utils:get_src_dirs(AppDir, ["src"]),
    SourceDirs = SourceDirs1 ++ OtherSrcDirs,
    OutDir = rebar3_lfe_utils:relative_out_dir(AppInfo),
    FirstFiles = rebar3_lfe_utils:get_first_files(Opts, AppDir),
    Config = rebar3_lfe_utils:config(OutDir, Opts),
    rebar_api:debug("OtherSrcDirs: ~p", [OtherSrcDirs]),
    rebar_api:debug("AppInfoDir: ~p", [AppDir]),
    rebar_api:debug("SourceDirs: ~p", [SourceDirs]),
    rebar_api:debug("OutDir: ~p", [OutDir]),
    rebar_api:debug("FirstFiles: ~p", [FirstFiles]),

    %%rebar_api:debug("AppInfo: ~p", [AppInfo]),
    %%rebar_api:debug("State: ~p", [State]),
    rebar_api:debug("Config: ~p", [dict:fetch(lfe_opts, Config)]),

    rebar3_lfe_package:generate_sources(SourceDirs),
    [compile_dir(Config, FirstFiles, Dir, OutDir) || Dir <- SourceDirs],
    rebar3_lfe_package:clean_sources(SourceDirs),
    rebar_api:debug("Finished compile.", []),
    code:add_patha(rebar3_lfe_utils:out_dir(rebar_app_info:dir(AppInfo))).

compile_dir(Config, FirstFiles, SourceDir, TargetDir) ->
    rebar3_lfe_utils:ensure_dir(TargetDir),

    %% Determine the app directory (parent of source directory)
    %% This is used to resolve include paths
    AppDir = filename:dirname(SourceDir),

    %% Create a wrapped compile function that checks dependencies
    %% before actually compiling
    CompileFun = fun(Source, Target, Cfg) ->
        case should_compile_file(Source, Target, AppDir) of
            true ->
                %% File needs compilation (source or header changed)
                compile_file(Source, Target, Cfg);
            false ->
                %% File is up to date, skip compilation
                ok
        end
    end,

    %% Let rebar_base_compiler do ALL the file discovery and management.
    %% It will find the .lfe files, respect FirstFiles ordering, and call
    %% our wrapped CompileFun for each one.
    %%
    %% CRITICAL: We are NOT scanning directories ourselves. We are merely
    %% wrapping the compile function with an additional check.
    rebar_base_compiler:run(Config, FirstFiles,
                            SourceDir, ".lfe", TargetDir, ".beam",
                            CompileFun).

compile_file(Source, Target, Config) ->
    rebar_api:debug("Compiling ~s ...",
                      [rebar3_lfe_utils:relative(Source)]),
    rebar_api:debug("Output file: ~s",[Target]),
    LfeOpts = dict:fetch(lfe_opts, Config),
    rebar_api:debug("Config: ~p", [LfeOpts]),
    CompileResults = lfe_comp:file(Source, LfeOpts),
    rebar_api:debug("Compile results: ~p", [CompileResults]),
    case CompileResults of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, [], Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Config);
        {error, [{error, Es, Ws}|_], _Es, _Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Config)
    end.

%% =============================================================================
%% Header Dependency Checking (The Non-Recursive Edition)
%% =============================================================================
%%
%% This section implements timestamp-based header dependency checking WITHOUT
%% any directory traversal whatsoever. We rely entirely on rebar_base_compiler
%% to tell us which files exist, and we merely check if those files' dependencies
%% have changed.
%%
%% Critical architectural decision: NO CODE IN THIS SECTION SCANS DIRECTORIES.
%% If you find yourself tempted to traverse a directory tree, step away from
%% the keyboard and contemplate the nature of infinite recursion until the
%% temptation passes.
%%

%% should_compile_file(Source, Target, AppDir) -> boolean().
%%  Determines whether a source file needs compilation by checking:
%%  1. Does the target (.beam file) exist at all?
%%  2. Is the source newer than the target?
%%  3. Are any header files this source includes newer than the target?
%%
%%  This function does NOT scan directories. It only checks timestamps
%%  of specific files that are explicitly mentioned.
-spec should_compile_file(file:filename(), file:filename(),
                          file:filename()) -> boolean().
should_compile_file(Source, Target, AppDir) ->
    case filelib:is_file(Target) of
        false ->
            %% No beam file exists, must compile
            rebar_api:debug("~s needs compilation: target does not exist",
                          [Source]),
            true;
        true ->
            SourceTime = filelib:last_modified(Source),
            TargetTime = filelib:last_modified(Target),

            SourceNewer = SourceTime > TargetTime,
            HeadersNewer = case SourceNewer of
                true ->
                    %% Source is already newer, no need to check headers
                    false;
                false ->
                    %% Source is not newer, check if headers are
                    check_header_dependencies_changed(Source, TargetTime, AppDir)
            end,

            Result = SourceNewer orelse HeadersNewer,

            case Result of
                true when SourceNewer ->
                    rebar_api:debug("~s needs compilation: source is newer",
                                  [Source]);
                true when HeadersNewer ->
                    rebar_api:debug("~s needs compilation: header dependency changed",
                                  [Source]);
                false ->
                    rebar_api:debug("~s is up to date", [Source])
            end,

            Result
    end.

%% check_header_dependencies_changed(Source, TargetTime, AppDir) -> boolean().
%%  Check if any header file this source depends on is newer than TargetTime.
%%  Returns true if any header is newer (needs recompile) or missing (let
%%  compilation fail with proper error).
%%
%%  NOTE: This function reads ONE file (the Source) and checks timestamps
%%  of the headers it explicitly lists. It does not scan directories.
-spec check_header_dependencies_changed(file:filename(), calendar:datetime(),
                                        file:filename()) -> boolean().
check_header_dependencies_changed(Source, TargetTime, AppDir) ->
    %% Parse the source file to find include directives
    HeaderPaths = get_include_paths_from_source(Source),

    case HeaderPaths of
        [] ->
            %% No includes found, no headers to check
            false;
        _ ->
            %% Check if any header is newer than the target
            lists:any(
                fun(HeaderPath) ->
                    is_header_newer_than(HeaderPath, TargetTime, AppDir)
                end,
                HeaderPaths
            )
    end.

%% get_include_paths_from_source(SourceFile) -> [string()].
%%  Parse an LFE source file and extract the paths from include-file and
%%  include-lib directives. Returns a list of path strings as they appear
%%  in the source.
%%
%%  This function reads ONLY the specified source file. It does not open
%%  or traverse any other files or directories.
-spec get_include_paths_from_source(file:filename()) -> [string()].
get_include_paths_from_source(SourceFile) ->
    case file:read_file(SourceFile) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            extract_include_paths(Content);
        {error, Reason} ->
            %% If we can't read the file, assume no includes
            %% (compilation will fail later with better error if file truly missing)
            rebar_api:debug("Could not read ~s for include scanning: ~p",
                          [SourceFile, Reason]),
            []
    end.

%% extract_include_paths(SourceContent) -> [string()].
%%  Use regex to extract include paths from LFE source code.
%%  Matches: (include-file "path") and (include-lib "path")
-spec extract_include_paths(string()) -> [string()].
extract_include_paths(Content) ->
    %% Pattern matches:
    %%   (include-file "some/path.lfe")
    %%   (include-lib "app/include/header.lfe")
    %% Captures only the path string inside quotes
    Pattern = "\\(include-(?:file|lib)\\s+\"([^\"]+)\"\\)",

    case re:run(Content, Pattern, [global, {capture, all_but_first, list}]) of
        {match, Matches} ->
            %% Matches is a list of lists: [["path1"], ["path2"], ...]
            Paths = [Path || [Path] <- Matches],
            %% Remove duplicates (same header included multiple times)
            lists:usort(Paths);
        nomatch ->
            []
    end.

%% is_header_newer_than(HeaderPath, Timestamp, AppDir) -> boolean().
%%  Check if the specified header file is newer than the given timestamp.
%%  Returns true if:
%%    - The header exists and is newer than Timestamp
%%    - The header doesn't exist (will force recompile to get proper error)
%%  Returns false if:
%%    - The header exists and is older than Timestamp
%%
%%  This function checks ONLY the specified header file. It does not
%%  scan directories or look for other files.
-spec is_header_newer_than(string(), calendar:datetime(),
                           file:filename()) -> boolean().
is_header_newer_than(HeaderPath, Timestamp, AppDir) ->
    %% Resolve the header path to an absolute path
    AbsPath = resolve_include_path(HeaderPath, AppDir),

    case filelib:is_file(AbsPath) of
        true ->
            HeaderTime = filelib:last_modified(AbsPath),
            IsNewer = HeaderTime > Timestamp,

            case IsNewer of
                true ->
                    rebar_api:debug("Header ~s is newer than target", [AbsPath]);
                false ->
                    ok
            end,

            IsNewer;
        false ->
            %% Header file not found - trigger recompile so the actual
            %% compilation can report the missing include error properly
            rebar_api:debug("Header ~s not found, will trigger recompile",
                          [AbsPath]),
            true
    end.

%% resolve_include_path(IncludePath, AppDir) -> file:filename().
%%  Convert an include path (as written in source) to an absolute filesystem path.
%%  Handles both include-file and include-lib style paths.
%%
%%  For include-file: tries ./include/ first, then relative to AppDir
%%  For include-lib: tries to resolve via code path (simplified for quick-fix)
%%
%%  This function checks specific file paths. It does not scan directories.
-spec resolve_include_path(string(), file:filename()) -> file:filename().
resolve_include_path(Path, AppDir) ->
    case filename:pathtype(Path) of
        absolute ->
            %% Already absolute, use as-is
            Path;

        relative ->
            %% Could be include-file style: "header.lfe" or "subdir/header.lfe"
            %% Try standard include/ directory first
            IncludeDir = filename:join(AppDir, "include"),
            BaseName = filename:basename(Path),

            %% Check: include/basename
            StandardPath = filename:join(IncludeDir, BaseName),
            case filelib:is_file(StandardPath) of
                true ->
                    StandardPath;
                false ->
                    %% Check: include/path (preserving subdirs)
                    FullIncludePath = filename:join(IncludeDir, Path),
                    case filelib:is_file(FullIncludePath) of
                        true ->
                            FullIncludePath;
                        false ->
                            %% Try relative to AppDir
                            AppRelPath = filename:join(AppDir, Path),
                            case filelib:is_file(AppRelPath) of
                                true ->
                                    AppRelPath;
                                false ->
                                    %% Return path as-is, will fail existence check
                                    Path
                            end
                    end
            end;

        volumerelative ->
            %% Windows volume-relative path, treat as absolute
            Path
    end.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "No additional configuration options are required to compile~n"
        "LFE (*.lfe) files. The rebar 'erl_opts' setting is reused by~n"
        "LFE. For more information, see the rebar documentation for~n"
        "'erl_opts'.~n",
        [Description]).
