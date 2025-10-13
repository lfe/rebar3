-module(rebar3_lfe_prv_compile).

-export([init/1,
         do/1,
         format_error/1]).
-export([compile/1,
         compile_app/1, compile_app/2,
         compile_dir/4,
         compile_file/3,
         compile_each/3,
         needs_compile/3,
         check_header_dependencies/3,
         get_header_dependencies/1,
         extract_includes/1,
         resolve_header_path/2]).

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

    % Get app directory for header resolution
    AppDir = filename:dirname(SourceDir),

    % Find all LFE source files
    AllFiles = rebar_utils:find_files(SourceDir, ".*\\.lfe$"),

    % Filter to files that need compilation
    FilesToCompile = lists:filter(
        fun(Source) -> needs_compile(Source, TargetDir, AppDir) end,
        AllFiles
    ),

    case FilesToCompile of
        [] ->
            rebar_api:debug("All LFE files in ~s are up to date", [SourceDir]),
            ok;
        _ ->
            rebar_api:debug("Compiling ~p LFE file(s) in ~s",
                          [length(FilesToCompile), SourceDir]),

            % Separate first files that need compilation
            FilteredFirstFiles = [F || F <- FirstFiles, lists:member(F, FilesToCompile)],
            OtherFiles = FilesToCompile -- FilteredFirstFiles,

            % Compile first files in order, then the rest
            compile_each(FilteredFirstFiles ++ OtherFiles, TargetDir, Config)
    end.

%% compile_each(Files, TargetDir, Config) -> ok | {error, Reason}.
%%  Compile each file in order, stopping on first error.
compile_each([], _TargetDir, _Config) ->
    ok;
compile_each([Source | Rest], TargetDir, Config) ->
    Module = filename:basename(Source, ".lfe"),
    Target = filename:join(TargetDir, Module ++ ".beam"),
    case compile_file(Source, Target, Config) of
        ok ->
            compile_each(Rest, TargetDir, Config);
        {ok, _Warnings} ->
            compile_each(Rest, TargetDir, Config);
        Error ->
            Error
    end.

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
%% Header Dependency Tracking (Quick-Fix for 0.4.x)
%% =============================================================================
%%
%% This is a timestamp-based approach to header dependency tracking added in
%% version 0.4.x as a quick-fix for the critical bug where include file changes
%% don't trigger recompilation.
%%
%% Limitations of this approach:
%% - Does not track transitive dependencies (A includes B, B includes C)
%% - No cross-application header tracking
%% - No integration with rebar3's DAG system
%% - include-lib resolution is simplified
%%
%% For a comprehensive solution, see the 1.0 rewrite which will adopt rebar3's
%% Custom Compiler Modules interface with full DAG integration.
%%

%% needs_compile(Source, OutDir, AppDir) -> boolean().
%%  Determine if a source file needs recompilation by checking:
%%  1. If the .beam file exists
%%  2. If the source is newer than the .beam
%%  3. If any included headers are newer than the .beam
-spec needs_compile(file:filename(), file:filename(), file:filename()) -> boolean().
needs_compile(Source, OutDir, AppDir) ->
    Module = filename:basename(Source, ".lfe"),
    BeamFile = filename:join(OutDir, Module ++ ".beam"),

    case filelib:is_file(BeamFile) of
        false ->
            rebar_api:debug("~s needs compile: beam file missing", [Source]),
            true;
        true ->
            SourceTime = filelib:last_modified(Source),
            BeamTime = filelib:last_modified(BeamFile),

            case SourceTime > BeamTime of
                true ->
                    rebar_api:debug("~s needs compile: source newer than beam", [Source]),
                    true;
                false ->
                    case check_header_dependencies(Source, BeamTime, AppDir) of
                        true ->
                            rebar_api:debug("~s needs compile: header dependency changed", [Source]),
                            true;
                        false ->
                            rebar_api:debug("~s up to date", [Source]),
                            false
                    end
            end
    end.

%% check_header_dependencies(Source, BeamTime, AppDir) -> boolean().
%%  Check if any header files included by Source are newer than BeamTime.
%%  Returns true if any header is newer (needs recompile) or missing (to report error).
-spec check_header_dependencies(file:filename(), calendar:datetime(),
                                 file:filename()) -> boolean().
check_header_dependencies(Source, BeamTime, AppDir) ->
    Headers = get_header_dependencies(Source),
    lists:any(
        fun(Header) ->
            FullPath = resolve_header_path(Header, AppDir),
            case filelib:is_file(FullPath) of
                true ->
                    HeaderTime = filelib:last_modified(FullPath),
                    IsNewer = HeaderTime > BeamTime,
                    case IsNewer of
                        true ->
                            rebar_api:debug("Header ~s is newer than beam", [FullPath]);
                        false ->
                            ok
                    end,
                    IsNewer;
                false ->
                    rebar_api:debug("Header ~s not found, forcing recompile", [FullPath]),
                    true  % Missing header will trigger recompile and report error
            end
        end,
        Headers
    ).

%% get_header_dependencies(SourceFile) -> [string()].
%%  Parse an LFE source file and extract all included header file paths.
%%  Matches both (include-file "path") and (include-lib "app/path") forms.
-spec get_header_dependencies(file:filename()) -> [string()].
get_header_dependencies(SourceFile) ->
    case file:read_file(SourceFile) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            extract_includes(Content);
        {error, Reason} ->
            rebar_api:debug("Could not read ~s for dependency scanning: ~p",
                          [SourceFile, Reason]),
            []
    end.

%% extract_includes(Content) -> [string()].
%%  Extract include file paths from LFE source code using regex.
%%  Matches: (include-file "path/file.lfe") and (include-lib "app/include/file.lfe")
-spec extract_includes(string()) -> [string()].
extract_includes(Content) ->
    % Match include-file and include-lib forms
    % Pattern: (include-file "...") or (include-lib "...")
    RE = "\\(include-(?:file|lib)\\s+\"([^\"]+)\"\\)",
    case re:run(Content, RE, [global, {capture, all_but_first, list}]) of
        {match, Matches} ->
            Paths = [Path || [Path] <- Matches],
            UniquePaths = lists:usort(Paths),
            case UniquePaths of
                [] -> ok;
                _ -> rebar_api:debug("Found includes: ~p", [UniquePaths])
            end,
            UniquePaths;
        nomatch ->
            []
    end.

%% resolve_header_path(Header, AppDir) -> file:filename().
%%  Resolve a header file path to an absolute path.
%%  For include-file: checks ./include/ directory, then treats as relative to AppDir
%%  For include-lib: attempts to resolve via code path (simplified for quick-fix)
-spec resolve_header_path(string(), file:filename()) -> file:filename().
resolve_header_path(Header, AppDir) ->
    case filename:pathtype(Header) of
        relative ->
            % First try the standard include/ directory
            IncludePath = filename:join([AppDir, "include", filename:basename(Header)]),
            case filelib:is_file(IncludePath) of
                true ->
                    IncludePath;
                false ->
                    % Try as relative to app directory
                    AppRelPath = filename:join(AppDir, Header),
                    case filelib:is_file(AppRelPath) of
                        true -> AppRelPath;
                        false -> Header  % Return as-is, will fail later
                    end
            end;
        absolute ->
            Header;
        volumerelative ->
            Header
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
