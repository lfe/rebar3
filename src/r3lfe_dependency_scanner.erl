-module(r3lfe_dependency_scanner).

%% API exports
-export([
    scan_file/2,
    scan_file/3,
    scan_file/4,
    scan_content/1,
    resolve_include/2,
    resolve_include/3
]).

%% For testing
-export([
    parse_include_forms/1,
    extract_include_path/1,
    classify_include/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-type include_type() :: include_file | include_lib.
-type include_form() :: {include_type(), string()}.

-export_type([include_type/0, include_form/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Scan an LFE source file for dependencies
%% Returns list of absolute paths to header files this source depends on
-spec scan_file(file:filename(), rebar_app_info:t()) -> [file:filename()].
scan_file(SourceFile, AppInfo) ->
    scan_file(SourceFile, AppInfo, #{}).

%% @doc Scan with options
%% Options:
%%   - cache => true/false (future: cache parsed results)
%%   - include_dirs => [Dir] (additional include directories)
-spec scan_file(file:filename(), rebar_app_info:t(), map()) -> [file:filename()].
scan_file(SourceFile, AppInfo, Opts) when is_map(Opts) ->
    %% Extract AppDir and IncludeDirs from AppInfo
    AppDir = rebar_app_info:dir(AppInfo),
    IncludeDirs = maps:get(include_dirs, Opts,
                           r3lfe_config:get_include_dirs(AppInfo)),

    %% Call common helper
    do_scan_file(SourceFile, AppDir, IncludeDirs, Opts).

%% @doc Scan file with explicit app directory and include directories
%% This variant doesn't require an AppInfo record
-spec scan_file(file:filename(), file:filename(), [file:filename()], map()) ->
    [file:filename()].
scan_file(SourceFile, AppDir, IncludeDirs, Opts) ->
    %% Call common helper
    do_scan_file(SourceFile, AppDir, IncludeDirs, Opts).

%% @doc Scan content string for include forms
-spec scan_content(string()) -> [include_form()].
scan_content(Content) ->
    Forms = parse_include_forms(Content),
    lists:filtermap(
        fun(RawForm) ->
            case extract_include_path(RawForm) of
                {ok, Type, Path} -> {true, {Type, Path}};
                error -> false
            end
        end,
        Forms
    ).

%% @doc Resolve an include form to an absolute path
-spec resolve_include(include_form(), file:filename()) ->
    {ok, file:filename()} | {error, term()}.
resolve_include(Form, AppDir) ->
    IncludeDir = filename:join(AppDir, ?DEFAULT_INCLUDE_DIR),
    resolve_include(Form, AppDir, [IncludeDir]).

-spec resolve_include(include_form(), file:filename(), [file:filename()]) ->
    {ok, file:filename()} | {error, term()}.
resolve_include({include_file, Path}, AppDir, IncludeDirs) ->
    resolve_include_file(Path, AppDir, IncludeDirs);
resolve_include({include_lib, Path}, _AppDir, _IncludeDirs) ->
    resolve_include_lib(Path).

%%====================================================================
%% Internal functions - Scanning
%%====================================================================

%% @doc Core scanning implementation used by all scan_file variants
%% This function contains the actual logic for scanning files and resolving dependencies
-spec do_scan_file(file:filename(), file:filename(), [file:filename()], map()) ->
    [file:filename()].
do_scan_file(SourceFile, AppDir, IncludeDirs, Opts) ->
    UseCache = maps:get(cache, Opts, true),

    %% Check cache first
    case UseCache andalso r3lfe_dep_cache:get(SourceFile) of
        {ok, CachedDeps, _Time} ->
            ?DEBUG("Using cached dependencies for ~s", [SourceFile]),
            CachedDeps;
        _ ->
            %% Cache miss or disabled, scan the file
            case file:read_file(SourceFile) of
                {ok, Binary} ->
                    Content = binary_to_list(Binary),
                    IncludeForms = scan_content(Content),

                    %% Resolve each include form to an absolute path
                    ResolvedPaths = lists:filtermap(
                        fun(Form) ->
                            case resolve_include(Form, AppDir, IncludeDirs) of
                                {ok, Path} -> {true, Path};
                                {error, Reason} ->
                                    ?WARN("Could not resolve include in ~s: ~p",
                                          [SourceFile, Reason]),
                                    false
                            end
                        end,
                        IncludeForms
                    ),

                    Result = lists:usort(ResolvedPaths),

                    %% Cache the result
                    UseCache andalso r3lfe_dep_cache:put(SourceFile, Result),

                    Result;

                {error, Reason} ->
                    ?ERROR("Failed to read ~s: ~p", [SourceFile, Reason]),
                    []
            end
    end.

%%====================================================================
%% Internal functions - Parsing
%%====================================================================

%% @doc Parse LFE content for include forms
%% Matches: (include-file "path") and (include-lib "app/path")
-spec parse_include_forms(string()) -> [string()].
parse_include_forms(Content) ->
    %% Regular expression to match include forms
    %% Captures the entire form: (include-file "...") or (include-lib "...")
    Pattern = "\\(include-(?:file|lib)\\s+\"[^\"]+\"\\)",

    case re:run(Content, Pattern, [global, {capture, all, list}]) of
        {match, Matches} ->
            %% Matches is [[FullMatch], [FullMatch], ...]
            [Match || [Match] <- Matches];
        nomatch ->
            []
    end.

%% @doc Extract include type and path from a matched form
-spec extract_include_path(string()) -> {ok, include_type(), string()} | error.
extract_include_path(Form) ->
    %% Form looks like: (include-file "path/to/file.lfe")
    %% or: (include-lib "app/include/file.lfe")

    %% Extract the type and path
    TypePattern = "\\(include-(file|lib)\\s+\"([^\"]+)\"\\)",

    case re:run(Form, TypePattern, [{capture, all_but_first, list}]) of
        {match, ["file", Path]} ->
            {ok, include_file, Path};
        {match, ["lib", Path]} ->
            {ok, include_lib, Path};
        _ ->
            error
    end.

%% @doc Classify an include path (for diagnostics/logging)
-spec classify_include(string()) -> include_type().
classify_include(Path) ->
    case string:find(Path, "/") of
        nomatch -> include_file;
        _ ->
            %% If it looks like "app/..." it's probably include-lib
            case re:run(Path, "^[a-z_]+/", [{capture, none}]) of
                match -> include_lib;
                nomatch -> include_file
            end
    end.

%%====================================================================
%% Internal functions - Resolution
%%====================================================================

%% @doc Resolve an include-file path
%% Searches in:
%% 1. Include directories (in order)
%% 2. Relative to app directory
%% 3. As absolute path (if given)
-spec resolve_include_file(string(), file:filename(), [file:filename()]) ->
    {ok, file:filename()} | {error, not_found}.
resolve_include_file(Path, AppDir, IncludeDirs) ->
    %% Build list of candidate paths to check
    %% Note: Use lists:append, not lists:flatten, to avoid flattening strings
    IncludeCandidates = [filename:join(IncDir, Path) || IncDir <- IncludeDirs],
    AppCandidates = [filename:join(AppDir, Path)],
    AbsoluteCandidates = [Path],

    AllCandidates = IncludeCandidates ++ AppCandidates ++ AbsoluteCandidates,

    case find_existing_file(AllCandidates) of
        {ok, Found} ->
            {ok, filename:absname(Found)};
        error ->
            ?DEBUG("Could not find include-file: ~s", [Path]),
            ?DEBUG("Searched in: ~p", [AllCandidates]),
            {error, not_found}
    end.

%% @doc Resolve an include-lib path
%% Format: "app/include/file.lfe" or "app/path/to/file.lfe"
%% Resolves using code:lib_dir/1
-spec resolve_include_lib(string()) -> {ok, file:filename()} | {error, term()}.
resolve_include_lib(Path) ->
    case string:split(Path, "/", leading) of
        [AppName, RestPath] ->
            %% Convert app name string to atom
            %% Use list_to_atom (not list_to_existing_atom) because dependency
            %% applications may not be loaded yet during compilation
            AppAtom = list_to_atom(AppName),
            case code:lib_dir(AppAtom) of
                {error, bad_name} ->
                    ?DEBUG("Application not found: ~s", [AppName]),
                    {error, {app_not_found, AppName}};
                AppDir ->
                    FullPath = filename:join(AppDir, RestPath),
                    case filelib:is_file(FullPath) of
                        true ->
                            {ok, filename:absname(FullPath)};
                        false ->
                            ?DEBUG("File not found: ~s", [FullPath]),
                            {error, not_found}
                    end
            end;
        _ ->
            ?ERROR("Invalid include-lib path format: ~s", [Path]),
            {error, invalid_format}
    end.

%% @doc Find the first existing file in a list of candidates
-spec find_existing_file([file:filename()]) -> {ok, file:filename()} | error.
find_existing_file([]) ->
    error;
find_existing_file([Path | Rest]) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> find_existing_file(Rest)
    end.
