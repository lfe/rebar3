-module(r3lfe_package).

%% API exports
-export([
    discover_files/1,
    prepare_packages/1,
    cleanup_packages/1,
    is_package_file/1,
    package_to_module_name/2
]).

%% For testing
-export([
    calculate_module_name/2,
    is_nested_file/2,
    validate_module_name/1
]).

-include("r3lfe.hrl").

-type package_info() :: #{
    source_file := file:filename(),      % Original nested file
    temp_file := file:filename(),        % Temporary flattened file
    module_name := string(),             % Calculated module name
    source_dir := file:filename()        % Base source directory
}.

-export_type([package_info/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Discover all LFE files including those in subdirectories
-spec discover_files(file:filename()) -> [file:filename()].
discover_files(SourceDir) ->
    case filelib:is_dir(SourceDir) of
        true ->
            discover_files_recursive(SourceDir, SourceDir);
        false ->
            ?WARN("Source directory does not exist: ~s", [SourceDir]),
            []
    end.

%% @doc Prepare package files for compilation
%% Accepts list of {File, SourceDir} tuples
%% Creates temporary flattened copies of nested files
%% Returns list of package_info() records for cleanup
-spec prepare_packages([{file:filename(), file:filename()}]) -> 
    {ok, [package_info()]} | {error, term()}.
prepare_packages(FilesWithSrcDirs) ->
    ?DEBUG("Starting package preparation for ~p files", [length(FilesWithSrcDirs)]),
    
    %% Separate nested files from already-flat files
    {NestedFiles, _FlatFiles} = lists:partition(
        fun({File, SourceDir}) ->
            is_nested_file(File, SourceDir)
        end,
        FilesWithSrcDirs
    ),

    ?DEBUG("Found ~p nested files, ~p flat files", 
           [length(NestedFiles), length(_FlatFiles)]),
    
    case NestedFiles of
        [] ->
            %% No packages to prepare
            ?DEBUG("No nested files to prepare", []),
            {ok, []};
        _ ->
            %% Create transformations
            ?DEBUG("Preparing ~p package files", [length(NestedFiles)]),
            prepare_package_files(NestedFiles)
    end.

%% @doc Clean up temporary package files
-spec cleanup_packages([package_info()]) -> ok.
cleanup_packages(PackageInfos) ->
    lists:foreach(
        fun(#{temp_file := TempFile}) ->
            case file:delete(TempFile) of
                ok ->
                    ?DEBUG("Cleaned up temporary file: ~s", [TempFile]),
                    ok;
                {error, enoent} ->
                    %% Already deleted, ignore
                    ok;
                {error, Reason} ->
                    ?WARN("Failed to delete temporary file ~s: ~p",
                          [TempFile, Reason])
            end
        end,
        PackageInfos
    ),
    ok.

%% @doc Check if a file is a package file (nested in subdirectory)
-spec is_package_file(file:filename()) -> boolean().
is_package_file(File) ->
    %% For backward compatibility - assume parent directory is source dir
    SourceDir = filename:dirname(File),
    is_nested_file(File, SourceDir).

%% @doc Convert package file path to module name
-spec package_to_module_name(file:filename_all(), file:filename_all()) -> file:filename_all().
package_to_module_name(File, SourceDir) ->
    calculate_module_name(File, SourceDir).

%%====================================================================
%% Internal functions - Discovery
%%====================================================================

%% @doc Recursively discover all .lfe files with cycle detection
-spec discover_files_recursive(file:filename(), file:filename()) ->
    [file:filename()].
discover_files_recursive(CurrentDir, BaseDir) ->
    discover_files_recursive(CurrentDir, BaseDir, sets:new([{version, 2}])).

%% @doc Recursively discover files with visited set tracking to prevent infinite loops
-spec discover_files_recursive(file:filename(), file:filename(), sets:set()) ->
    [file:filename()].
discover_files_recursive(CurrentDir, _BaseDir, Visited) ->
    %% Get canonical path to detect cycles (resolve symlinks)
    CanonicalDir = case file:read_link_all(CurrentDir) of
        {ok, Target} ->
            %% Symlink - resolve to absolute path
            case filename:pathtype(Target) of
                absolute -> Target;
                relative -> filename:absname(Target, filename:dirname(CurrentDir))
            end;
        {error, _} ->
            %% Not a symlink, use absolute path
            filename:absname(CurrentDir)
    end,

    %% Check if we've already visited this directory
    case sets:is_element(CanonicalDir, Visited) of
        true ->
            ?DEBUG("Skipping already-visited directory: ~s", [CanonicalDir]),
            [];
        false ->
            %% Mark as visited
            NewVisited = sets:add_element(CanonicalDir, Visited),

            %% Process directory
            case file:list_dir(CurrentDir) of
                {ok, Entries} ->
                    lists:flatmap(
                        fun(Entry) ->
                            Path = filename:join(CurrentDir, Entry),
                            case filelib:is_dir(Path) of
                                true ->
                                    %% Recurse with visited set
                                    discover_files_recursive(Path, CurrentDir, NewVisited);
                                false ->
                                    case filename:extension(Path) of
                                        ?LFE_SRC_EXTENSION ->
                                            [Path];
                                        _ ->
                                            []
                                    end
                            end
                        end,
                        Entries
                    );
                {error, Reason} ->
                    ?WARN("Could not list directory ~s: ~p", [CurrentDir, Reason]),
                    []
            end
    end.

%%====================================================================
%% Internal functions - Package Preparation
%%====================================================================

%% @doc Prepare nested package files
-spec prepare_package_files([{file:filename(), file:filename()}]) ->
    {ok, [package_info()]} | {error, term()}.
prepare_package_files(NestedFiles) ->
    Results = lists:map(
        fun({SourceFile, SourceDir}) ->
            prepare_single_package(SourceFile, SourceDir)
        end,
        NestedFiles
    ),

    %% Check if any failed
    case lists:filter(fun(R) -> element(1, R) =:= error end, Results) of
        [] ->
            %% All succeeded
            PackageInfos = [Info || {ok, Info} <- Results],
            {ok, PackageInfos};
        Errors ->
            %% Some failed, clean up any successful ones
            SuccessfulInfos = [Info || {ok, Info} <- Results],
            cleanup_packages(SuccessfulInfos),
            {error, {package_preparation_failed, Errors}}
    end.

%% @doc Prepare a single package file
-spec prepare_single_package(file:filename(), file:filename()) ->
    {ok, package_info()} | {error, term()}.
prepare_single_package(SourceFile, SourceDir) ->
    ModuleName = calculate_module_name(SourceFile, SourceDir),

    %% Validate module name
    case validate_module_name(ModuleName) of
        ok ->
            %% Create temporary file in source directory
            TempFile = filename:join(SourceDir, ModuleName ++ ?LFE_SRC_EXTENSION),

            case copy_file_safe(SourceFile, TempFile) of
                ok ->
                    Info = #{
                        source_file => SourceFile,
                        temp_file => TempFile,
                        module_name => ModuleName,
                        source_dir => SourceDir
                    },

                    ?DEBUG("Prepared package: ~s -> ~s", [SourceFile, TempFile]),
                    {ok, Info};

                {error, Reason} ->
                    ?ERROR("Failed to copy ~s to ~s: ~p",
                           [SourceFile, TempFile, Reason]),
                    {error, {copy_failed, SourceFile, Reason}}
            end;

        {error, Reason} ->
            ?ERROR("Invalid module name ~s for ~s: ~p",
                   [ModuleName, SourceFile, Reason]),
            {error, {invalid_module_name, ModuleName, Reason}}
    end.

%% @doc Copy a file safely with error handling
-spec copy_file_safe(file:filename(), file:filename()) -> ok | {error, term()}.
copy_file_safe(Source, Dest) ->
    %% Check if destination already exists (shouldn't happen, but be safe)
    _ = case filelib:is_file(Dest) of
        true ->
            ?WARN("Temporary file already exists: ~s", [Dest]),
            %% Delete it and try again
            file:delete(Dest);
        false ->
            ok
    end,

    %% Copy the file
    case file:copy(Source, Dest) of
        {ok, _BytesCopied} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions - Naming
%%====================================================================

%% @doc Calculate module name from file path and source directory
%% Examples:
%%   src/my/package.lfe -> my.package
%%   src/my/other/deep.lfe -> my.other.deep
%%   test/my-test.lfe -> my-test (flat)
-spec calculate_module_name(file:filename_all(), file:filename_all()) -> file:filename_all().
calculate_module_name(File, SourceDir) ->
    %% Get relative path from source directory
    AbsFile = filename:absname(File),
    AbsSourceDir = filename:absname(SourceDir),
    
    RelPath = case string:prefix(AbsFile, AbsSourceDir) of
        nomatch ->
            %% File not under source directory (shouldn't happen)
            ?WARN("File ~s not under source dir ~s", [File, SourceDir]),
            filename:basename(File, ?LFE_SRC_EXTENSION);
        "/" ++ Rest ->
            Rest;
        Rest ->
            Rest
    end,

    %% Remove .lfe extension
    WithoutExt = filename:rootname(RelPath, ?LFE_SRC_EXTENSION),

    %% Replace path separators with dots
    %% and normalize separators for cross-platform compatibility
    Normalized = re:replace(WithoutExt, "[/\\\\]+", ".",
                           [global, {return, list}]),

    Normalized.

%% @doc Check if a file is nested (not directly in source directory)
-spec is_nested_file(file:filename(), file:filename()) -> boolean().
is_nested_file(File, SourceDir) ->
    FileDir = filename:dirname(File),

    %% Compare normalized paths
    NormFileDir = filename:absname(FileDir),
    NormSourceDir = filename:absname(SourceDir),

    NormFileDir =/= NormSourceDir.

%% @doc Validate a module name
%% Module names must:
%% - Not be empty
%% - Not start with a dot
%% - Not end with a dot
%% - Not contain consecutive dots
%% - Only contain valid characters (alphanumeric, underscore, dot, hyphen)
-spec validate_module_name(string()) -> ok | {error, term()}.
validate_module_name("") ->
    {error, empty_name};
validate_module_name("." ++ _) ->
    {error, starts_with_dot};
validate_module_name(Name) ->
    case lists:last(Name) of
        $. ->
            {error, ends_with_dot};
        _ ->
            %% Check for consecutive dots
            case string:find(Name, "..") of
                nomatch ->
                    %% Check for valid characters
                    case re:run(Name, "^[a-zA-Z0-9._-]+$", [{capture, none}]) of
                        match ->
                            ok;
                        nomatch ->
                            {error, invalid_characters}
                    end;
                _ ->
                    {error, consecutive_dots}
            end
    end.
