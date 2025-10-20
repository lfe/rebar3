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

-include_lib("rebar3_lfe/include/r3lfe.hrl").

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
%% Creates temporary flattened copies of nested files
%% Returns list of package_info() records for cleanup
-spec prepare_packages([file:filename()]) -> {ok, [package_info()]} | {error, term()}.
prepare_packages(Files) ->
    %% Separate nested files from already-flat files
    {NestedFiles, _FlatFiles} = lists:partition(
        fun(File) ->
            SourceDir = find_source_dir(File),
            is_nested_file(File, SourceDir)
        end,
        Files
    ),

    case NestedFiles of
        [] ->
            %% No packages to prepare
            {ok, []};
        _ ->
            %% Create transformations
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
    SourceDir = find_source_dir(File),
    is_nested_file(File, SourceDir).

%% @doc Convert package file path to module name
-spec package_to_module_name(file:filename(), file:filename()) -> string().
package_to_module_name(File, SourceDir) ->
    calculate_module_name(File, SourceDir).

%%====================================================================
%% Internal functions - Discovery
%%====================================================================

%% @doc Recursively discover all .lfe files
-spec discover_files_recursive(file:filename(), file:filename()) ->
    [file:filename()].
discover_files_recursive(CurrentDir, _BaseDir) ->
    case file:list_dir(CurrentDir) of
        {ok, Entries} ->
            lists:flatmap(
                fun(Entry) ->
                    Path = filename:join(CurrentDir, Entry),
                    case filelib:is_dir(Path) of
                        true ->
                            %% Recurse into subdirectory
                            discover_files_recursive(Path, CurrentDir);
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
    end.

%% @doc Find the source directory for a file
%% This searches upward for a directory named "src"
-spec find_source_dir(file:filename()) -> file:filename().
find_source_dir(File) ->
    find_source_dir_upward(filename:dirname(File)).

-spec find_source_dir_upward(file:filename()) -> file:filename().
find_source_dir_upward(Dir) ->
    case filename:basename(Dir) of
        "src" ->
            Dir;
        "/" ->
            %% Reached root without finding src
            Dir;
        _ ->
            find_source_dir_upward(filename:dirname(Dir))
    end.

%%====================================================================
%% Internal functions - Package Preparation
%%====================================================================

%% @doc Prepare nested package files
-spec prepare_package_files([file:filename()]) ->
    {ok, [package_info()]} | {error, term()}.
prepare_package_files(NestedFiles) ->
    Results = lists:map(
        fun(SourceFile) ->
            prepare_single_package(SourceFile)
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
-spec prepare_single_package(file:filename()) ->
    {ok, package_info()} | {error, term()}.
prepare_single_package(SourceFile) ->
    SourceDir = find_source_dir(SourceFile),
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
    case filelib:is_file(Dest) of
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
-spec calculate_module_name(file:filename(), file:filename()) -> string().
calculate_module_name(File, SourceDir) ->
    %% Get relative path from source directory
    RelPath = case string:prefix(File, SourceDir) of
        nomatch ->
            %% File not under source directory (shouldn't happen)
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
