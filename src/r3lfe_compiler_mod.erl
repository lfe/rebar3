-module(r3lfe_compiler_mod).
-behaviour(rebar_compiler).

%% rebar_compiler callbacks
-export([
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2,
    format_error/1
]).

%% Exported for testing
-export([
    needs_compilation/3,
    source_to_target/2,
    check_dependencies_newer/3
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

%%====================================================================
%% rebar_compiler callbacks
%%====================================================================

%% @doc Define the compiler context
-spec context(rebar_app_info:t()) -> #{
    src_dirs => [file:filename()],
    include_dirs => [file:filename()],
    src_ext => string(),
    out_mappings => [{string(), file:filename()}],
    dependencies_opts => map()
}.
context(AppInfo) ->
    SrcDirs = r3lfe_config:get_src_dirs(AppInfo),
    IncludeDirs = r3lfe_config:get_include_dirs(AppInfo),
    OutDir = r3lfe_config:get_out_dir(AppInfo),

    ?DEBUG("Compiler context for ~s:", [rebar_app_info:name(AppInfo)]),
    ?DEBUG("  Source dirs: ~p", [SrcDirs]),
    ?DEBUG("  Include dirs: ~p", [IncludeDirs]),
    ?DEBUG("  Output dir: ~s", [OutDir]),

    #{
        src_dirs => SrcDirs,
        include_dirs => IncludeDirs,
        src_ext => ?LFE_SRC_EXTENSION,
        out_mappings => [{?BEAM_EXTENSION, OutDir}],
        dependencies_opts => #{app_info => AppInfo}
    }.

%% @doc Extract dependencies from a source file
%% This is called by rebar3's DAG builder for each source file
-spec dependencies(file:filename(), file:filename(), [file:filename()]) ->
    [file:filename()].
dependencies(Source, SourceDir, IncludeDirs) ->
    ?DEBUG("Scanning dependencies for: ~s", [Source]),
    ?DEBUG("  Source dir: ~s", [SourceDir]),
    ?DEBUG("  Include dirs: ~p", [IncludeDirs]),

    %% AppDir is the parent of the source directory
    %% SourceDir is typically AppDir/src, so we need to go up one level
    AppDir = filename:dirname(SourceDir),

    %% Use our dependency scanner with explicit include dirs
    Opts = #{include_dirs => IncludeDirs},
    Deps = r3lfe_dependency_scanner:scan_file(Source, AppDir, IncludeDirs, Opts),

    ?DEBUG("Found ~p dependencies for ~s", [length(Deps), Source]),

    Deps.

%% @doc Determine which files need compilation
%% This uses the DAG (Directed Acyclic Graph) built by rebar3 to determine
%% which files are stale based on timestamps of sources and their dependencies
-spec needed_files(
    rebar_digraph:t(),
    [file:filename()],
    [{string(), file:filename()}],
    rebar_app_info:t()
) -> {{[file:filename()], term()}, {{[file:filename()], [file:filename()]}, term()}}.
needed_files(G, FoundFiles, OutMappings, AppInfo) ->
    ?DEBUG("Checking which files need compilation...", []),

    %% Get first files that must be compiled before others
    FirstFiles = r3lfe_config:get_first_files(AppInfo),

    %% Use the DAG to determine which files need compilation
    %% A file needs compilation if:
    %% 1. Its .beam doesn't exist
    %% 2. Its source is newer than .beam
    %% 3. Any of its dependencies (headers) are newer than .beam

    NeededFiles = lists:filter(
        fun(Source) ->
            needs_compilation(G, Source, OutMappings)
        end,
        FoundFiles
    ),

    ?DEBUG("Files needing compilation: ~p", [length(NeededFiles)]),

    %% Separate first files from regular files
    {FirstNeeded, RestNeeded} = lists:partition(
        fun(File) -> lists:member(File, FirstFiles) end,
        NeededFiles
    ),

    %% Return format: {{Sequential, Opts}, {{Sequential, Parallel}, Opts}}
    %% FirstNeeded must be compiled sequentially first
    %% RestNeeded can potentially be compiled in parallel (Phase 3)
    {{FirstNeeded, []}, {{[], RestNeeded}, []}}.

%% @doc Compile a source file
%% This is called by rebar3 for each file that needs compilation
-spec compile(file:filename(), [{string(), file:filename()}],
              rebar_dict:t(), list()) ->
    ok | {ok, [string()]} | {error, [string()], [string()]}.
compile(Source, OutMappings, _Dict, Opts) ->
    %% Extract output directory from mappings
    OutDir = case OutMappings of
        [{_Ext, Dir} | _] -> Dir;
        [] -> "ebin"  % Fallback
    end,

    %% Get LFE compiler options from Opts (if available)
    LfeOpts = case lists:keyfind(lfe_opts, 1, Opts) of
        {lfe_opts, Opts1} -> Opts1;
        false -> []
    end,

    %% Get include directories from Opts and add to compiler options
    IncludeDirs = case lists:keyfind(include_dirs, 1, Opts) of
        {include_dirs, Dirs} -> Dirs;
        false -> []
    end,
    IncludeOpts = [{i, Dir} || Dir <- IncludeDirs],
    FinalOpts = LfeOpts ++ IncludeOpts,

    %% Compile the file
    case r3lfe_compile_worker:compile_file(Source, OutDir, FinalOpts) of
        ok ->
            %% Save options hash for future checks
            r3lfe_compile_opts:save_opts_hash(Source, LfeOpts),
            ok;

        {ok, Warnings} ->
            %% Format warnings as strings for rebar3
            r3lfe_compile_opts:save_opts_hash(Source, LfeOpts),
            WarningStrs = format_diagnostics(Warnings),
            {ok, WarningStrs};

        {error, Errors, Warnings} ->
            %% Format both errors and warnings
            ErrorStrs = format_diagnostics(Errors),
            WarningStrs = format_diagnostics(Warnings),
            {error, ErrorStrs, WarningStrs}
    end.

%% @doc Clean compiled files
%% NOTE: Files parameter contains SOURCE files (.lfe), but we need to delete
%% the corresponding compiled .beam files, not the sources!
-spec clean([file:filename()], rebar_app_info:t()) -> ok.
clean(Files, AppInfo) ->
    OutDir = r3lfe_config:get_out_dir(AppInfo),
    lists:foreach(
        fun(SourceFile) ->
            %% Convert source file to corresponding beam file
            BaseName = filename:basename(SourceFile, ?LFE_SRC_EXTENSION),
            BeamFile = filename:join(OutDir, BaseName ++ ?BEAM_EXTENSION),

            case file:delete(BeamFile) of
                ok ->
                    ?DEBUG("Deleted: ~s", [BeamFile]),
                    ok;
                {error, enoent} ->
                    %% Beam file doesn't exist, nothing to clean
                    ok;
                {error, Reason} ->
                    ?WARN("Failed to delete ~s: ~p", [BeamFile, Reason])
            end
        end,
        Files
    ),
    ok.

%% @doc Add format_error/1 callback for provider-level errors
-spec format_error(term()) -> iolist().
format_error(Reason) ->
    r3lfe_compile_worker:format_error(Reason).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Format error/warning tuples into human-readable strings
-spec format_diagnostics([{file:filename(), [{integer(), module(), term()}]}]) ->
    [string()].
format_diagnostics(Diagnostics) ->
    lists:flatmap(
        fun({File, Items}) ->
            [format_diagnostic_item(File, Line, Module, Desc)
             || {Line, Module, Desc} <- Items]
        end,
        Diagnostics
    ).

%% @doc Format a single diagnostic item
-spec format_diagnostic_item(file:filename(), integer(), module(), term()) ->
    string().
format_diagnostic_item(File, Line, Module, Description) ->
    %% Try to use the module's format_error if available
    Message = try
        Module:format_error(Description)
    catch
        _:_ ->
            %% Fallback to term formatting
            io_lib:format("~p", [Description])
    end,

    %% Format as: "file.lfe:123: error message"
    lists:flatten(io_lib:format("~s:~p: ~s", [File, Line, Message])).

%% @doc Check if a source file needs compilation based on DAG
-spec needs_compilation(
    rebar_digraph:t(),
    file:filename(),
    [{string(), file:filename()}]
) -> boolean().
needs_compilation(G, Source, OutMappings) ->
    %% Determine target file path
    Target = source_to_target(Source, OutMappings),

    case filelib:last_modified(Target) of
        0 ->
            %% Target doesn't exist, must compile
            ?DEBUG("~s needs compilation: target does not exist", [Source]),
            true;
        TargetTime ->
            %% Check if source is newer
            SourceTime = filelib:last_modified(Source),

            if
                SourceTime > TargetTime ->
                    ?DEBUG("~s needs compilation: source is newer", [Source]),
                    true;
                true ->
                    %% Check if any dependencies are newer
                    check_dependencies_newer(G, Source, TargetTime)
            end
    end.

%% @doc Check if any dependencies are newer than the target
-spec check_dependencies_newer(
    rebar_digraph:t(),
    file:filename(),
    file:date_time()
) -> boolean().
check_dependencies_newer(G, Source, TargetTime) ->
    %% Get all dependencies from the DAG
    %% The DAG has vertices for all files (sources and headers)
    %% and edges representing dependencies

    case digraph:vertex(G, Source) of
        false ->
            %% Source not in graph (shouldn't happen)
            ?WARN("Source ~s not found in dependency graph", [Source]),
            false;
        {Source, _Label} ->
            %% Get outgoing edges (dependencies)
            OutEdges = digraph:out_edges(G, Source),

            %% Check each dependency
            lists:any(
                fun(Edge) ->
                    {_Edge, _From, Dependency, _EdgeLabel} = digraph:edge(G, Edge),

                    case filelib:last_modified(Dependency) of
                        0 ->
                            %% Dependency doesn't exist - trigger compilation
                            %% so we get proper error message
                            ?DEBUG("~s needs compilation: dependency ~s not found",
                                   [Source, Dependency]),
                            true;
                        DepTime when DepTime > TargetTime ->
                            ?DEBUG("~s needs compilation: dependency ~s is newer",
                                   [Source, Dependency]),
                            true;
                        _ ->
                            false
                    end
                end,
                OutEdges
            )
    end.

%% @doc Convert source file path to target (beam) file path
-spec source_to_target(
    file:filename(),
    [{string(), file:filename()}]
) -> file:filename().
source_to_target(Source, OutMappings) ->
    %% OutMappings is [{Extension, OutputDir}, ...]
    %% For LFE, it's [{".beam", "path/to/ebin"}]

    case OutMappings of
        [{_Ext, OutDir} | _] ->
            BaseName = filename:basename(Source, ?LFE_SRC_EXTENSION),
            filename:join(OutDir, BaseName ++ ?BEAM_EXTENSION);
        [] ->
            %% Shouldn't happen, but have a fallback
            filename:rootname(Source) ++ ?BEAM_EXTENSION
    end.
