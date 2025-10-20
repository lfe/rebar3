-module(rb3lfe_compiler_mod).
-behaviour(rebar_compiler).

%% rebar_compiler callbacks
-export([
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2
]).

%% Exported for testing
-export([
    needs_compilation/3,
    source_to_target/2
]).

-include_lib("rebar3_lfe/include/rb3lfe.hrl").

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
    SrcDirs = rb3lfe_config:get_src_dirs(AppInfo),
    IncludeDirs = rb3lfe_config:get_include_dirs(AppInfo),
    OutDir = rb3lfe_config:get_out_dir(AppInfo),

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
-spec dependencies(file:filename(), file:filename(), rebar_app_info:t()) ->
    [file:filename()].
dependencies(Source, _SourceDir, AppInfo) ->
    ?DEBUG("Scanning dependencies for: ~s", [Source]),

    %% Use our dependency scanner
    Deps = rb3lfe_dependency_scanner:scan_file(Source, AppInfo),

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
    FirstFiles = rb3lfe_config:get_first_files(AppInfo),

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

%% @doc Compile a source file (stub for Phase 3)
-spec compile(file:filename(), [{string(), file:filename()}],
              rebar_dict:t(), list()) -> ok.
compile(Source, _OutMappings, _Dict, _Opts) ->
    ?INFO("Would compile: ~s", [Source]),
    %% Phase 3 will implement actual compilation
    ok.

%% @doc Clean compiled files
-spec clean([file:filename()], rebar_app_info:t()) -> ok.
clean(Files, _AppInfo) ->
    lists:foreach(
        fun(File) ->
            case file:delete(File) of
                ok ->
                    ?DEBUG("Deleted: ~s", [File]),
                    ok;
                {error, enoent} ->
                    ok;
                {error, Reason} ->
                    ?WARN("Failed to delete ~s: ~p", [File, Reason])
            end
        end,
        Files
    ),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

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
