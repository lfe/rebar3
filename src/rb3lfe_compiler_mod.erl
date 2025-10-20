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

-include("rb3lfe.hrl").

%%====================================================================
%% rebar_compiler callbacks
%%====================================================================

%% @doc Define the compiler context
%% This tells rebar3 where our source files are, what extensions to look for,
%% and where to put compiled output
-spec context(rebar_app_info:t()) -> #{
    src_dirs => [file:filename()],
    include_dirs => [file:filename()],
    src_ext => string(),
    out_mappings => [{string(), file:filename()}]
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
        out_mappings => [{?BEAM_EXTENSION, OutDir}]
    }.

%% @doc Determine which files need compilation
%% This is called by rebar3 to filter the file list based on timestamps
%% and dependencies. We'll implement full logic in Phase 2.
-spec needed_files(
    rebar_digraph:t(),
    [file:filename()],
    [{string(), file:filename()}],
    rebar_app_info:t()
) -> {{[file:filename()], term()}, {{[file:filename()], [file:filename()]}, term()}}.
needed_files(_G, FoundFiles, _OutMappings, AppInfo) ->
    %% Phase 1: Simple implementation - compile everything
    %% Phase 2 will add proper dependency checking via DAG

    FirstFiles = rb3lfe_config:get_first_files(AppInfo),

    %% Separate first files from regular files
    {First, Rest} = lists:partition(
        fun(File) -> lists:member(File, FirstFiles) end,
        FoundFiles
    ),

    ?DEBUG("Files to compile: ~p first, ~p regular",
           [length(First), length(Rest)]),

    %% Return format: {{Sequential, Opts}, {{Sequential, Parallel}, Opts}}
    %% For now, everything is sequential
    {{First, []}, {{Rest, []}, []}}.

%% @doc Extract dependencies from a source file
%% Returns list of header files this source depends on
%% Full implementation in Phase 2
-spec dependencies(file:filename(), file:filename(), rebar_app_info:t()) ->
    [file:filename()].
dependencies(_Source, _SourceDir, _AppInfo) ->
    %% Phase 1: No dependency tracking yet
    %% Phase 2 will implement header file scanning
    [].

%% @doc Compile a source file
%% Full implementation in Phase 3
-spec compile(file:filename(), [{string(), file:filename()}],
              rebar_dict:t(), list()) -> ok.
compile(Source, _OutMappings, _Dict, _Opts) ->
    ?INFO("Would compile: ~s", [Source]),
    %% Phase 1: Stub implementation
    %% Phase 3 will add actual lfe_comp:file calls
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
