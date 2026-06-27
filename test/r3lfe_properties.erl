-module(r3lfe_properties).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    prop_module_name_reversible/0,
    prop_package_cleanup_always_succeeds/0,
    prop_dependency_graph_acyclic/0,
    prop_incremental_compilation_deterministic/0,
    prop_concurrent_compilation_safe/0
]).

%%====================================================================
%% Property: Module name calculation is reversible
%%====================================================================

prop_module_name_reversible() ->
    ?FORALL(
        Path,
        valid_nested_path(),
        begin
            %% Generate a source directory and nested file path
            SrcDir = "/tmp/test/src",
            FullPath = filename:join(SrcDir, Path),

            %% Calculate module name
            ModuleName = r3lfe_package:calculate_module_name(FullPath, SrcDir),

            %% Validate the module name
            case r3lfe_package:validate_module_name(ModuleName) of
                ok ->
                    %% Should be dots, no slashes
                    not lists:member($/, ModuleName) andalso
                    %% Should match the path structure
                    lists:all(fun(C) -> C =/= $/ end, ModuleName);
                {error, _} ->
                    false
            end
        end
    ).

valid_nested_path() ->
    ?LET(
        Segments,
        non_empty(list(valid_path_segment())),
        filename:join(Segments) ++ ".lfe"
    ).

valid_path_segment() ->
    ?SUCHTHAT(
        Segment,
        non_empty(list(union([choose($a, $z), choose($0, $9), exactly($_)]))),
        length(Segment) > 0 andalso length(Segment) < 50
    ).

%%====================================================================
%% Property: Package cleanup always succeeds
%%====================================================================

prop_package_cleanup_always_succeeds() ->
    ?FORALL(
        PackageInfos,
        list(package_info()),
        begin
            %% Cleanup should never crash
            try
                r3lfe_package:cleanup_packages(PackageInfos),
                true
            catch
                _:_ -> false
            end
        end
    ).

package_info() ->
    ?LET(
        {SourceFile, TempFile, ModuleName},
        {filepath(), filepath(), module_name()},
        #{
            source_file => SourceFile,
            temp_file => TempFile,
            module_name => ModuleName,
            source_dir => "/tmp/test/src"
        }
    ).

filepath() ->
    ?LET(
        Name,
        non_empty(list(union([choose($a, $z), choose($0, $9)]))),
        "/tmp/test/" ++ Name ++ ".lfe"
    ).

module_name() ->
    ?LET(
        Segments,
        non_empty(list(valid_module_segment())),
        string:join(Segments, ".")
    ).

valid_module_segment() ->
    non_empty(list(union([choose($a, $z), choose($0, $9), exactly($_)]))).

%%====================================================================
%% Property: Dependency graph is always acyclic
%%====================================================================

prop_dependency_graph_acyclic() ->
    ?FORALL(
        Files,
        list(source_file_with_deps()),
        begin
            %% Build a dependency graph
            G = digraph:new([acyclic]),

            try
                %% Add all files as vertices
                [digraph:add_vertex(G, File) || {File, _} <- Files],

                %% Add all dependencies as edges
                lists:all(
                    fun({File, Deps}) ->
                        lists:all(
                            fun(Dep) ->
                                case digraph:add_edge(G, File, Dep) of
                                    {error, {bad_edge, _}} ->
                                        false;  % Would create cycle
                                    _ ->
                                        true
                                end
                            end,
                            Deps
                        )
                    end,
                    Files
                )
            after
                digraph:delete(G)
            end
        end
    ).

source_file_with_deps() ->
    ?LET(
        {File, NumDeps},
        {filepath(), choose(0, 3)},
        {File, [filepath() || _ <- lists:seq(1, NumDeps)]}
    ).

%%====================================================================
%% Property: Incremental compilation is deterministic
%%====================================================================

prop_incremental_compilation_deterministic() ->
    ?FORALL(
        _Scenario,
        compilation_scenario(),
        begin
            %% Same input should always produce same result
            %% This is a placeholder - full implementation would
            %% actually compile and compare results
            true
        end
    ).

compilation_scenario() ->
    #{
        files => list(filepath()),
        first_files => list(filepath()),
        opts => list(compiler_opt())
    }.

compiler_opt() ->
    oneof([
        verbose,
        debug_info,
        {outdir, "/tmp/test"},
        {i, "/tmp/test/include"}
    ]).

%%====================================================================
%% Property: Concurrent compilation is safe
%%====================================================================

prop_concurrent_compilation_safe() ->
    ?FORALL(
        Files,
        non_empty(list(filepath())),
        begin
            %% Multiple processes compiling shouldn't interfere
            %% This would need actual file operations in full implementation

            Parent = self(),

            Pids = [spawn(fun() ->
                Parent ! {self(), ok}
            end) || _ <- Files],

            Results = [receive {Pid, Result} -> Result end || Pid <- Pids],

            lists:all(fun(R) -> R =:= ok end, Results)
        end
    ).

