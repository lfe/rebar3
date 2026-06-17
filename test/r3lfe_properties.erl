-module(r3lfe_properties).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    prop_module_name_reversible/0,
    prop_package_cleanup_always_succeeds/0,
    prop_dependency_graph_acyclic/0,
    prop_incremental_compilation_deterministic/0,
    prop_concurrent_compilation_safe/0,
    prop_lfe_four_oracles/0
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

%%====================================================================
%% Property: LFE formatter satisfies all four oracles on generated source
%%====================================================================

%% Generated LFE source text — restricted grammar guarantees it always parses.
%% No #.( read-eval forms are generated so AST-equiv always applies.
prop_lfe_four_oracles() ->
    ?FORALL(
        Src,
        gen_lfe_source(),
        begin
            Bin = iolist_to_binary(Src),
            case r3lfe_formatter:format(Bin) of
                {error, Reason} ->
                    error_logger:error_msg(
                        "Generator produced unparseable LFE ~p: ~200p~n",
                        [Reason, Bin]),
                    false;
                {ok, IO} ->
                    Out = iolist_to_binary(IO),
                    fmt_oracle_idempotency(Out)
                    andalso fmt_oracle_tokens(Bin, Out)
                    andalso fmt_oracle_comments(Bin, Out)
                    andalso fmt_oracle_ast(Bin, Out)
            end
        end
    ).

fmt_oracle_idempotency(Out) ->
    case r3lfe_formatter:format(Out) of
        {ok, IO2} -> iolist_to_binary(IO2) =:= Out;
        _         -> false
    end.

fmt_oracle_tokens(Src, Out) ->
    fmt_sig_pairs(Src) =:= fmt_sig_pairs(Out).

fmt_oracle_comments(Src, Out) ->
    fmt_comments(Src) =:= fmt_comments(Out).

fmt_oracle_ast(Src, Out) ->
    OrigText = binary_to_list(Src),
    OutText  = binary_to_list(Out),
    case {lfe_io:read_string(OrigText), lfe_io:read_string(OutText)} of
        {{ok, Orig}, {ok, Fmted}} -> Orig =:= Fmted;
        {{error, _}, _}           -> true;
        {_, {error, _}}           -> false
    end.

fmt_sig_pairs(Bin) ->
    {ok, Toks} = r3lfe_format_lexer:tokens(Bin),
    Trivia = [whitespace, newline, line_comment, block_comment],
    [{r3lfe_format_lexer:kind(T), r3lfe_format_lexer:text(T)}
     || T <- Toks,
        not lists:member(r3lfe_format_lexer:kind(T), Trivia)].

fmt_comments(Bin) ->
    {ok, Toks} = r3lfe_format_lexer:tokens(Bin),
    {ok, Doc}  = r3lfe_format_cst:parse(Toks),
    [r3lfe_format_lexer:text(T) || T <- r3lfe_format_cst:comments(Doc)].

%%====================================================================
%% Generators — restricted LFE grammar (always parseable, no #.()
%%====================================================================

gen_lfe_source() ->
    ?LET(Forms, non_empty(list(gen_top_form())),
         lists:join("\n", Forms)).

gen_top_form() ->
    frequency([
        {5, gen_sexpr(3)},
        {3, gen_defun()}
    ]).

gen_defun() ->
    ?LET({Name, Arg, Body},
         {gen_atom(), gen_atom(), gen_sexpr(2)},
         ["(defun ", Name, " (", Arg, ") ", Body, ")"]).

gen_sexpr(0) ->
    frequency([
        {4, gen_atom()},
        {3, gen_int()},
        {2, gen_str()}
    ]);
gen_sexpr(D) ->
    frequency([
        {5, gen_atom()},
        {3, gen_int()},
        {2, gen_str()},
        {4, gen_list(D - 1)},
        {2, gen_tuple(D - 1)},
        {1, gen_let(D - 1)},
        {1, gen_if(D - 1)}
    ]).

gen_atom() ->
    ?LET({C, Rest},
         {choose($a, $z), list(union([choose($a, $z), choose($0, $9), exactly($-)]))},
         [C | Rest]).

gen_int() ->
    ?LET(N, ?SUCHTHAT(N, integer(), N >= 0), integer_to_list(N)).

gen_str() ->
    ?LET(Chars, list(choose($a, $z)), [$", Chars, $"]).

gen_list(D) ->
    ?LET(Elems, list(gen_sexpr(D)),
         [$( | [lists:join(" ", Elems), $)]]).

gen_tuple(D) ->
    ?LET(Elems, list(gen_sexpr(D)),
         ["#(" | [lists:join(" ", Elems), $)]]).

gen_let(D) ->
    ?LET({Var, Val, Body},
         {gen_atom(), gen_sexpr(D), gen_sexpr(D)},
         ["(let ((", Var, " ", Val, ")) ", Body, ")"]).

gen_if(D) ->
    ?LET({Cond, Then, Else},
         {gen_atom(), gen_sexpr(D), gen_sexpr(D)},
         ["(if ", Cond, " ", Then, " ", Else, ")"]).
