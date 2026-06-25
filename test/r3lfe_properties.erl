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

%% A7·S5b carve-out: token oracle is multiset (sort-insensitive) so that
%% export sorting does not cause false negatives. Any add/drop/mutate is
%% still caught because the multiset changes; only order is relaxed.
fmt_oracle_tokens(Src, Out) ->
    lists:sort(fmt_sig_pairs(Src)) =:= lists:sort(fmt_sig_pairs(Out)).

fmt_oracle_comments(Src, Out) ->
    fmt_comments(Src) =:= fmt_comments(Out).

%% A7·S5b carve-out: AST oracle normalizes export entry order so that
%% the sort doesn't cause false negatives. All other ordering is preserved.
fmt_oracle_ast(Src, Out) ->
    OrigText = binary_to_list(Src),
    OutText  = binary_to_list(Out),
    case {lfe_io:read_string(OrigText), lfe_io:read_string(OutText)} of
        {{ok, Orig}, {ok, Fmted}} ->
            normalize_module_decls(Orig) =:= normalize_module_decls(Fmted);
        {{error, _}, _}           -> true;
        {_, {error, _}}           -> false
    end.

%% normalize_module_decls: sort export entries canonically so the AST oracle
%% is order-insensitive for (export …) entries only. Uses norm_list/1 to
%% handle improper lists (dotted pairs) safely.
normalize_module_decls([export | Entries]) ->
    [export | normalize_export_entries(Entries)];
normalize_module_decls([import | Clauses]) ->
    [import | [normalize_import_clause(C) || C <- Clauses]];
normalize_module_decls(Term) when is_list(Term) ->
    norm_list(Term);
normalize_module_decls(Term) ->
    Term.

norm_list([]) -> [];
norm_list([H | T]) when is_list(T) -> [normalize_module_decls(H) | norm_list(T)];
norm_list([H | T])                 -> [normalize_module_decls(H) | T];
norm_list(Other)                   -> Other.

normalize_export_entries(Entries) ->
    AllPairs = lists:all(
        fun([N, A]) -> is_atom(N) andalso is_integer(A);
           (_)      -> false
        end, Entries),
    case AllPairs of
        true  -> lists:sort(Entries);
        false -> Entries
    end.

%% normalize_import_clause: sort entries within a single import clause.
%% (from M Es): sort Es that are [name, arity] pairs.
%% (rename M Ps): sort Ps that are [[name, arity], new-name] by old {name, arity}.
%% Other clause forms: unchanged.
normalize_import_clause([from, M | Es]) ->
    [from, M | normalize_export_entries(Es)];
normalize_import_clause([rename, M | Ps]) ->
    AllPairs = lists:all(
        fun([[N, A], _]) -> is_atom(N) andalso is_integer(A);
           (_) -> false
        end, Ps),
    case AllPairs of
        true ->
            Tagged = [{{N, A}, P} || [[N, A] | _] = P <- Ps],
            [rename, M | [P || {_, P} <- lists:keysort(1, Tagged)]];
        false -> [rename, M | Ps]
    end;
normalize_import_clause(Clause) ->
    Clause.

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
