-module(compile_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    full_compilation_cycle/1,
    incremental_compilation/1,
    compile_with_includes/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        full_compilation_cycle,
        incremental_compilation,
        compile_with_includes
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    rb3lfe_dep_cache:init(),
    rb3lfe_compile_opts:init(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    rb3lfe_dep_cache:clear(),
    rb3lfe_compile_opts:clear_opts_cache(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

full_compilation_cycle(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),

    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create multiple source files
    Files = [
        {"module1.lfe", "(defmodule module1)\n(defun test1 () 'ok)\n"},
        {"module2.lfe", "(defmodule module2)\n(defun test2 () 'ok)\n"},
        {"module3.lfe", "(defmodule module3)\n(defun test3 () 'ok)\n"}
    ],

    lists:foreach(
        fun({Name, Content}) ->
            File = filename:join(SrcDir, Name),
            test_utils:write_file(File, Content)
        end,
        Files
    ),

    %% Compile all files
    Results = lists:map(
        fun({Name, _Content}) ->
            Source = filename:join(SrcDir, Name),
            rb3lfe_compile_worker:compile_file(Source, EbinDir, [])
        end,
        Files
    ),

    %% All should succeed
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

    %% Check all beam files exist
    lists:foreach(
        fun({Name, _Content}) ->
            BeamName = filename:basename(Name, ".lfe") ++ ".beam",
            BeamFile = filename:join(EbinDir, BeamName),
            ?assert(filelib:is_file(BeamFile))
        end,
        Files
    ),

    ok.

incremental_compilation(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create and compile initial file
    SourceFile = filename:join(SrcDir, "incremental.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule incremental)\n"
        "(defun version () 1)\n"),

    ok = rb3lfe_compile_worker:compile_file(SourceFile, EbinDir, []),

    BeamFile = filename:join(EbinDir, "incremental.beam"),
    ?assert(filelib:is_file(BeamFile)),

    InitialTime = filelib:last_modified(BeamFile),

    %% Sleep to ensure timestamp difference
    timer:sleep(1000),

    %% Modify source file
    test_utils:write_file(SourceFile,
        "(defmodule incremental)\n"
        "(defun version () 2)\n"),

    %% Create DAG to test needs_compilation
    G = digraph:new([acyclic]),
    digraph:add_vertex(G, SourceFile),

    OutMappings = [{".beam", EbinDir}],

    %% Should need compilation
    NeedsCompile = rb3lfe_compiler_mod:needs_compilation(
        G, SourceFile, OutMappings
    ),

    ?assert(NeedsCompile, "Modified file should need recompilation"),

    %% Compile again
    ok = rb3lfe_compile_worker:compile_file(SourceFile, EbinDir, []),

    UpdatedTime = filelib:last_modified(BeamFile),
    ?assert(UpdatedTime > InitialTime, "Beam file should be updated"),

    digraph:delete(G),
    ok.

compile_with_includes(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create header file
    HeaderFile = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(HeaderFile,
        "(defrecord person name age)\n"),

    %% Create source that uses the header
    SourceFile = filename:join(SrcDir, "with_includes.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule with-includes)\n"
        "(include-file \"records.lfe\")\n"
        "(defun make-person (name age)\n"
        "  (make-person name name age age))\n"),

    %% Compile with include path
    Opts = [{i, IncludeDir}],
    Result = rb3lfe_compile_worker:compile_file(SourceFile, EbinDir, Opts),

    %% Should succeed
    ?assertMatch(ok, Result),

    %% Check beam file exists
    BeamFile = filename:join(EbinDir, "with-includes.beam"),
    ?assert(filelib:is_file(BeamFile)),

    ok.
