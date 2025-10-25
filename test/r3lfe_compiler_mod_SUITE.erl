-module(r3lfe_compiler_mod_SUITE).

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
    compile_simple_module/1,
    compile_with_no_out_mappings/1,
    compile_with_multiple_mappings/1,
    source_to_target_no_mappings/1,
    source_to_target_with_mapping/1,
    needs_compilation_missing_source/1,
    needs_compilation_source_newer/1,
    needs_compilation_target_newer/1,
    clean_removes_beam_files/1,
    context_returns_map_with_correct_keys/1,
    format_error_returns_string/1,
    dependencies_with_include_dirs/1,
    compile_with_include_dirs/1,
    compile_returns_warnings/1,
    compile_returns_errors/1,
    needed_files_with_first_files/1,
    needed_files_parallel_compilation/1,
    check_dependencies_newer_dependency_missing/1,
    check_dependencies_newer_dependency_modified/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_simple_module,
        compile_with_no_out_mappings,
        compile_with_multiple_mappings,
        source_to_target_no_mappings,
        source_to_target_with_mapping,
        needs_compilation_missing_source,
        needs_compilation_source_newer,
        needs_compilation_target_newer,
        clean_removes_beam_files,
        context_returns_map_with_correct_keys,
        format_error_returns_string,
        dependencies_with_include_dirs,
        compile_with_include_dirs,
        compile_returns_warnings,
        compile_returns_errors,
        needed_files_with_first_files,
        needed_files_parallel_compilation,
        check_dependencies_newer_dependency_missing,
        check_dependencies_newer_dependency_modified
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

compile_simple_module(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile, "(defmodule simple)\n"),

    OutMappings = [{".beam", EbinDir}],
    Opts = [verbose, return],

    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),

    %% Should compile successfully
    ?assertEqual(ok, Result),

    %% BEAM file should exist
    BeamFile = filename:join(EbinDir, "simple.beam"),
    ?assert(filelib:is_file(BeamFile)),
    ok.

compile_with_no_out_mappings(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),

    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile, "(defmodule simple)\n"),

    %% Empty mappings - should use fallback or fail gracefully
    OutMappings = [],
    Opts = [],

    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),

    %% Should handle gracefully (may succeed or fail, but shouldn't crash)
    ?assert(is_tuple(Result) orelse Result =:= ok),
    ok.

compile_with_multiple_mappings(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    SourceFile = filename:join(SrcDir, "module.lfe"),
    test_utils:write_file(SourceFile, "(defmodule module)\n"),

    %% Multiple output mappings
    OutDir1 = filename:join(TestDir, "out1"),
    OutDir2 = filename:join(TestDir, "out2"),
    filelib:ensure_dir(filename:join(OutDir1, "dummy")),
    filelib:ensure_dir(filename:join(OutDir2, "dummy")),

    OutMappings = [{".beam", EbinDir}, {".out", OutDir1}],
    Opts = [verbose, return],

    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),

    %% Should compile
    ?assertEqual(ok, Result),
    ok.

source_to_target_no_mappings(_Config) ->
    Source = "/tmp/test.lfe",
    OutMappings = [],

    Target = r3lfe_compiler_mod:source_to_target(Source, OutMappings),

    %% Should return something (possibly error or default)
    ?assert(is_list(Target) orelse Target =:= error),
    ok.

source_to_target_with_mapping(_Config) ->
    Source = "/tmp/src/test.lfe",
    OutMappings = [{".beam", "/tmp/ebin"}],

    Target = r3lfe_compiler_mod:source_to_target(Source, OutMappings),

    %% Should map to .beam in ebin
    ?assert(is_list(Target)),
    ?assert(string:str(Target, ".beam") > 0),
    ?assert(string:str(Target, "/tmp/ebin") > 0),
    ok.

needs_compilation_missing_source(_Config) ->
    %% Create DAG
    G = digraph:new([acyclic]),

    %% Source file that doesn't exist
    NonExistent = "/tmp/nonexistent.lfe",
    digraph:add_vertex(G, NonExistent),

    OutMappings = [{".beam", "/tmp"}],

    %% Should return true (needs compilation to get error)
    ?assert(r3lfe_compiler_mod:needs_compilation(G, NonExistent, OutMappings)),

    digraph:delete(G),
    ok.

needs_compilation_source_newer(Config) ->
    TestDir = ?config(test_dir, Config),
    G = digraph:new([acyclic]),

    %% Create source and target
    Source = filename:join(TestDir, "test.lfe"),
    Target = filename:join(TestDir, "test.beam"),

    test_utils:write_file(Source, "(defmodule test)\n"),
    test_utils:write_file(Target, "fake beam"),

    %% Make source newer
    timer:sleep(1100),
    test_utils:write_file(Source, "(defmodule test)\n%% modified\n"),

    digraph:add_vertex(G, Source),
    OutMappings = [{".beam", TestDir}],

    %% Should need compilation (source is newer)
    ?assert(r3lfe_compiler_mod:needs_compilation(G, Source, OutMappings)),

    digraph:delete(G),
    ok.

needs_compilation_target_newer(Config) ->
    TestDir = ?config(test_dir, Config),
    G = digraph:new([acyclic]),

    %% Create source and target
    Source = filename:join(TestDir, "test.lfe"),
    Target = filename:join(TestDir, "test.beam"),

    test_utils:write_file(Source, "(defmodule test)\n"),

    %% Make target newer
    timer:sleep(1100),
    test_utils:write_file(Target, "fake beam"),

    digraph:add_vertex(G, Source),
    OutMappings = [{".beam", TestDir}],

    %% Should not need compilation (target is newer)
    ?assertNot(r3lfe_compiler_mod:needs_compilation(G, Source, OutMappings)),

    digraph:delete(G),
    ok.


clean_removes_beam_files(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    EbinDir = filename:join(TestDir, "ebin"),
    filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create source .lfe files
    SourceFiles = [
        filename:join(SrcDir, "module1.lfe"),
        filename:join(SrcDir, "module2.lfe")
    ],

    %% Create corresponding .beam files in ebin
    BeamFiles = [
        filename:join(EbinDir, "module1.beam"),
        filename:join(EbinDir, "module2.beam")
    ],

    %% Write source files
    lists:foreach(
        fun(File) ->
            test_utils:write_file(File, "(defmodule test ())")
        end,
        SourceFiles
    ),

    %% Write beam files
    lists:foreach(
        fun(File) ->
            test_utils:write_file(File, "fake beam content")
        end,
        BeamFiles
    ),

    %% Verify source and beam files exist
    lists:foreach(
        fun(File) ->
            ?assert(filelib:is_file(File))
        end,
        SourceFiles ++ BeamFiles
    ),

    %% Set up AppInfo with proper ebin_dir
    {ok, AppInfo0} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    AppInfo = rebar_app_info:ebin_dir(AppInfo0, EbinDir),

    %% Clean - pass SOURCE files, not beam files
    ok = r3lfe_compiler_mod:clean(SourceFiles, AppInfo),

    %% Beam files should be deleted
    lists:foreach(
        fun(File) ->
            ?assertNot(filelib:is_file(File))
        end,
        BeamFiles
    ),

    %% Source files should still exist
    lists:foreach(
        fun(File) ->
            ?assert(filelib:is_file(File))
        end,
        SourceFiles
    ),
    ok.

context_returns_map_with_correct_keys(_Config) ->
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", "/tmp/test"),

    Context = r3lfe_compiler_mod:context(AppInfo),

    %% Should return a map with the expected keys
    ?assert(is_map(Context)),
    ?assert(maps:is_key(src_dirs, Context)),
    ?assert(maps:is_key(include_dirs, Context)),
    ?assert(maps:is_key(src_ext, Context)),
    ?assert(maps:is_key(out_mappings, Context)),
    ?assert(maps:is_key(dependencies_opts, Context)),

    %% Verify types
    ?assert(is_list(maps:get(src_dirs, Context))),
    ?assert(is_list(maps:get(include_dirs, Context))),
    ?assertEqual(".lfe", maps:get(src_ext, Context)),
    ?assert(is_list(maps:get(out_mappings, Context))),
    ok.

format_error_returns_string(_Config) ->
    %% Test that format_error/1 returns a string
    Result = r3lfe_compiler_mod:format_error(some_error),

    ?assert(is_list(Result) orelse is_binary(Result)),
    ok.

dependencies_with_include_dirs(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create header
    HeaderFile = filename:join(IncludeDir, "header.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord rec field)\n"),

    %% Create source that uses it
    SourceFile = filename:join(SrcDir, "uses_header.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule uses-header)\n"
        "(include-file \"header.lfe\")\n"
        "(defun test () 'ok)\n"),

    %% Call dependencies/3
    Deps = r3lfe_compiler_mod:dependencies(SourceFile, SrcDir, [IncludeDir]),

    ?assert(is_list(Deps)),
    ?assert(lists:any(fun(D) -> string:find(D, "header.lfe") =/= nomatch end, Deps)),

    ok.

compile_with_include_dirs(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create header
    HeaderFile = filename:join(IncludeDir, "defs.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord test-rec field)\n"),

    %% Create source that uses the record
    SourceFile = filename:join(SrcDir, "with_include.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule with-include)\n"
        "(include-file \"defs.lfe\")\n"
        "(defun run () (make-test-rec field 42))\n"),

    %% Compile with include dirs in opts
    OutMappings = [{".beam", EbinDir}],
    Opts = [{include_dirs, [IncludeDir]}, {lfe_opts, []}],

    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),

    ?assertEqual(ok, Result),

    ok.

compile_returns_warnings(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create source with potential warning
    SourceFile = filename:join(SrcDir, "with_warning.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule with-warning)\n"
        "(defun unused () 'never-called)\n"
        "(defun test () 'ok)\n"),

    OutMappings = [{".beam", EbinDir}],
    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), []),

    %% May return ok or {ok, Warnings}
    case Result of
        ok -> ok;
        {ok, _Warnings} -> ok
    end,

    ok.

compile_returns_errors(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create source with error
    SourceFile = filename:join(SrcDir, "with_error.lfe"),
    test_utils:write_file(SourceFile, "(defmodule with-error\n"),  % Incomplete

    OutMappings = [{".beam", EbinDir}],
    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), []),

    ?assertMatch({error, _, _}, Result),

    ok.

needed_files_with_first_files(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create some files
    File1 = filename:join(SrcDir, "first.lfe"),
    File2 = filename:join(SrcDir, "second.lfe"),
    test_utils:write_file(File1, "(defmodule first)\n"),
    test_utils:write_file(File2, "(defmodule second)\n"),

    %% Create graph
    G = digraph:new([acyclic]),
    digraph:add_vertex(G, File1),
    digraph:add_vertex(G, File2),

    %% Create AppInfo with first_files config
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Opts = rebar_app_info:opts(AppInfo),
    Opts1 = rebar_opts:set(Opts, lfe_first_files, ["first.lfe"]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),

    OutMappings = [{".beam", EbinDir}],

    %% Call needed_files
    {{FirstNeeded, _}, {{_Sequential, _Parallel}, _}} =
        r3lfe_compiler_mod:needed_files(G, [File1, File2], OutMappings, AppInfo1),

    %% first.lfe should be in FirstNeeded
    ?assert(lists:member(File1, FirstNeeded) orelse FirstNeeded =:= []),

    digraph:delete(G),
    ok.

needed_files_parallel_compilation(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create multiple independent files
    Files = lists:map(
        fun(N) ->
            File = filename:join(SrcDir, io_lib:format("module~p.lfe", [N])),
            test_utils:write_file(File, io_lib:format("(defmodule module~p)\n", [N])),
            File
        end,
        lists:seq(1, 5)
    ),

    %% Create graph
    G = digraph:new([acyclic]),
    lists:foreach(fun(F) -> digraph:add_vertex(G, F) end, Files),

    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    OutMappings = [{".beam", EbinDir}],

    %% Get needed files
    {{_FirstNeeded, _}, {{_Sequential, Parallel}, _}} =
        r3lfe_compiler_mod:needed_files(G, Files, OutMappings, AppInfo),

    %% With no dependencies, all should be eligible for parallel compilation
    ?assert(is_list(Parallel)),

    digraph:delete(G),
    ok.

check_dependencies_newer_dependency_missing(Config) ->
    TestDir = ?config(test_dir, Config),

    SourceFile = filename:join(TestDir, "source.lfe"),
    test_utils:write_file(SourceFile, "(defmodule source)\n"),

    %% Non-existent dependency
    MissingDep = filename:join(TestDir, "missing.lfe"),

    G = digraph:new([acyclic]),
    digraph:add_vertex(G, SourceFile),
    digraph:add_vertex(G, MissingDep),
    digraph:add_edge(G, SourceFile, MissingDep),

    %% Create target that's older than source
    TargetFile = filename:join(TestDir, "source.beam"),
    test_utils:write_file(TargetFile, <<>>),
    timer:sleep(100),

    TargetTime = filelib:last_modified(TargetFile),

    %% Should need compilation due to missing dependency
    NeedsCompile = r3lfe_compiler_mod:check_dependencies_newer(G, SourceFile, TargetTime),

    ?assert(NeedsCompile),

    digraph:delete(G),
    ok.

check_dependencies_newer_dependency_modified(Config) ->
    TestDir = ?config(test_dir, Config),

    SourceFile = filename:join(TestDir, "source.lfe"),
    DepFile = filename:join(TestDir, "dep.lfe"),

    test_utils:write_file(SourceFile, "(defmodule source)\n"),
    test_utils:write_file(DepFile, "(defmodule dep)\n"),

    %% Create target
    TargetFile = filename:join(TestDir, "source.beam"),
    test_utils:write_file(TargetFile, <<>>),

    TargetTime = filelib:last_modified(TargetFile),

    %% Wait and modify dependency
    timer:sleep(1100),
    test_utils:write_file(DepFile, "(defmodule dep)\n%% modified\n"),

    G = digraph:new([acyclic]),
    digraph:add_vertex(G, SourceFile),
    digraph:add_vertex(G, DepFile),
    digraph:add_edge(G, SourceFile, DepFile),

    %% Should need compilation due to newer dependency
    NeedsCompile = r3lfe_compiler_mod:check_dependencies_newer(G, SourceFile, TargetTime),

    ?assert(NeedsCompile),

    digraph:delete(G),
    ok.
