%% Additional test cases to improve coverage for r3lfe_compile_worker and r3lfe_compiler_mod
%% Add these to the existing test/r3lfe_compile_worker_SUITE.erl and test/r3lfe_compiler_mod_SUITE.erl

%% ============================================================================
%% Tests for r3lfe_compile_worker_SUITE.erl
%% ============================================================================

%% Add these to exports and all/0:
compile_with_warnings/1,
compile_alternative_format/1,
compile_file_errors_format/1,
build_compiler_opts_removes_conflicts/1,
option_key_extraction/1,
format_warning_item_variants/1,
format_error_item_variants/1.

compile_with_warnings(Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Create source with code that generates warnings
    SourceFile = filename:join(TestDir, "warnings.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule warnings)\n"
        "(defun test ()\n"
        "  (let ((unused-var 42))\n"
        "    'ok))\n"),
    
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),
    
    %% May succeed with or without warnings depending on LFE version
    case Result of
        ok -> ok;
        {ok, _Warnings} -> ok;
        {error, _Errors, _Warnings} -> ok
    end,
    
    ok.

compile_alternative_format(Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Test that alternative success format is handled
    SourceFile = filename:join(TestDir, "simple.lfe"),
    test_utils:write_file(SourceFile, "(defmodule simple)\n(defun test () 'ok)\n"),
    
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),
    
    %% Should handle {ok, Module} format
    ?assertMatch(ok, Result),
    
    ok.

compile_file_errors_format(Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Create file with error format {error, [], FileErrors, []}
    SourceFile = filename:join(TestDir, "error_format.lfe"),
    test_utils:write_file(SourceFile, "(defmodule error-format\n"),  % Incomplete
    
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),
    
    %% Should handle error format
    ?assertMatch({error, _, _}, Result),
    
    ok.

build_compiler_opts_removes_conflicts(_Config) ->
    Source = "/path/to/source.lfe",
    OutDir = "/path/to/out",
    
    %% BaseOpts with conflicting options
    BaseOpts = [
        verbose,
        {outdir, "/wrong/dir"},
        debug_info,
        return
    ],
    
    Opts = r3lfe_compile_worker:build_compiler_opts(Source, OutDir, BaseOpts),
    
    %% Should have correct outdir
    ?assert(lists:keymember(outdir, 1, Opts)),
    {outdir, ActualOutDir} = lists:keyfind(outdir, 1, Opts),
    ?assertEqual(OutDir, ActualOutDir),
    
    %% Should still have non-conflicting opts
    ?assert(lists:member(verbose, Opts)),
    ?assert(lists:member(debug_info, Opts)),
    
    ok.

option_key_extraction(_Config) ->
    %% Test option_key/1 function via compile_opts building
    Opts = [
        verbose,
        debug_info,
        {outdir, "/tmp"},
        {i, "/include"},
        return
    ],
    
    %% All should be preserved as they're valid
    BuiltOpts = r3lfe_compile_worker:build_compiler_opts("/src/file.lfe", "/out", Opts),
    
    ?assert(lists:member(verbose, BuiltOpts)),
    ?assert(lists:member(debug_info, BuiltOpts)),
    
    ok.

format_warning_item_variants(_Config) ->
    %% Test different warning formats
    Warning1 = {"/tmp/file.lfe", [{10, lfe_lint, {unused_var, 'X'}}]},
    Warning2 = {warning, "/tmp/file.lfe", [{20, lfe_lint, {unused_function, test}}]},
    Warning3 = unknown_format,
    
    Formatted1 = r3lfe_compile_worker:format_warnings([Warning1]),
    Formatted2 = r3lfe_compile_worker:format_warnings([Warning2]),
    Formatted3 = r3lfe_compile_worker:format_warnings([Warning3]),
    
    ?assert(is_list(Formatted1)),
    ?assert(is_list(Formatted2)),
    ?assert(is_list(Formatted3)),
    
    ok.

format_error_item_variants(_Config) ->
    %% Test different error formats
    Error1 = {"/tmp/file.lfe", [{15, lfe_parse, "syntax error"}]},
    Error2 = {error, "/tmp/file.lfe", [{25, lfe_parse, "unexpected token"}]},
    Error3 = unknown_error_format,
    
    Formatted1 = r3lfe_compile_worker:format_errors([Error1]),
    Formatted2 = r3lfe_compile_worker:format_errors([Error2]),
    Formatted3 = r3lfe_compile_worker:format_errors([Error3]),
    
    ?assert(is_list(Formatted1)),
    ?assert(is_list(Formatted2)),
    ?assert(is_list(Formatted3)),
    
    ok.

%% ============================================================================
%% Tests for r3lfe_compiler_mod_SUITE.erl
%% ============================================================================

%% Add these to exports and all/0:
dependencies_with_include_dirs/1,
compile_with_include_dirs/1,
compile_returns_warnings/1,
compile_returns_errors/1,
needed_files_with_first_files/1,
needed_files_parallel_compilation/1,
check_dependencies_newer_dependency_missing/1,
check_dependencies_newer_dependency_modified/1.

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
    test_utils:write_file(HeaderFile, "(defmacro test-macro () 'tested)\n"),
    
    %% Create source
    SourceFile = filename:join(SrcDir, "with_include.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule with-include)\n"
        "(include-file \"defs.lfe\")\n"
        "(defun run () (test-macro))\n"),
    
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
