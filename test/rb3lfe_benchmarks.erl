-module(rb3lfe_benchmarks).

-export([
    run_all/0,
    benchmark_compilation/0,
    benchmark_dependency_scanning/0,
    benchmark_package_preparation/0
]).

-define(ITERATIONS, 100).

%%====================================================================
%% Public API
%%====================================================================

run_all() ->
    io:format("~n=== rb3lfe Performance Benchmarks ===~n~n"),

    benchmark_compilation(),
    benchmark_dependency_scanning(),
    benchmark_package_preparation(),

    io:format("~n=== Benchmarks Complete ===~n~n"),
    ok.

%%====================================================================
%% Compilation Benchmark
%%====================================================================

benchmark_compilation() ->
    io:format("Compiling 100 simple modules...~n"),

    %% Setup
    TestDir = setup_benchmark_project(100),

    try
        %% Warm up
        compile_project(TestDir),

        %% Benchmark cold compilation
        ColdTime = timer:tc(fun() -> compile_project(TestDir) end),

        %% Benchmark incremental (no changes)
        IncrementalTime = timer:tc(fun() -> compile_project(TestDir) end),

        io:format("  Cold compile:        ~.2f ms~n", [element(1, ColdTime) / 1000]),
        io:format("  Incremental (none):  ~.2f ms~n", [element(1, IncrementalTime) / 1000]),
        io:format("  Speedup:            ~.2fx~n~n",
                  [element(1, ColdTime) / element(1, IncrementalTime)])
    after
        cleanup_benchmark_project(TestDir)
    end.

%%====================================================================
%% Dependency Scanning Benchmark
%%====================================================================

benchmark_dependency_scanning() ->
    io:format("Scanning dependencies in 1000 files...~n"),

    %% Create test files with varying include counts
    Files = lists:map(
        fun(N) ->
            create_test_file_with_includes(N, N rem 10)
        end,
        lists:seq(1, 1000)
    ),

    try
        %% Create mock AppInfo
        {ok, AppInfo} = mock_app_info(),

        %% Benchmark
        {Time, _Results} = timer:tc(fun() ->
            [rb3lfe_dependency_scanner:scan_file(F, AppInfo) || F <- Files]
        end),

        io:format("  Total time:  ~.2f ms~n", [Time / 1000]),
        io:format("  Per file:    ~.2f ms~n~n", [Time / 1000 / 1000])
    after
        [file:delete(F) || F <- Files]
    end.

%%====================================================================
%% Package Preparation Benchmark
%%====================================================================

benchmark_package_preparation() ->
    io:format("Preparing 500 package files...~n"),

    TestDir = setup_package_benchmark(500),

    try
        AllFiles = rb3lfe_package:discover_files(TestDir),

        %% Benchmark preparation
        {PrepTime, {ok, PackageInfos}} = timer:tc(fun() ->
            rb3lfe_package:prepare_packages(AllFiles)
        end),

        %% Benchmark cleanup
        {CleanTime, _} = timer:tc(fun() ->
            rb3lfe_package:cleanup_packages(PackageInfos)
        end),

        io:format("  Preparation: ~.2f ms~n", [PrepTime / 1000]),
        io:format("  Cleanup:     ~.2f ms~n", [CleanTime / 1000]),
        io:format("  Total:       ~.2f ms~n~n", [(PrepTime + CleanTime) / 1000])
    after
        cleanup_benchmark_project(TestDir)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

setup_benchmark_project(NumFiles) ->
    TestDir = test_utils:create_temp_dir("benchmark"),
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),

    %% Create N simple files
    lists:foreach(
        fun(N) ->
            File = filename:join(SrcDir, io_lib:format("module~p.lfe", [N])),
            Content = io_lib:format(
                "(defmodule module~p)~n"
                "(defun test~p () 'ok)~n",
                [N, N]
            ),
            file:write_file(File, Content)
        end,
        lists:seq(1, NumFiles)
    ),

    TestDir.

setup_package_benchmark(NumFiles) ->
    TestDir = test_utils:create_temp_dir("pkg_benchmark"),
    SrcDir = filename:join(TestDir, "src"),

    %% Create nested structure
    lists:foreach(
        fun(N) ->
            SubDir = filename:join([SrcDir, "pkg" ++ integer_to_list(N)]),
            ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

            File = filename:join(SubDir, "module.lfe"),
            Content = io_lib:format(
                "(defmodule pkg~p.module)~n"
                "(defun test () 'ok)~n",
                [N]
            ),
            file:write_file(File, Content)
        end,
        lists:seq(1, NumFiles)
    ),

    SrcDir.

cleanup_benchmark_project(TestDir) ->
    file:del_dir_r(TestDir).

compile_project(TestDir) ->
    %% Simplified compilation for benchmarking
    SrcDir = filename:join(TestDir, "src"),
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    Files = filelib:wildcard(filename:join(SrcDir, "*.lfe")),

    [rb3lfe_compile_worker:compile_file(F, EbinDir, []) || F <- Files],
    ok.

create_test_file_with_includes(N, IncludeCount) ->
    File = filename:join("/tmp", io_lib:format("test~p.lfe", [N])),

    Includes = [
        io_lib:format("(include-file \"header~p.lfe\")~n", [I])
        || I <- lists:seq(1, IncludeCount)
    ],

    Content = [
        io_lib:format("(defmodule test~p)~n", [N]),
        Includes,
        "(defun test () 'ok)~n"
    ],

    file:write_file(File, Content),
    File.

mock_app_info() ->
    rebar_app_info:new(test_app, "0.1.0", "/tmp/test").
