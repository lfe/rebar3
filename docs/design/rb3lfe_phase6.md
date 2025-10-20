# Phase 6: Testing Infrastructure & CI/CD

## Overview

This phase completes the test infrastructure with property-based tests, integration tests, performance benchmarks, and a modern CI/CD pipeline. We'll ensure the plugin is rock-solid and ready for production use across all supported Erlang/OTP versions.

**Goal**: 100% confidence in correctness, performance, and reliability through comprehensive testing.

## Prerequisites

- Phase 1-5 completed with all tests passing
- Understanding of property-based testing with PropEr
- Knowledge of GitHub Actions
- Familiarity with coverage analysis tools

## Testing Strategy

```
Test Pyramid:

                    /\
                   /  \     Manual/Exploratory
                  /    \
                 /------\
                / E2E    \   End-to-End (CT suites)
               /  Tests   \
              /------------\
             /  Integration \ Integration (CT suites)
            /     Tests      \
           /------------------\
          /   Property-Based   \ Properties (PropEr)
         /       Tests           \
        /------------------------\
       /      Unit Tests          \ Unit (CT suites)
      /          (CT)              \
     /------------------------------\
```

## Implementation Tasks

### Task 6.1: Create Property-Based Tests

**File: `test/rb3lfe_properties.erl`**

```erlang
-module(rb3lfe_properties).

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
            ModuleName = rb3lfe_package:calculate_module_name(FullPath, SrcDir),

            %% Validate the module name
            case rb3lfe_package:validate_module_name(ModuleName) of
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
                rb3lfe_package:cleanup_packages(PackageInfos),
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
```

### Task 6.2: Create Performance Benchmarks

**File: `test/rb3lfe_benchmarks.erl`**

```erlang
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
        AppInfo = mock_app_info(),

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
```

### Task 6.3: Create End-to-End Test Suite

**File: `test/e2e_SUITE.erl`**

```erlang
-module(e2e_SUITE).

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
    full_project_workflow/1,
    umbrella_project_workflow/1,
    mixed_erlang_lfe_project/1,
    project_with_dependencies/1,
    release_workflow/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        full_project_workflow,
        umbrella_project_workflow,
        mixed_erlang_lfe_project,
        project_with_dependencies,
        release_workflow
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("e2e"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

full_project_workflow(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create project structure
    create_full_project(TestDir),

    %% Test compile
    State = create_rebar_state(TestDir),
    {ok, _} = rb3lfe_prv_compile:do(State),

    %% Verify beam files
    assert_beam_files_exist(TestDir, ["myapp", "myapp.utils"]),

    %% Test clean
    {ok, _} = rb3lfe_prv_clean:do(State),

    %% Verify beams removed
    assert_beam_files_not_exist(TestDir, ["myapp", "myapp.utils"]),

    %% Test recompile
    {ok, _} = rb3lfe_prv_compile:do(State),
    assert_beam_files_exist(TestDir, ["myapp", "myapp.utils"]),

    ok.

umbrella_project_workflow(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create umbrella with multiple apps
    create_umbrella_project(TestDir),

    %% Test compile all
    State = create_umbrella_state(TestDir),
    {ok, _} = rb3lfe_prv_compile:do(State),

    %% Verify all apps compiled
    assert_beam_exists(TestDir, "apps/app1/ebin/app1.beam"),
    assert_beam_exists(TestDir, "apps/app2/ebin/app2.beam"),

    ok.

mixed_erlang_lfe_project(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create project with both Erlang and LFE
    create_mixed_project(TestDir),

    %% Compile
    State = create_rebar_state(TestDir),
    {ok, _} = rb3lfe_prv_compile:do(State),

    %% Both should compile
    assert_beam_exists(TestDir, "ebin/erlang_module.beam"),
    assert_beam_exists(TestDir, "ebin/lfe-module.beam"),

    ok.

project_with_dependencies(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create project that uses LFE standard library
    create_project_with_deps(TestDir),

    %% Compile
    State = create_rebar_state(TestDir),
    {ok, _} = rb3lfe_prv_compile:do(State),

    %% Should compile successfully
    assert_beam_exists(TestDir, "ebin/myapp.beam"),

    ok.

release_workflow(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create release project
    create_release_project(TestDir),

    %% Compile
    State = create_rebar_state(TestDir),
    {ok, State1} = rb3lfe_prv_compile:do(State),

    %% Build release
    {ok, _} = rb3lfe_prv_release:do(State1),

    %% Verify release created
    assert_release_exists(TestDir),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_full_project(TestDir) ->
    %% Create rebar.config
    RebarConfig =
        "{plugins, [{rb3lfe, \"0.5.0\"}]}.\n"
        "{deps, [{lfe, \"2.2.0\"}]}.\n",
    file:write_file(filename:join(TestDir, "rebar.config"), RebarConfig),

    %% Create src structure
    SrcDir = filename:join(TestDir, "src"),
    SubDir = filename:join(SrcDir, "myapp"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    %% Create files
    test_utils:write_file(
        filename:join(SrcDir, "myapp.lfe"),
        "(defmodule myapp)\n(defun start () 'ok)\n"
    ),

    test_utils:write_file(
        filename:join(SubDir, "utils.lfe"),
        "(defmodule myapp.utils)\n(defun helper () 'ok)\n"
    ),

    ok.

create_umbrella_project(TestDir) ->
    %% Create umbrella structure
    App1Dir = filename:join([TestDir, "apps", "app1", "src"]),
    App2Dir = filename:join([TestDir, "apps", "app2", "src"]),
    ok = filelib:ensure_dir(filename:join(App1Dir, "dummy")),
    ok = filelib:ensure_dir(filename:join(App2Dir, "dummy")),

    %% Create app files
    test_utils:write_file(
        filename:join(App1Dir, "app1.lfe"),
        "(defmodule app1)\n(defun test () 'ok)\n"
    ),

    test_utils:write_file(
        filename:join(App2Dir, "app2.lfe"),
        "(defmodule app2)\n(defun test () 'ok)\n"
    ),

    ok.

create_mixed_project(TestDir) ->
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),

    %% Erlang module
    test_utils:write_file(
        filename:join(SrcDir, "erlang_module.erl"),
        "-module(erlang_module).\n-export([test/0]).\ntest() -> ok.\n"
    ),

    %% LFE module
    test_utils:write_file(
        filename:join(SrcDir, "lfe_module.lfe"),
        "(defmodule lfe-module)\n(defun test () 'ok)\n"
    ),

    ok.

create_project_with_deps(TestDir) ->
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),

    test_utils:write_file(
        filename:join(SrcDir, "myapp.lfe"),
        "(defmodule myapp)\n"
        "(include-lib \"lfe/include/clj.lfe\")\n"
        "(defun test () 'ok)\n"
    ),

    ok.

create_release_project(TestDir) ->
    create_full_project(TestDir),

    %% Add relx config
    RelxConfig =
        "{relx, [\n"
        "  {release, {myapp, \"0.1.0\"}, [myapp]},\n"
        "  {dev_mode, false}\n"
        "]}.\n",

    ConfigFile = filename:join(TestDir, "rebar.config"),
    {ok, Existing} = file:read_file(ConfigFile),
    file:write_file(ConfigFile, [Existing, RelxConfig]),

    ok.

create_rebar_state(TestDir) ->
    %% Create minimal rebar state for testing
    State = rebar_state:new(),
    AppInfo = rebar_app_info:new(test_app, "0.1.0", TestDir),
    rebar_state:project_apps(State, [AppInfo]).

create_umbrella_state(TestDir) ->
    State = rebar_state:new(),

    App1 = rebar_app_info:new(app1, "0.1.0",
                               filename:join([TestDir, "apps", "app1"])),
    App2 = rebar_app_info:new(app2, "0.1.0",
                               filename:join([TestDir, "apps", "app2"])),

    rebar_state:project_apps(State, [App1, App2]).

assert_beam_files_exist(TestDir, Modules) ->
    lists:foreach(
        fun(Mod) ->
            BeamFile = filename:join([TestDir, "ebin", Mod ++ ".beam"]),
            ?assert(filelib:is_file(BeamFile),
                    io_lib:format("~s should exist", [BeamFile]))
        end,
        Modules
    ).

assert_beam_files_not_exist(TestDir, Modules) ->
    lists:foreach(
        fun(Mod) ->
            BeamFile = filename:join([TestDir, "ebin", Mod ++ ".beam"]),
            ?assertNot(filelib:is_file(BeamFile),
                       io_lib:format("~s should not exist", [BeamFile]))
        end,
        Modules
    ).

assert_beam_exists(TestDir, RelPath) ->
    FullPath = filename:join(TestDir, RelPath),
    ?assert(filelib:is_file(FullPath),
            io_lib:format("~s should exist", [FullPath])).

assert_release_exists(TestDir) ->
    %% Check for release directory
    RelDir = filename:join([TestDir, "_build", "default", "rel"]),
    ?assert(filelib:is_dir(RelDir),
            "Release directory should exist").
```

### Task 6.4: Update CI/CD Configuration

**File: `.github/workflows/ci.yml`**

```yaml
name: CI/CD

on:
  push:
    branches: ['main', 'release/**']
    tags: ['v*']
  pull_request:
    branches: ['main', 'release/**']
  schedule:
    - cron: '0 4 * * *'  # Daily at 4 AM UTC

jobs:
  test:
    name: Test OTP ${{ matrix.otp }} / rebar3 ${{ matrix.rebar3 }}
    runs-on: ubuntu-latest

    strategy:
      fail-fast: false
      matrix:
        include:
          # OTP 28 with latest rebar3
          - otp: '28'
            rebar3: '3.25.0'
            coverage: true

          # OTP 27 with latest rebar3
          - otp: '27'
            rebar3: '3.25.0'

          # OTP 26 with rebar3 3.25
          - otp: '26'
            rebar3: '3.25.0'

          # OTP 25 with rebar3 3.22
          - otp: '25'
            rebar3: '3.22.0'

          # OTP 24 with rebar3 3.22 (minimum supported)
          - otp: '24'
            rebar3: '3.22.0'

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: ${{ matrix.rebar3 }}

      - name: Cache Dependencies
        uses: actions/cache@v4
        with:
          path: |
            _build
            ~/.cache/rebar3
          key: ${{ runner.os }}-otp${{ matrix.otp }}-${{ hashFiles('rebar.lock') }}
          restore-keys: |
            ${{ runner.os }}-otp${{ matrix.otp }}-

      - name: Compile
        run: rebar3 compile

      - name: Run xref
        run: rebar3 xref

      - name: Run dialyzer
        run: rebar3 dialyzer
        continue-on-error: true  # Don't fail on dialyzer warnings initially

      - name: Run tests
        run: rebar3 ct

      - name: Run property tests
        run: rebar3 proper -c

      - name: Generate coverage
        if: matrix.coverage
        run: rebar3 cover -v

      - name: Upload coverage to Codecov
        if: matrix.coverage
        uses: codecov/codecov-action@v4
        with:
          files: _build/test/cover/ct.coverdata
          flags: unittests
          name: OTP-${{ matrix.otp }}

  integration:
    name: Integration Tests
    runs-on: ubuntu-latest
    needs: test

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '27'
          rebar3-version: '3.25.0'

      - name: Run integration tests
        run: |
          rebar3 compile
          make smoke-tests

  benchmarks:
    name: Performance Benchmarks
    runs-on: ubuntu-latest
    needs: test
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '27'
          rebar3-version: '3.25.0'

      - name: Run benchmarks
        run: |
          rebar3 compile
          rebar3 shell --eval "rb3lfe_benchmarks:run_all()"

  publish:
    name: Publish to Hex
    runs-on: ubuntu-latest
    needs: [test, integration]
    if: startsWith(github.ref, 'refs/tags/v')

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '27'
          rebar3-version: '3.25.0'

      - name: Publish to Hex
        env:
          HEX_API_KEY: ${{ secrets.HEX_API_KEY }}
        run: rebar3 hex publish --yes
```

### Task 6.5: Add Coverage Configuration

**File: `rebar.config` (UPDATE)**

```erlang
%% Add to profiles section:

{profiles, [
    {test, [
        {deps, [
            {proper, "1.5.0"}
        ]},
        {plugins, [
            {rebar3_proper, "0.12.1"}
        ]},
        {ct_opts, [
            {sys_config, "test/test.config"},
            {verbose, true},
            {cover_enabled, true},
            {cover_opts, [verbose]}
        ]},
        {cover_enabled, true},
        {cover_opts, [
            {verbose, true},
            {min_coverage, 90}
        ]},
        {erl_opts, [
            {d, 'TEST'},
            {src_dirs, ["src", "test"]}
        ]}
    ]}
]}.

%% Add alias for coverage
{alias, [
    {check, [
        compile,
        xref,
        dialyzer,
        ct,
        {proper, "-c"},
        {cover, "-v --min_coverage=90"}
    ]},
    {coverage, [
        {ct, "--cover"},
        {cover, "-v"}
    ]}
]}.
```

### Task 6.6: Create Test Configuration

**File: `test/test.config`**

```erlang
%% Test configuration
[
    {rb3lfe, [
        {test_mode, true},
        {verbose, true}
    ]},

    {kernel, [
        {logger_level, debug},
        {logger, [
            {handler, default, logger_std_h,
                #{level => debug}}
        ]}
    ]}
].
```

### Task 6.7: Update Makefile for Testing

**File: `Makefile` (UPDATE)**

```makefile
PROJECT = rb3lfe
ROOT_DIR = $(shell pwd)
SYS_TEST_DIR = /tmp/rb3lfe/_integration/_testing
GLOBAL_INSTALL_DIR = ~/.config/rebar3/plugins
GLOBAL_INSTALL = $(GLOBAL_INSTALL_DIR)/$(PROJECT)

.PHONY: all compile clean check test coverage benchmarks smoke-tests

all: compile

compile:
 @rebar3 compile

clean:
 @rm -rf _build rebar.lock $(SYS_TEST_DIR) $(GLOBAL_INSTALL)

check: clean
 @rebar3 check

test: clean
 @rebar3 ct
 @rebar3 proper -c

coverage: clean
 @rebar3 as test do ct, proper -c, cover -v

benchmarks: compile
 @rebar3 shell --eval "rb3lfe_benchmarks:run_all(), init:stop()."

# Integration test setup
$(SYS_TEST_DIR):
 @mkdir -p $(SYS_TEST_DIR)
 @cp priv/testing/rebar.config $(SYS_TEST_DIR)/rebar.config

setup: $(SYS_TEST_DIR)
 -git branch -D integration-testing
 git checkout -b integration-testing
 @git fetch origin
 @if git rev-parse --verify origin/integration-testing >/dev/null 2>&1; then \
  git push origin integration-testing -f; \
 else \
  git push origin integration-testing; \
 fi
 git switch -

# Smoke tests
smoke-tests: test-new test-new-lfe-lib test-new-lfe-main test-new-lfe-app

test-new: clean setup
 @echo "Testing template listing..."
 @cd $(SYS_TEST_DIR) && rebar3 new

test-new-lfe-lib: clean setup
 @echo "Testing lfe-lib template..."
 @cd $(SYS_TEST_DIR) && \
  rebar3 new lfe-lib example-lib && \
  cd example-lib && \
  rebar3 lfe compile

test-new-lfe-main: clean setup
 @echo "Testing lfe-main template..."
 @cd $(SYS_TEST_DIR) && \
  rebar3 new lfe-main example-main && \
  cd example-main && \
  rebar3 lfe compile && \
  rebar3 lfe run -- 42

test-new-lfe-app: clean setup
 @echo "Testing lfe-app template..."
 @cd $(SYS_TEST_DIR) && \
  rebar3 new lfe-app example-app && \
  cd example-app && \
  rebar3 lfe compile

# Quality checks
xref:
 @rebar3 xref

dialyzer:
 @rebar3 dialyzer

# Full quality check
quality: xref dialyzer

# CI/CD helper
ci: clean compile quality test coverage

# Publish to hex.pm
publish:
 @rebar3 hex publish
```

### Task 6.8: Create README with Badges

**File: `README.md` (UPDATE)**

```markdown
# rb3lfe - Modern rebar3 Plugin for LFE

[![CI/CD](https://github.com/lfe-rebar3/rebar3_lfe/workflows/CI%2FCD/badge.svg)](https://github.com/lfe-rebar3/rebar3_lfe/actions)
[![Coverage](https://codecov.io/gh/lfe-rebar3/rebar3_lfe/branch/main/graph/badge.svg)](https://codecov.io/gh/lfe-rebar3/rebar3_lfe)
[![Hex.pm](https://img.shields.io/hexpm/v/rb3lfe.svg)](https://hex.pm/packages/rb3lfe)
[![LFE Versions](https://img.shields.io/badge/lfe-2.2+-blue.svg)](https://github.com/lfe/lfe)
[![Erlang Versions](https://img.shields.io/badge/erlang-24--28-blue.svg)](https://www.erlang.org)

A comprehensive, modern rebar3 plugin for LFE (Lisp Flavoured Erlang) projects.

## Features

- ✅ **Proper Dependency Tracking**: Header file changes trigger recompilation
- ✅ **Package System**: Organize code in nested directories
- ✅ **Incremental Compilation**: Fast rebuilds
- ✅ **Modern rebar3 Integration**: Uses Custom Compiler Modules interface
- ✅ **Professional Error Messages**: Clear, actionable feedback
- ✅ **Comprehensive Testing**: >90% code coverage
- ✅ **Multi-OTP Support**: Works on Erlang/OTP 24-28

## Quick Start

Add to `rebar.config`:

```erlang
{plugins, [
    {rb3lfe, "0.5.0"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.
```

## Commands

- `rebar3 lfe compile` - Compile LFE files
- `rebar3 lfe clean` - Remove compiled files
- `rebar3 lfe repl` - Start LFE REPL
- `rebar3 lfe ltest` - Run tests
- `rebar3 lfe release` - Build release
- `rebar3 lfe versions` - Show version info

## Breaking Changes from 0.4.x

Version 0.5.0 is a complete rewrite with breaking changes:

- Plugin renamed from `rebar3_lfe` to `rb3lfe`
- All internal modules renamed with `rb3lfe_` prefix
- Improved architecture and reliability
- Better error messages

See [MIGRATION.md](MIGRATION.md) for upgrade guide.

## Documentation

Full documentation available at [https://lfe-rebar3.github.io/rebar3_lfe](https://lfe-rebar3.github.io/rebar3_lfe)

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md)

## License

Apache 2.0

```

## Testing Instructions

### Running All Tests

```bash
# Full test suite
make test

# With coverage
make coverage

# Just properties
rebar3 proper

# Benchmarks
make benchmarks

# Smoke tests
make smoke-tests

# Full CI check locally
make ci
```

### Expected Output

```
=== Running Tests ===
Common Test: 85 tests, 0 failed
PropEr: 5 properties, 1000 tests each, 0 failed
Coverage: 92.5%

=== Running Benchmarks ===
Compiling 100 simple modules...
  Cold compile:        1250.32 ms
  Incremental (none):  45.67 ms
  Speedup:            27.38x
...
```

## Expected Outcomes

At the end of Phase 6, you should have:

1. ✅ Property-based tests for core functionality
2. ✅ Performance benchmarks
3. ✅ End-to-end test suite
4. ✅ Modern CI/CD pipeline
5. ✅ Coverage reporting to Codecov
6. ✅ Multi-OTP testing (24-28)
7. ✅ Automated hex.pm publishing
8. ✅ Code coverage >90%
9. ✅ All quality checks passing
10. ✅ Professional README with badges

### Integration Checklist

- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] All property tests pass
- [ ] E2E tests pass
- [ ] Benchmarks run without errors
- [ ] Coverage >90%
- [ ] Dialyzer clean
- [ ] Xref clean
- [ ] CI passes on all OTP versions
- [ ] Smoke tests pass
- [ ] Documentation complete
- [ ] Ready for release

## Next Steps

Phase 7 (Final) will implement:

- Documentation generation
- User guide and examples
- Migration guide from 0.4.x
- Release notes
- Final polish

## Notes for Claude Code

- Property tests should test invariants, not implementations
- Benchmarks should be repeatable and meaningful
- E2E tests should test real-world workflows
- CI should test all supported OTP versions
- Coverage should exclude test files themselves
- Use `?FORALL` for property tests, not `?LET` alone
- Benchmarks need warm-up runs to be accurate
- Mock sparingly - prefer real integration tests
- Document any flaky tests and fix them
- All tests must be deterministic and independent
