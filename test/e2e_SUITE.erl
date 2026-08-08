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
    versions_command_works/1
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
        versions_command_works
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
    {ok, _} = r3lfe_prv_compile:do(State),

    %% Verify beam files
    assert_beam_files_exist(TestDir, ["myapp", "myapp.utils"]),

    %% Test clean
    {ok, _} = r3lfe_prv_clean:do(State),

    %% Verify beams removed
    assert_beam_files_not_exist(TestDir, ["myapp", "myapp.utils"]),

    %% Test recompile
    {ok, _} = r3lfe_prv_compile:do(State),
    assert_beam_files_exist(TestDir, ["myapp", "myapp.utils"]),

    ok.

umbrella_project_workflow(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create umbrella with multiple apps
    create_umbrella_project(TestDir),

    %% Test compile all
    State = create_umbrella_state(TestDir),
    {ok, _} = r3lfe_prv_compile:do(State),

    %% Verify all apps compiled
    assert_beam_exists(TestDir, "apps/app1/ebin/app1.beam"),
    assert_beam_exists(TestDir, "apps/app2/ebin/app2.beam"),

    ok.

mixed_erlang_lfe_project(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create project with both Erlang and LFE
    create_mixed_project(TestDir),

    %% Compile just LFE (Erlang compilation is separate)
    State = create_rebar_state(TestDir),
    {ok, _} = r3lfe_prv_compile:do(State),

    %% LFE module should compile
    assert_beam_exists(TestDir, "ebin/lfe-module.beam"),

    ok.

project_with_dependencies(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create project that uses LFE standard library
    create_project_with_deps(TestDir),

    %% Compile
    State = create_rebar_state(TestDir),
    {ok, _} = r3lfe_prv_compile:do(State),

    %% Should compile successfully
    assert_beam_exists(TestDir, "ebin/myapp.beam"),

    ok.

versions_command_works(_Config) ->
    %% Test versions provider
    State = rebar_state:new(),
    {ok, State1} = r3lfe_prv_versions:init(State),
    {ok, _} = r3lfe_prv_versions:do(State1),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_full_project(TestDir) ->
    %% Create rebar.config
    RebarConfig =
        "{plugins, [{rebar3_lfe, \"0.5.0\"}]}.\n"
        "{deps, [{lfe, \"~> 2.0\"}]}.\n",
    file:write_file(filename:join(TestDir, "rebar.config"), RebarConfig),

    %% Create src structure
    SrcDir = filename:join(TestDir, "src"),
    SubDir = filename:join(SrcDir, "myapp"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    %% Create ebin directory
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

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
    App1Ebin = filename:join([TestDir, "apps", "app1", "ebin"]),
    App2Ebin = filename:join([TestDir, "apps", "app2", "ebin"]),

    ok = filelib:ensure_dir(filename:join(App1Dir, "dummy")),
    ok = filelib:ensure_dir(filename:join(App2Dir, "dummy")),
    ok = filelib:ensure_dir(filename:join(App1Ebin, "dummy")),
    ok = filelib:ensure_dir(filename:join(App2Ebin, "dummy")),

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
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

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
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    test_utils:write_file(
        filename:join(SrcDir, "myapp.lfe"),
        "(defmodule myapp)\n"
        "(defun test () 'ok)\n"
    ),

    ok.

create_rebar_state(TestDir) ->
    %% Create minimal rebar state for testing
    State = rebar_state:new(),
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    rebar_state:project_apps(State, [AppInfo]).

create_umbrella_state(TestDir) ->
    State = rebar_state:new(),

    {ok, App1} = rebar_app_info:new(app1, "0.1.0",
                               filename:join([TestDir, "apps", "app1"])),
    {ok, App2} = rebar_app_info:new(app2, "0.1.0",
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
