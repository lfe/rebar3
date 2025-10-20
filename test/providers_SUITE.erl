-module(providers_SUITE).

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
    compile_provider_registers/1,
    clean_provider_registers/1,
    compile_provider_works/1,
    clean_provider_works/1,
    versions_provider_works/1,
    provider_namespace_correct/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_provider_registers,
        clean_provider_registers,
        compile_provider_works,
        clean_provider_works,
        versions_provider_works,
        provider_namespace_correct
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

compile_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_compile:init(State),

    %% Check provider is registered by checking state has providers
    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Compile provider should be registered"),
    ok.

clean_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_clean:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Clean provider should be registered"),
    ok.

compile_provider_works(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir, "testapp"),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    %% Create a simple LFE file
    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule simple)\n"
        "(defun test () 'ok)\n"),

    %% Create rebar state with this app
    State = rebar_state:new(),
    {ok, AppInfo} = rebar_app_info:new(testapp, "0.1.0", AppDir),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    %% Initialize and run compile provider
    {ok, State2} = r3lfe_prv_compile:init(State1),

    Result = r3lfe_prv_compile:do(State2),

    ?assertMatch({ok, _}, Result),

    %% Check beam file was created
    BeamFile = filename:join(maps:get(ebin_dir, AppData), "simple.beam"),
    ?assert(filelib:is_file(BeamFile)),

    ok.

clean_provider_works(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir, "testapp"),
    AppDir = maps:get(dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    %% Create a fake beam file
    BeamFile = filename:join(EbinDir, "fake.beam"),
    test_utils:write_file(BeamFile, <<>>),

    ?assert(filelib:is_file(BeamFile)),

    %% Create state and run clean
    State = rebar_state:new(),
    {ok, AppInfo} = rebar_app_info:new(testapp, "0.1.0", AppDir),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    {ok, State2} = r3lfe_prv_clean:init(State1),
    {ok, _State3} = r3lfe_prv_clean:do(State2),

    %% Beam file should be deleted
    ?assertNot(filelib:is_file(BeamFile)),

    ok.

versions_provider_works(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_versions:init(State),

    %% Should not error
    Result = r3lfe_prv_versions:do(State1),

    ?assertMatch({ok, _}, Result),
    ok.

provider_namespace_correct(_Config) ->
    State = rebar_state:new(),

    %% Initialize all providers
    {ok, State1} = rebar3_lfe:init(State),

    %% Get all providers
    Providers = rebar_state:providers(State1),

    %% Should have registered at least 6 providers
    %% (compile, clean, repl, ltest, release, versions)
    ?assert(length(Providers) >= 6, "Should have at least 6 providers registered"),

    ok.
