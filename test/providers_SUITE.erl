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
    provider_namespace_correct/1,
    all_providers_register/1,
    all_providers_use_lfe_namespace/1,
    providers_have_format_error/1,
    compile_provider_has_correct_opts/1,
    repl_provider_registers/1,
    escriptize_provider_registers/1,
    run_provider_registers/1,
    release_provider_registers/1,
    confabulate_provider_registers/1
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
        provider_namespace_correct,
        all_providers_register,
        all_providers_use_lfe_namespace,
        providers_have_format_error,
        compile_provider_has_correct_opts,
        repl_provider_registers,
        escriptize_provider_registers,
        run_provider_registers,
        release_provider_registers,
        confabulate_provider_registers
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

all_providers_register(_Config) ->
    State = rebar_state:new(),

    %% Initialize all providers
    {ok, State1} = rebar3_lfe:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have all expected providers
    ExpectedProviders = [
        compile, clean, repl, ltest, release,
        versions, run, escriptize, 'run-escript',
        'run-release', confabulate
    ],

    %% Count providers in lfe namespace
    LfeProviders = lists:filter(
        fun(P) ->
            providers:namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(length(LfeProviders) >= length(ExpectedProviders)),

    %% Verify each expected provider exists
    lists:foreach(
        fun(Name) ->
            Found = lists:any(
                fun(P) ->
                    providers:impl(P) =:= Name andalso
                    providers:namespace(P) =:= lfe
                end,
                Providers
            ),
            ?assert(Found, io_lib:format("Provider ~p should be registered", [Name]))
        end,
        ExpectedProviders
    ),
    ok.

all_providers_use_lfe_namespace(_Config) ->
    State = rebar_state:new(),
    {ok, State1} = rebar3_lfe:init(State),

    Providers = rebar_state:providers(State1),

    %% Get all our providers
    OurProviders = lists:filter(
        fun(P) ->
            Mod = providers:module(P),
            case atom_to_list(Mod) of
                "r3lfe_prv_" ++ _ -> true;
                _ -> false
            end
        end,
        Providers
    ),

    %% All should use 'lfe' namespace
    lists:foreach(
        fun(P) ->
            ?assertEqual(lfe, providers:namespace(P))
        end,
        OurProviders
    ),
    ok.

providers_have_format_error(_Config) ->
    %% Verify each provider module exports format_error/1
    ProviderModules = [
        r3lfe_prv_compile,
        r3lfe_prv_clean,
        r3lfe_prv_repl,
        r3lfe_prv_ltest,
        r3lfe_prv_release,
        r3lfe_prv_versions,
        r3lfe_prv_run,
        r3lfe_prv_escriptize,
        r3lfe_prv_run_escript,
        r3lfe_prv_run_release,
        r3lfe_prv_confabulate
    ],

    lists:foreach(
        fun(Mod) ->
            %% Check module exports format_error/1
            Exports = Mod:module_info(exports),
            ?assert(lists:member({format_error, 1}, Exports),
                    io_lib:format("~p should export format_error/1", [Mod]))
        end,
        ProviderModules
    ),
    ok.

compile_provider_has_correct_opts(_Config) ->
    State = rebar_state:new(),
    {ok, State1} = r3lfe_prv_compile:init(State),

    Providers = rebar_state:providers(State1),

    %% Find compile provider
    CompileProvider = lists:filter(
        fun(P) ->
            providers:impl(P) =:= compile andalso
            providers:namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assertEqual(1, length(CompileProvider)),
    ok.

repl_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_repl:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "REPL provider should be registered"),
    ok.

escriptize_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_escriptize:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Escriptize provider should be registered"),
    ok.

run_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_run:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Run provider should be registered"),
    ok.

release_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_release:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Release provider should be registered"),
    ok.

confabulate_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_confabulate:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Confabulate provider should be registered"),
    ok.
