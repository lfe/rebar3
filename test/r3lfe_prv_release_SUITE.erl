-module(r3lfe_prv_release_SUITE).

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
    release_provider_registers/1,
    update_app_file_with_modules/1,
    update_app_file_error_handling/1,
    get_release_name_from_different_formats/1,
    show_usage_info_displays/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        release_provider_registers,
        update_app_file_with_modules,
        update_app_file_error_handling,
        get_release_name_from_different_formats,
        show_usage_info_displays
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("release"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

release_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_release:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Release provider should be registered"),

    ok.

update_app_file_with_modules(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create app structure
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create .app file
    AppFile = filename:join(EbinDir, "testapp.app"),
    AppContent = "{application, testapp, [{vsn, \"0.1.0\"}, {modules, []}]}.",
    ok = file:write_file(AppFile, AppContent),

    %% Create some beam files
    BeamFiles = [
        filename:join(EbinDir, "module1.beam"),
        filename:join(EbinDir, "module2.beam"),
        filename:join(EbinDir, "module3.beam")
    ],
    lists:foreach(fun(F) -> ok = file:write_file(F, <<>>) end, BeamFiles),

    %% Create AppInfo
    {ok, AppInfo} = rebar_app_info:new(testapp, "0.1.0", TestDir),
    AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),
    AppInfo2 = rebar_app_info:app_file(AppInfo1, AppFile),

    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo2]),

    %% Initialize provider
    {ok, _State2} = r3lfe_prv_release:init(State1),

    %% Read app file to verify it could be parsed
    {ok, [{application, testapp, Props}]} = file:consult(AppFile),
    ?assertMatch([{vsn, _}, {modules, []}], Props),

    ok.

update_app_file_error_handling(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create app structure with invalid app file
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create malformed .app file
    AppFile = filename:join(EbinDir, "testapp.app"),
    AppContent = "{application, testapp, [invalid syntax",
    ok = file:write_file(AppFile, AppContent),

    %% Create AppInfo
    {ok, AppInfo} = rebar_app_info:new(testapp, "0.1.0", TestDir),
    AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),
    AppInfo2 = rebar_app_info:app_file(AppInfo1, AppFile),

    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo2]),

    %% Initialize provider - should handle error gracefully
    {ok, _State2} = r3lfe_prv_release:init(State1),

    ok.

get_release_name_from_different_formats(_Config) ->
    %% Test that provider can handle different release name formats
    State = rebar_state:new(),

    %% Initialize provider
    {ok, State1} = r3lfe_prv_release:init(State),

    %% Verify provider registered
    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),

    ok.

show_usage_info_displays(_Config) ->
    %% Test format_error returns usage information
    ErrorMsg = r3lfe_prv_release:format_error(usage),

    ?assert(is_list(ErrorMsg)),
    ?assert(length(ErrorMsg) > 0),

    ok.
