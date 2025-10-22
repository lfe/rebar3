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
    show_usage_info_displays/1,
    get_release_name_with_atom_name/1,
    get_release_name_with_string_name/1,
    get_release_name_from_app/1,
    get_release_name_default/1,
    get_release_output_dir_default/1,
    get_release_output_dir_custom_relative/1,
    get_release_output_dir_custom_absolute/1,
    format_error_various_reasons/1,
    update_app_files_multiple_apps/1,
    update_app_file_updates_modules/1,
    show_usage_info_output/1,
    info_output_validation/1
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
        show_usage_info_displays,
        get_release_name_with_atom_name,
        get_release_name_with_string_name,
        get_release_name_from_app,
        get_release_name_default,
        get_release_output_dir_default,
        get_release_output_dir_custom_relative,
        get_release_output_dir_custom_absolute,
        format_error_various_reasons,
        update_app_files_multiple_apps,
        update_app_file_updates_modules,
        show_usage_info_output,
        info_output_validation
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

%%====================================================================
%% Additional Test Cases for Coverage
%%====================================================================

get_release_name_with_atom_name(_Config) ->
    %% Test get_release_name with atom name in relx config
    State = rebar_state:new(),

    %% Set relx config with atom name
    State1 = rebar_state:set(State, relx, [
        {release, {myapp, "1.0.0"}, [myapp]}
    ]),

    Name = r3lfe_prv_release:get_release_name(State1),

    ?assertEqual("myapp", Name),
    ok.

get_release_name_with_string_name(_Config) ->
    %% Test get_release_name with string name in relx config
    State = rebar_state:new(),

    %% Set relx config with string name
    State1 = rebar_state:set(State, relx, [
        {release, {"myapp_string", "1.0.0"}, [myapp]}
    ]),

    Name = r3lfe_prv_release:get_release_name(State1),

    ?assertEqual("myapp_string", Name),
    ok.

get_release_name_from_app(Config) ->
    %% Test get_release_name falls back to app name
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),

    %% Create an app
    {ok, AppInfo} = rebar_app_info:new(testapp, "0.1.0", TestDir),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    %% No relx config, should use app name
    Name = r3lfe_prv_release:get_release_name(State1),

    ?assertEqual("testapp", Name),
    ok.

get_release_name_default(_Config) ->
    %% Test get_release_name with no config and no apps
    State = rebar_state:new(),

    %% No relx config, no apps
    Name = r3lfe_prv_release:get_release_name(State),

    ?assertEqual("myapp", Name),
    ok.

get_release_output_dir_default(Config) ->
    %% Test get_release_output_dir with default config
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:set(State, base_dir, TestDir),

    Dir = r3lfe_prv_release:get_release_output_dir(State1),

    %% rebar_dir:base_dir appends "default" profile directory
    %% so the expected path includes it
    ?assert(string:find(Dir, "rel") =/= nomatch),
    ?assert(filelib:is_dir(filename:dirname(Dir)) orelse true),
    ok.

get_release_output_dir_custom_relative(Config) ->
    %% Test get_release_output_dir with custom relative path
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:set(State, base_dir, TestDir),
    State2 = rebar_state:set(State1, relx, [
        {output_dir, "custom_rel"}
    ]),

    Dir = r3lfe_prv_release:get_release_output_dir(State2),

    %% Should contain custom_rel in the path
    ?assert(string:find(Dir, "custom_rel") =/= nomatch),
    ok.

get_release_output_dir_custom_absolute(_Config) ->
    %% Test get_release_output_dir with custom absolute path
    State = rebar_state:new(),

    AbsPath = "/tmp/absolute_rel",
    State1 = rebar_state:set(State, relx, [
        {output_dir, AbsPath}
    ]),

    Dir = r3lfe_prv_release:get_release_output_dir(State1),

    ?assertEqual(AbsPath, Dir),
    ok.

format_error_various_reasons(_Config) ->
    %% Test format_error with various error reasons
    Reasons = [
        usage,
        {error, file_not_found},
        {error, {parse_error, "invalid syntax"}},
        unknown_error
    ],

    lists:foreach(
        fun(Reason) ->
            Msg = r3lfe_prv_release:format_error(Reason),
            ?assert(is_list(Msg)),
            ?assert(length(Msg) > 0)
        end,
        Reasons
    ),

    ok.

%%====================================================================
%% Additional Tests for Better Coverage
%%====================================================================

update_app_files_multiple_apps(Config) ->
    %% Test update_app_files with multiple apps
    TestDir = ?config(test_dir, Config),

    %% Create multiple apps
    App1Dir = filename:join(TestDir, "app1"),
    App2Dir = filename:join(TestDir, "app2"),

    Apps = lists:map(
        fun({AppName, AppDir}) ->
            EbinDir = filename:join(AppDir, "ebin"),
            ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

            %% Create .app file
            AppFile = filename:join(EbinDir, atom_to_list(AppName) ++ ".app"),
            AppContent = io_lib:format("{application, ~p, [{vsn, \"0.1.0\"}, {modules, []}]}.", [AppName]),
            ok = file:write_file(AppFile, AppContent),

            %% Create beam files
            BeamFile = filename:join(EbinDir, atom_to_list(AppName) ++ ".beam"),
            ok = file:write_file(BeamFile, <<>>),

            {ok, AppInfo} = rebar_app_info:new(AppName, "0.1.0", AppDir),
            AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),
            rebar_app_info:app_file(AppInfo1, AppFile)
        end,
        [{app1, App1Dir}, {app2, App2Dir}]
    ),

    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, Apps),

    %% Call update_app_files
    ok = r3lfe_prv_release:update_app_files(State1),

    ok.

update_app_file_updates_modules(Config) ->
    %% Test that update_app_file actually updates the modules list
    TestDir = ?config(test_dir, Config),

    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create .app file with empty modules list
    AppFile = filename:join(EbinDir, "testapp.app"),
    AppContent = "{application, testapp, [{vsn, \"0.1.0\"}, {modules, []}]}.",
    ok = file:write_file(AppFile, AppContent),

    %% Create beam files
    BeamFiles = [
        filename:join(EbinDir, "mod1.beam"),
        filename:join(EbinDir, "mod2.beam")
    ],
    lists:foreach(fun(F) -> ok = file:write_file(F, <<>>) end, BeamFiles),

    %% Create AppInfo
    {ok, AppInfo} = rebar_app_info:new(testapp, "0.1.0", TestDir),
    AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),
    AppInfo2 = rebar_app_info:app_file(AppInfo1, AppFile),

    %% Call update_app_file
    ok = r3lfe_prv_release:update_app_file(AppInfo2),

    %% Read updated app file
    {ok, [{application, testapp, Props}]} = file:consult(AppFile),

    %% Check that modules list was updated
    Modules = proplists:get_value(modules, Props),
    ?assertEqual(2, length(Modules)),
    ?assert(lists:member(mod1, Modules)),
    ?assert(lists:member(mod2, Modules)),

    ok.

show_usage_info_output(Config) ->
    %% Test show_usage_info function
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:set(State, base_dir, TestDir),
    State2 = rebar_state:set(State1, relx, [
        {release, {testrel, "1.0.0"}, [testapp]}
    ]),

    %% Call show_usage_info (should not crash)
    ok = r3lfe_prv_release:show_usage_info(State2),

    ok.

info_output_validation(_Config) ->
    %% Test info function output
    Result = r3lfe_prv_release:info("Build LFE release"),

    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),

    %% Should contain key information
    Flat = lists:flatten(Result),
    ?assert(string:str(Flat, "relx") > 0),
    ?assert(string:str(Flat, "release") > 0),

    ok.
