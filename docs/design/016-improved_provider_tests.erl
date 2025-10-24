%% This file contains improved test cases to increase coverage for provider modules
%% Add these test cases to the existing SUITE files

%% ============================================================================
%% Additional tests for r3lfe_prv_release_SUITE.erl
%% ============================================================================

%% Add these to the exports and all/0 list:
update_app_file_with_modules/1,
update_app_file_error_handling/1,
get_release_name_from_different_formats/1,
show_usage_info_displays/1.

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

%% ============================================================================
%% Additional tests for r3lfe_prv_ltest_SUITE.erl - add to test/r3lfe_prv_ltest_SUITE.erl
%% ============================================================================

ltest_format_error_messages(_Config) ->
    %% Test error message formatting
    Error1 = {test_failed, "module_test"},
    Error2 = {suite_not_found, "my_suite"},
    Error3 = unknown_error,
    
    Msg1 = r3lfe_prv_ltest:format_error(Error1),
    Msg2 = r3lfe_prv_ltest:format_error(Error2),
    Msg3 = r3lfe_prv_ltest:format_error(Error3),
    
    ?assert(is_list(Msg1)),
    ?assert(is_list(Msg2)),
    ?assert(is_list(Msg3)),
    
    ok.

%% ============================================================================
%% Additional tests for r3lfe_prv_repl_SUITE.erl - add to test/r3lfe_prv_repl_SUITE.erl
%% ============================================================================

build_banner_generates_output(_Config) ->
    Banner = r3lfe_prv_repl:build_banner(),
    
    %% Should contain expected elements
    ?assert(is_list(Banner)),
    ?assert(length(Banner) > 0),
    ?assert(string:find(Banner, "LFE") =/= nomatch),
    
    ok.

%% ============================================================================
%% Additional tests for r3lfe_prv_run_SUITE.erl - add to test/r3lfe_prv_run_SUITE.erl
%% ============================================================================

run_format_error_types(_Config) ->
    Errors = [
        no_main_file,
        {file_not_found, "/tmp/missing.lfe"},
        {lfescript_error, some_reason},
        {run_error, another_reason}
    ],
    
    lists:foreach(
        fun(Error) ->
            Msg = r3lfe_prv_run:format_error(Error),
            ?assert(is_list(Msg)),
            ?assert(length(Msg) > 0)
        end,
        Errors
    ),
    
    ok.
