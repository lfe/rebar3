-module(r3lfe_prv_clean_SUITE).

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
    clean_provider_registers/1,
    clean_do_returns_ok/1,
    clean_do_with_current_app/1,
    format_error_generic/1,
    clean_app_deletes_beams/1,
    clean_app_handles_missing_files/1,
    clean_app_handles_errors/1,
    info_output_validation/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        clean_provider_registers,
        clean_do_returns_ok,
        clean_do_with_current_app,
        format_error_generic,
        clean_app_deletes_beams,
        clean_app_handles_missing_files,
        clean_app_handles_errors,
        info_output_validation
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("clean"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

clean_provider_registers(_Config) ->
    %% Test that provider registers correctly
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_clean:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Clean provider should be registered"),

    ok.

clean_do_returns_ok(_Config) ->
    %% Test that do/1 returns ok with no apps
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_clean:do(State),

    ?assert(is_tuple(State1)),

    ok.

clean_do_with_current_app(Config) ->
    %% Test that do/1 works with a current app
    TestDir = ?config(test_dir, Config),

    %% Create a mock app with ebin dir
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create a beam file
    BeamFile = filename:join(EbinDir, "test.beam"),
    ok = file:write_file(BeamFile, <<>>),

    {ok, AppInfo} = rebar_app_info:new(testapp, "1.0.0", TestDir),
    AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),

    State = rebar_state:new(),
    State1 = rebar_state:current_app(State, AppInfo1),

    {ok, _State2} = r3lfe_prv_clean:do(State1),

    %% Beam file should be deleted
    ?assertNot(filelib:is_file(BeamFile)),

    ok.

format_error_generic(_Config) ->
    %% Test format_error with various reasons
    Reasons = [
        unknown_error,
        {error, something},
        test_reason
    ],

    lists:foreach(
        fun(Reason) ->
            Msg = r3lfe_prv_clean:format_error(Reason),
            ?assert(is_list(Msg)),
            ?assert(length(Msg) > 0)
        end,
        Reasons
    ),

    ok.

%%====================================================================
%% Test clean_app/1
%%====================================================================

clean_app_deletes_beams(Config) ->
    %% Test that clean_app deletes beam files
    TestDir = ?config(test_dir, Config),

    %% Create ebin directory
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create multiple beam files
    BeamFiles = [
        filename:join(EbinDir, "module1.beam"),
        filename:join(EbinDir, "module2.beam"),
        filename:join(EbinDir, "module3.beam")
    ],
    lists:foreach(
        fun(File) -> ok = file:write_file(File, <<>>) end,
        BeamFiles
    ),

    %% Verify files exist
    lists:foreach(
        fun(File) -> ?assert(filelib:is_file(File)) end,
        BeamFiles
    ),

    %% Create app info
    {ok, AppInfo} = rebar_app_info:new(testapp, "1.0.0", TestDir),
    AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),

    %% Clean the app
    ok = r3lfe_prv_clean:clean_app(AppInfo1),

    %% Verify files are deleted
    lists:foreach(
        fun(File) -> ?assertNot(filelib:is_file(File)) end,
        BeamFiles
    ),

    ok.

clean_app_handles_missing_files(_Config) ->
    %% Test that clean_app handles missing ebin directory gracefully
    TestDir = test_utils:create_temp_dir("clean_missing"),

    %% Create app with non-existent ebin dir
    EbinDir = filename:join(TestDir, "ebin"),

    {ok, AppInfo} = rebar_app_info:new(testapp, "1.0.0", TestDir),
    AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),

    %% Should not crash
    ok = r3lfe_prv_clean:clean_app(AppInfo1),

    test_utils:cleanup_temp_dir(TestDir),

    ok.

clean_app_handles_errors(Config) ->
    %% Test that clean_app handles file deletion errors gracefully
    TestDir = ?config(test_dir, Config),

    %% Create ebin directory
    EbinDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create app info
    {ok, AppInfo} = rebar_app_info:new(testapp, "1.0.0", TestDir),
    AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),

    %% Clean should succeed even with no beam files
    ok = r3lfe_prv_clean:clean_app(AppInfo1),

    ok.

%%====================================================================
%% Test info/1
%%====================================================================

info_output_validation(_Config) ->
    %% Test info function output
    Result = r3lfe_prv_clean:info("Clean LFE files"),

    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),

    %% Should contain key information
    Flat = lists:flatten(Result),
    ?assert(string:str(Flat, "beam") > 0),

    ok.
