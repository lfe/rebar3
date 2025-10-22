-module(r3lfe_prv_run_escript_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

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
    run_escript_provider_registers/1,
    find_escript_from_state/1,
    find_escript_from_config_atom/1,
    find_escript_from_config_string/1,
    find_escript_from_app_name/1,
    find_escript_fallback/1,
    parse_args_empty/1,
    parse_args_with_separator/1,
    parse_args_multiple_args/1,
    build_command_no_args/1,
    build_command_with_args/1,
    determine_escript_path_from_config/1,
    format_error_escript_not_found/1,
    format_error_run_error/1,
    format_error_generic/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        run_escript_provider_registers,
        find_escript_from_state,
        find_escript_from_config_atom,
        find_escript_from_config_string,
        find_escript_from_app_name,
        find_escript_fallback,
        parse_args_empty,
        parse_args_with_separator,
        parse_args_multiple_args,
        build_command_no_args,
        build_command_with_args,
        determine_escript_path_from_config,
        format_error_escript_not_found,
        format_error_run_error,
        format_error_generic
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("run_escript"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

run_escript_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_run_escript:init(State),

    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),
    ok.

find_escript_from_state(_Config) ->
    %% Test when escript_path is set in state
    EscriptPath = "/path/to/my_escript",
    State = rebar_state:new(),
    State1 = rebar_state:escript_path(State, EscriptPath),

    Result = r3lfe_prv_run_escript:find_escript(State1),
    ?assertEqual(EscriptPath, Result),
    ok.

find_escript_from_config_atom(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create state with escript_name as atom
    BuildDir = filename:join(TestDir, "_build"),
    State = rebar_state:new(),
    State1 = rebar_state:set(State, escript_name, my_script),
    State2 = rebar_state:set(State1, base_dir, BuildDir),

    Result = r3lfe_prv_run_escript:determine_escript_path(State2),
    %% rebar_dir:base_dir appends the current profile (default)
    ?assert(string:str(Result, "my_script") > 0),
    ok.

find_escript_from_config_string(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create state with escript_name as string
    BuildDir = filename:join(TestDir, "_build"),
    State = rebar_state:new(),
    State1 = rebar_state:set(State, escript_name, "my_script"),
    State2 = rebar_state:set(State1, base_dir, BuildDir),

    Result = r3lfe_prv_run_escript:determine_escript_path(State2),
    %% rebar_dir:base_dir appends the current profile (default)
    ?assert(string:str(Result, "my_script") > 0),
    ok.

find_escript_from_app_name(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create app info
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),

    %% Create state with project apps
    BuildDir = filename:join(TestDir, "_build"),
    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo]),
    State2 = rebar_state:set(State1, base_dir, BuildDir),

    Result = r3lfe_prv_run_escript:determine_escript_path(State2),
    %% Should contain the app name
    ?assert(string:str(Result, "test_app") > 0),
    ok.

find_escript_fallback(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create state with no config and no apps
    BuildDir = filename:join(TestDir, "_build"),
    State = rebar_state:new(),
    State1 = rebar_state:set(State, base_dir, BuildDir),

    Result = r3lfe_prv_run_escript:determine_escript_path(State1),
    %% Should contain the fallback name "escript"
    ?assert(string:str(Result, "escript") > 0),
    ok.

parse_args_empty(_Config) ->
    %% Test with no arguments
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, []),

    Result = r3lfe_prv_run_escript:parse_args(State1),
    ?assertEqual([], Result),
    ok.

parse_args_with_separator(_Config) ->
    %% Test with -- separator but no args
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, ["--"]),

    Result = r3lfe_prv_run_escript:parse_args(State1),
    ?assertEqual([], Result),
    ok.

parse_args_multiple_args(_Config) ->
    %% Test with -- separator and multiple args
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, ["lfe", "run-escript", "--", "arg1", "arg2", "--verbose"]),

    Result = r3lfe_prv_run_escript:parse_args(State1),
    ?assertEqual(["arg1", "arg2", "--verbose"], Result),
    ok.

build_command_no_args(Config) ->
    TestDir = ?config(test_dir, Config),
    EscriptPath = filename:join(TestDir, "my_escript"),

    %% Create a dummy escript file
    ok = file:write_file(EscriptPath, <<"#!/usr/bin/env escript\nmain(_) -> ok.">>),

    Result = r3lfe_prv_run_escript:build_command(EscriptPath, []),
    ?assertEqual(EscriptPath, Result),

    %% Verify file is executable
    {ok, FileInfo} = file:read_file_info(EscriptPath),
    Mode = FileInfo#file_info.mode,
    ?assert((Mode band 8#111) > 0, "File should be executable"),
    ok.

build_command_with_args(Config) ->
    TestDir = ?config(test_dir, Config),
    EscriptPath = filename:join(TestDir, "my_escript"),

    %% Create a dummy escript file
    ok = file:write_file(EscriptPath, <<"#!/usr/bin/env escript\nmain(_) -> ok.">>),

    Args = ["arg1", "arg2", "--verbose"],
    Result = r3lfe_prv_run_escript:build_command(EscriptPath, Args),
    Expected = string:join([EscriptPath | Args], " "),
    ?assertEqual(Expected, Result),
    ok.

determine_escript_path_from_config(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create state with multiple apps
    {ok, AppInfo1} = rebar_app_info:new(app1, "0.1.0", TestDir),
    {ok, AppInfo2} = rebar_app_info:new(app2, "0.1.0", TestDir),

    BuildDir = filename:join(TestDir, "_build"),
    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo1, AppInfo2]),
    State2 = rebar_state:set(State1, base_dir, BuildDir),

    %% Should use first app's name
    Result = r3lfe_prv_run_escript:determine_escript_path(State2),
    ?assert(string:str(Result, "app1") > 0),
    ok.

format_error_escript_not_found(_Config) ->
    Path = "/path/to/missing_escript",
    Error = {escript_not_found, Path},

    Result = r3lfe_prv_run_escript:format_error(Error),
    ?assert(is_list(Result)),
    ?assert(string:str(lists:flatten(Result), "not found") > 0),
    ?assert(string:str(lists:flatten(Result), Path) > 0),
    ok.

format_error_run_error(_Config) ->
    Reason = {some_error, "details"},
    Error = {run_error, Reason},

    Result = r3lfe_prv_run_escript:format_error(Error),
    ?assert(is_list(Result)),
    ?assert(string:str(lists:flatten(Result), "execution failed") > 0),
    ok.

format_error_generic(_Config) ->
    Error = unknown_error,

    Result = r3lfe_prv_run_escript:format_error(Error),
    ?assert(is_list(Result)),
    ok.
