-module(r3lfe_prv_run_SUITE).

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
    run_provider_registers/1,
    run_no_main_error/1,
    run_format_error_types/1,
    find_main_from_options_main/1,
    find_main_from_options_script/1,
    find_main_from_options_none/1,
    find_main_from_config_present/1,
    find_main_from_config_absent/1,
    validate_main_file_exists/1,
    validate_main_file_missing/1,
    validate_main_file_undefined/1,
    parse_args_no_separator/1,
    parse_args_with_separator/1,
    parse_args_multiple_args/1,
    find_main_file_from_options/1,
    find_main_file_from_config/1,
    find_main_file_undefined/1,
    info_output/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        run_provider_registers,
        run_no_main_error,
        run_format_error_types,
        find_main_from_options_main,
        find_main_from_options_script,
        find_main_from_options_none,
        find_main_from_config_present,
        find_main_from_config_absent,
        validate_main_file_exists,
        validate_main_file_missing,
        validate_main_file_undefined,
        parse_args_no_separator,
        parse_args_with_separator,
        parse_args_multiple_args,
        find_main_file_from_options,
        find_main_file_from_config,
        find_main_file_undefined,
        info_output
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("run"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

run_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_run:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Run provider should be registered"),

    ok.

run_no_main_error(_Config) ->
    %% No main specified
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[], []}),

    Result = r3lfe_prv_run:do(State1),

    ?assertMatch({error, _}, Result),

    ok.

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

%%====================================================================
%% Test find_main_from_options/1
%%====================================================================

find_main_from_options_main(_Config) ->
    %% Test --main option
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{main, "scripts/run.lfe"}], []}),

    Result = r3lfe_prv_run:find_main_from_options(State1),

    ?assertEqual("scripts/run.lfe", Result),

    ok.

find_main_from_options_script(_Config) ->
    %% Test --script option (fallback)
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{script, "scripts/script.lfe"}], []}),

    Result = r3lfe_prv_run:find_main_from_options(State1),

    ?assertEqual("scripts/script.lfe", Result),

    ok.

find_main_from_options_none(_Config) ->
    %% Test no options
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[], []}),

    Result = r3lfe_prv_run:find_main_from_options(State1),

    ?assertEqual(undefined, Result),

    ok.

%%====================================================================
%% Test find_main_from_config/1
%%====================================================================

find_main_from_config_present(_Config) ->
    %% Test with lfe config
    State = rebar_state:new(),
    State1 = rebar_state:set(State, lfe, [{main, "config/main.lfe"}]),

    Result = r3lfe_prv_run:find_main_from_config(State1),

    ?assertEqual("config/main.lfe", Result),

    ok.

find_main_from_config_absent(_Config) ->
    %% Test without lfe config
    State = rebar_state:new(),

    Result = r3lfe_prv_run:find_main_from_config(State),

    ?assertEqual(undefined, Result),

    ok.

%%====================================================================
%% Test validate_main_file/1
%%====================================================================

validate_main_file_exists(Config) ->
    %% Create a test file
    TestDir = ?config(test_dir, Config),
    TestFile = filename:join(TestDir, "test.lfe"),
    ok = file:write_file(TestFile, "(defun main (args) 'ok)"),

    Result = r3lfe_prv_run:validate_main_file(TestFile),

    %% Should return absolute path
    ?assertEqual(filename:absname(TestFile), Result),

    ok.

validate_main_file_missing(_Config) ->
    %% Test with missing file
    MissingFile = "/tmp/nonexistent_" ++ integer_to_list(erlang:system_time()) ++ ".lfe",

    ?assertThrow(
        {error, {file_not_found, MissingFile}},
        r3lfe_prv_run:validate_main_file(MissingFile)
    ),

    ok.

validate_main_file_undefined(_Config) ->
    %% Test with undefined
    Result = r3lfe_prv_run:validate_main_file(undefined),

    ?assertEqual(undefined, Result),

    ok.

%%====================================================================
%% Test parse_args/1
%%====================================================================

parse_args_no_separator(_Config) ->
    %% Test without -- separator
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, ["other", "args"]),

    Result = r3lfe_prv_run:parse_args(State1),

    ?assertEqual([], Result),

    ok.

parse_args_with_separator(_Config) ->
    %% Test with -- separator
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, ["--", "arg1", "arg2"]),

    Result = r3lfe_prv_run:parse_args(State1),

    ?assertEqual(["arg1", "arg2"], Result),

    ok.

parse_args_multiple_args(_Config) ->
    %% Test with multiple args after --
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, ["cmd", "--", "a", "b", "c", "d"]),

    Result = r3lfe_prv_run:parse_args(State1),

    ?assertEqual(["a", "b", "c", "d"], Result),

    ok.

%%====================================================================
%% Test find_main_file/1
%%====================================================================

find_main_file_from_options(Config) ->
    %% Test priority: options over config
    TestDir = ?config(test_dir, Config),
    TestFile = filename:join(TestDir, "opt.lfe"),
    ok = file:write_file(TestFile, "(defun main (args) 'ok)"),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{main, TestFile}], []}),
    State2 = rebar_state:set(State1, lfe, [{main, "config.lfe"}]),

    Result = r3lfe_prv_run:find_main_file(State2),

    %% Should use option, not config
    ?assertEqual(filename:absname(TestFile), Result),

    ok.

find_main_file_from_config(Config) ->
    %% Test fallback to config
    TestDir = ?config(test_dir, Config),
    TestFile = filename:join(TestDir, "cfg.lfe"),
    ok = file:write_file(TestFile, "(defun main (args) 'ok)"),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[], []}),
    State2 = rebar_state:set(State1, lfe, [{main, TestFile}]),

    Result = r3lfe_prv_run:find_main_file(State2),

    ?assertEqual(filename:absname(TestFile), Result),

    ok.

find_main_file_undefined(_Config) ->
    %% Test with no options or config
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[], []}),

    Result = r3lfe_prv_run:find_main_file(State1),

    ?assertEqual(undefined, Result),

    ok.

%%====================================================================
%% Test info/1
%%====================================================================

info_output(_Config) ->
    %% Test info function
    Result = r3lfe_prv_run:info("Run LFE script"),

    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),

    %% Should contain key information
    Flat = lists:flatten(Result),
    ?assert(string:str(Flat, "main") > 0),
    ?assert(string:str(Flat, "script") > 0),

    ok.
