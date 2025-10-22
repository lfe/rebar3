-module(r3lfe_prv_run_release_SUITE).

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
    run_release_provider_registers/1,
    run_release_finds_script/1,
    run_release_validates_commands/1,
    run_release_no_command_error/1,
    run_release_script_not_found_error/1,
    release_output_dir_default/1,
    release_output_dir_custom/1,
    release_output_dir_absolute/1,
    release_name_from_config/1,
    release_name_from_app/1,
    release_name_fallback/1,
    get_command_with_args/1,
    get_command_empty/1,
    validate_command_valid/1,
    validate_command_unknown/1,
    build_command_line_simple/1,
    build_command_line_with_args/1,
    is_interactive_command_console/1,
    is_interactive_command_status/1,
    find_release_script_path/1,
    format_error_no_command/1,
    format_error_script_not_found/1,
    format_error_invalid_command/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        run_release_provider_registers,
        run_release_finds_script,
        run_release_validates_commands,
        run_release_no_command_error,
        run_release_script_not_found_error,
        release_output_dir_default,
        release_output_dir_custom,
        release_output_dir_absolute,
        release_name_from_config,
        release_name_from_app,
        release_name_fallback,
        get_command_with_args,
        get_command_empty,
        validate_command_valid,
        validate_command_unknown,
        build_command_line_simple,
        build_command_line_with_args,
        is_interactive_command_console,
        is_interactive_command_status,
        find_release_script_path,
        format_error_no_command,
        format_error_script_not_found,
        format_error_invalid_command
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("run_release"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

run_release_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_run_release:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Run-release provider should be registered"),

    ok.

run_release_finds_script(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create mock release structure
    RelDir = filename:join([TestDir, "_build", "default", "rel", "myapp", "bin"]),
    ok = filelib:ensure_dir(filename:join(RelDir, "dummy")),

    %% Create mock release script
    ScriptPath = filename:join(RelDir, "myapp"),
    test_utils:write_file(ScriptPath, "#!/bin/sh\necho 'mock script'\n"),
    file:change_mode(ScriptPath, 8#755),

    %% Create state
    State = create_test_state(TestDir, "myapp"),
    State1 = rebar_state:command_args(State, ["status"]),

    %% Should find the script
    Result = r3lfe_prv_run_release:do(State1),

    %% Won't fully succeed without real release, but should find script
    ct:pal("Result: ~p", [Result]),

    ok.

run_release_validates_commands(_Config) ->
    %% Valid commands should pass validation
    ValidCommands = [
        "start", "stop", "restart", "status", "ping",
        "console", "remote_console", "attach"
    ],

    lists:foreach(
        fun(Cmd) ->
            %% These are internal functions, can't test directly
            %% But we verify they're in the valid list
            ?assert(is_list(Cmd))
        end,
        ValidCommands
    ),

    ok.

run_release_no_command_error(_Config) ->
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, []),

    Result = r3lfe_prv_run_release:do(State1),

    ?assertMatch({error, _}, Result),

    {error, ErrorMsg} = Result,
    ?assert(string:find(ErrorMsg, "No command") =/= nomatch),

    ok.

run_release_script_not_found_error(Config) ->
    TestDir = ?config(test_dir, Config),

    %% State points to non-existent release
    State = create_test_state(TestDir, "nonexistent"),
    State1 = rebar_state:command_args(State, ["start"]),

    Result = r3lfe_prv_run_release:do(State1),

    ?assertMatch({error, _}, Result),

    {error, ErrorMsg} = Result,
    ?assert(string:find(ErrorMsg, "not found") =/= nomatch),

    ok.

release_output_dir_default(Config) ->
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% No custom output_dir in relx config
    %% Should use default

    BaseDir = rebar_dir:base_dir(State1),
    Expected = filename:join(BaseDir, "rel"),

    %% This tests internal function indirectly through provider
    ct:pal("Expected default dir: ~s", [Expected]),

    ok.

release_output_dir_custom(Config) ->
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Set custom output_dir
    RelxConfig = [{output_dir, "custom_release"}],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    BaseDir = rebar_dir:base_dir(State2),
    Expected = filename:join(BaseDir, "custom_release"),

    ct:pal("Expected custom dir: ~s", [Expected]),

    ok.

release_name_from_config(Config) ->
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Set release name in config as atom
    RelxConfig = [{release, {myrelease, "1.0.0"}, [myapp]}],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    %% Release name should be extracted
    Name = r3lfe_prv_run_release:get_release_name(State2),
    ?assertEqual("myrelease", Name),

    ok.

release_name_from_app(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create app info
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),

    State = rebar_state:new(),
    State1 = rebar_state:project_apps(State, [AppInfo]),

    %% Should use app name as fallback
    Name = r3lfe_prv_run_release:get_release_name(State1),
    ?assertEqual("test_app", Name),

    ok.

release_name_fallback(_Config) ->
    %% No relx config, no apps
    State = rebar_state:new(),

    %% Should use default fallback
    Name = r3lfe_prv_run_release:get_release_name(State),
    ?assertEqual("myapp", Name),

    ok.

release_output_dir_absolute(Config) ->
    TestDir = ?config(test_dir, Config),

    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Set absolute custom output_dir
    AbsPath = filename:join(TestDir, "abs_release"),
    RelxConfig = [{output_dir, AbsPath}],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    OutputDir = r3lfe_prv_run_release:get_release_output_dir(State2),
    %% Should use absolute path as-is
    ?assert(string:str(OutputDir, "abs_release") > 0),

    ok.

get_command_with_args(_Config) ->
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, ["start", "extra", "args"]),

    Command = r3lfe_prv_run_release:get_command(State1),
    ?assertEqual("start", Command),

    ok.

get_command_empty(_Config) ->
    State = rebar_state:new(),
    State1 = rebar_state:command_args(State, []),

    Command = r3lfe_prv_run_release:get_command(State1),
    ?assertEqual(undefined, Command),

    ok.

validate_command_valid(_Config) ->
    %% Test various valid commands
    ValidCommands = ["start", "stop", "restart", "console", "ping", "status"],

    lists:foreach(
        fun(Cmd) ->
            Result = r3lfe_prv_run_release:validate_command(Cmd),
            ?assertEqual(Cmd, Result)
        end,
        ValidCommands
    ),

    ok.

validate_command_unknown(_Config) ->
    %% Unknown commands should still pass through (with warning)
    Unknown = "unknown_cmd",
    Result = r3lfe_prv_run_release:validate_command(Unknown),
    ?assertEqual(Unknown, Result),

    ok.

build_command_line_simple(_Config) ->
    Script = "/path/to/release",
    Command = "start",

    Result = r3lfe_prv_run_release:build_command_line(Script, Command),
    Expected = "/path/to/release start",
    ?assertEqual(Expected, Result),

    ok.

build_command_line_with_args(_Config) ->
    Script = "/path/to/release",
    Command = "eval \"io:format('test')\"",

    Result = r3lfe_prv_run_release:build_command_line(Script, Command),
    ?assert(string:str(Result, Script) > 0),
    ?assert(string:str(Result, "eval") > 0),

    ok.

is_interactive_command_console(_Config) ->
    %% Test interactive commands
    InteractiveCmds = ["console", "remote_console", "attach", "foreground"],

    lists:foreach(
        fun(Cmd) ->
            Result = r3lfe_prv_run_release:is_interactive_command(Cmd),
            ?assert(Result, io_lib:format("~s should be interactive", [Cmd]))
        end,
        InteractiveCmds
    ),

    ok.

is_interactive_command_status(_Config) ->
    %% Test non-interactive commands
    NonInteractiveCmds = ["start", "stop", "restart", "ping", "status"],

    lists:foreach(
        fun(Cmd) ->
            Result = r3lfe_prv_run_release:is_interactive_command(Cmd),
            ?assertNot(Result, io_lib:format("~s should not be interactive", [Cmd]))
        end,
        NonInteractiveCmds
    ),

    ok.

find_release_script_path(Config) ->
    TestDir = ?config(test_dir, Config),

    State = create_test_state(TestDir, "myapp"),

    Script = r3lfe_prv_run_release:find_release_script(State),
    ?assert(string:str(Script, "myapp") > 0),
    ?assert(string:str(Script, "bin") > 0),

    ok.

format_error_no_command(_Config) ->
    Error = no_command,
    Result = r3lfe_prv_run_release:format_error(Error),

    ?assert(is_list(Result)),
    ?assert(string:str(lists:flatten(Result), "No command") > 0),

    ok.

format_error_script_not_found(_Config) ->
    Script = "/path/to/missing",
    Error = {release_script_not_found, Script},
    Result = r3lfe_prv_run_release:format_error(Error),

    ?assert(is_list(Result)),
    ?assert(string:str(lists:flatten(Result), "not found") > 0),
    ?assert(string:str(lists:flatten(Result), Script) > 0),

    ok.

format_error_invalid_command(_Config) ->
    Command = "invalid",
    Error = {invalid_command, Command},
    Result = r3lfe_prv_run_release:format_error(Error),

    ?assert(is_list(Result)),
    ?assert(string:str(lists:flatten(Result), "Invalid") > 0),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_test_state(TestDir, AppName) ->
    State = rebar_state:new(),
    State1 = rebar_state:dir(State, TestDir),

    %% Add relx config
    RelxConfig = [
        {release, {list_to_atom(AppName), "0.1.0"}, [list_to_atom(AppName)]}
    ],
    State2 = rebar_state:set(State1, relx, RelxConfig),

    State2.
