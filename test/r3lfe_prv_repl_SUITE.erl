-module(r3lfe_prv_repl_SUITE).

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
    read_vm_args_valid_file/1,
    read_vm_args_missing_file/1,
    read_vm_args_with_comments/1,
    read_vm_args_empty_lines/1,
    build_shell_args_no_vm_args/1,
    build_shell_args_with_vm_args/1,
    build_shell_args_with_erl_option/1,
    build_shell_args_combined/1,
    build_shell_args_with_custom_module/1,
    merge_repl_opts_cmd_overrides_config/1,
    merge_repl_opts_empty/1,
    build_banner_generates_output/1,
    repl_provider_registers/1,
    format_error_app_start_failed/1,
    format_error_generic/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        read_vm_args_valid_file,
        read_vm_args_missing_file,
        read_vm_args_with_comments,
        read_vm_args_empty_lines,
        build_shell_args_no_vm_args,
        build_shell_args_with_vm_args,
        build_shell_args_with_erl_option,
        build_shell_args_combined,
        build_shell_args_with_custom_module,
        merge_repl_opts_cmd_overrides_config,
        merge_repl_opts_empty,
        build_banner_generates_output,
        repl_provider_registers,
        format_error_app_start_failed,
        format_error_generic
    ].

init_per_suite(Config) ->
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

read_vm_args_valid_file(Config) ->
    TestDir = ?config(test_dir, Config),
    VmArgsPath = filename:join(TestDir, "vm.args"),

    %% Create a simple vm.args file
    Content = "+A 10\n+K true\n",
    ok = file:write_file(VmArgsPath, Content),

    %% Read and parse
    Result = r3lfe_prv_repl:read_vm_args(VmArgsPath),

    %% Should return parsed args as a single string
    ?assertEqual("+A 10 +K true", Result),
    ok.

read_vm_args_missing_file(Config) ->
    TestDir = ?config(test_dir, Config),
    VmArgsPath = filename:join(TestDir, "nonexistent.args"),

    %% Try to read a non-existent file
    Result = r3lfe_prv_repl:read_vm_args(VmArgsPath),

    %% Should return undefined
    ?assertEqual(undefined, Result),
    ok.

read_vm_args_with_comments(Config) ->
    TestDir = ?config(test_dir, Config),
    VmArgsPath = filename:join(TestDir, "vm.args"),

    %% Create vm.args with comments
    Content = "# This is a comment\n+A 10\n# Another comment\n+K true\n",
    ok = file:write_file(VmArgsPath, Content),

    %% Read and parse
    Result = r3lfe_prv_repl:read_vm_args(VmArgsPath),

    %% Comments should be filtered out
    ?assertEqual("+A 10 +K true", Result),
    ok.

read_vm_args_empty_lines(Config) ->
    TestDir = ?config(test_dir, Config),
    VmArgsPath = filename:join(TestDir, "vm.args"),

    %% Create vm.args with empty lines
    Content = "+A 10\n\n\n+K true\n\n",
    ok = file:write_file(VmArgsPath, Content),

    %% Read and parse
    Result = r3lfe_prv_repl:read_vm_args(VmArgsPath),

    %% Empty lines should be filtered out
    ?assertEqual("+A 10 +K true", Result),
    ok.

build_shell_args_no_vm_args(_Config) ->
    %% Build shell args with no VM arguments
    Opts = #{},

    Args = r3lfe_prv_repl:build_shell_args(Opts),

    %% Should have basic shell args but no erl_args
    ?assertMatch([{shell_args, [{lfe_shell, start, []}]}, {nobanner, true}], Args),
    ok.

build_shell_args_with_vm_args(Config) ->
    TestDir = ?config(test_dir, Config),
    VmArgsPath = filename:join(TestDir, "vm.args"),

    %% Create vm.args file
    Content = "+A 10\n+K true\n",
    ok = file:write_file(VmArgsPath, Content),

    %% Build shell args with vm_args option
    Opts = #{vm_args => VmArgsPath},

    Args = r3lfe_prv_repl:build_shell_args(Opts),

    %% Should include erl_args with parsed vm.args
    ?assertMatch([{erl_args, "+A 10 +K true"} | _], Args),
    ok.

build_shell_args_with_erl_option(_Config) ->
    %% Build shell args with erl option
    Opts = #{erl => "-noshell -noinput"},

    Args = r3lfe_prv_repl:build_shell_args(Opts),

    %% Should include erl_args
    ?assertMatch([{erl_args, "-noshell -noinput"} | _], Args),
    ok.

build_shell_args_combined(Config) ->
    TestDir = ?config(test_dir, Config),
    VmArgsPath = filename:join(TestDir, "vm.args"),

    %% Create vm.args file
    Content = "+A 10\n",
    ok = file:write_file(VmArgsPath, Content),

    %% Build shell args with both vm_args and erl options
    Opts = #{vm_args => VmArgsPath, erl => "-noshell"},

    Args = r3lfe_prv_repl:build_shell_args(Opts),

    %% Should combine both
    ?assertMatch([{erl_args, "+A 10 -noshell"} | _], Args),
    ok.

merge_repl_opts_cmd_overrides_config(_Config) ->
    %% Config options
    ConfigOpts = [{apps, "config_app"}, {nobanner, true}],

    %% Command line options (should override)
    CmdOpts = [{apps, "cmd_app"}],

    %% Merge options
    MergedOpts = r3lfe_prv_repl:merge_repl_opts(ConfigOpts, CmdOpts),

    %% Command line should take precedence
    ?assertEqual("cmd_app", maps:get(apps, MergedOpts)),

    %% Config-only options should still be present
    ?assertEqual(true, maps:get(nobanner, MergedOpts)),
    ok.

build_banner_generates_output(_Config) ->
    Banner = r3lfe_prv_repl:build_banner(),

    %% Should contain expected elements
    ?assert(is_list(Banner)),
    ?assert(length(Banner) > 0),
    ?assert(string:find(Banner, "LFE") =/= nomatch),

    ok.

build_shell_args_with_custom_module(_Config) ->
    %% Build shell args with custom start module
    Opts = #{start_module => custom_shell},

    Args = r3lfe_prv_repl:build_shell_args(Opts),

    %% Should use custom module
    ?assertMatch([{shell_args, [{custom_shell, start, []}]}, {nobanner, true}], Args),
    ok.

merge_repl_opts_empty(_Config) ->
    %% Merge with empty config and command opts
    ConfigOpts = [],
    CmdOpts = [],

    MergedOpts = r3lfe_prv_repl:merge_repl_opts(ConfigOpts, CmdOpts),

    %% Should return empty map
    ?assertEqual(#{}, MergedOpts),
    ok.

repl_provider_registers(_Config) ->
    %% Test that provider registers successfully
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_repl:init(State),

    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),

    ok.

format_error_app_start_failed(_Config) ->
    %% Test format_error for app_start_failed
    Error = {app_start_failed, myapp, {not_started, dependency}},

    Result = r3lfe_prv_repl:format_error(Error),

    ?assert(is_list(Result)),
    ?assert(string:str(lists:flatten(Result), "Failed to start") > 0),
    ?assert(string:str(lists:flatten(Result), "myapp") > 0),

    ok.

format_error_generic(_Config) ->
    %% Test format_error for generic error
    Error = some_random_error,

    Result = r3lfe_prv_repl:format_error(Error),

    ?assert(is_list(Result)),

    ok.
