-module(rb3lfe_compile_worker_SUITE).

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
    compile_simple_module/1,
    compile_with_errors/1,
    format_errors_standard/1,
    format_warnings_standard/1,
    build_compiler_opts/1,
    relative_path_formatting/1,
    compile_opts_change_detection/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_simple_module,
        compile_with_errors,
        format_errors_standard,
        format_warnings_standard,
        build_compiler_opts,
        relative_path_formatting,
        compile_opts_change_detection
    ].

init_per_suite(Config) ->
    %% Ensure LFE is available
    application:ensure_all_started(lfe),
    rb3lfe_compile_opts:init(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    rb3lfe_compile_opts:clear_opts_cache(),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

compile_simple_module(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create source file
    SourceFile = filename:join(TestDir, "simple.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule simple)\n"
        "(defun hello () 'world)\n"),

    %% Create output directory
    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    %% Compile
    Result = rb3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% Should succeed
    ?assertMatch(ok, Result),

    %% Check beam file exists
    BeamFile = filename:join(OutDir, "simple.beam"),
    ?assert(filelib:is_file(BeamFile)),

    ok.

compile_with_errors(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create source with syntax error
    SourceFile = filename:join(TestDir, "errors.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule errors)\n"
        "(defun bad (  )\n"),  % Syntax error: incomplete

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Result = rb3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% Should fail with errors
    ?assertMatch({error, _Errors, _Warnings}, Result),

    {error, Errors, _Warnings} = Result,
    ?assert(length(Errors) > 0),

    ok.

format_errors_standard(_Config) ->
    %% Test standard error format
    Errors = [
        {"file.lfe", [{10, erl_parse, "syntax error"}]}
    ],

    Formatted = rb3lfe_compile_worker:format_errors(Errors),

    ?assertEqual(1, length(Formatted)),
    ?assertMatch([{"file.lfe", [{10, erl_parse, "syntax error"}]}], Formatted),

    ok.

format_warnings_standard(_Config) ->
    %% Test standard warning format
    Warnings = [
        {"file.lfe", [{15, erl_lint, {unused_var, 'X'}}]}
    ],

    Formatted = rb3lfe_compile_worker:format_warnings(Warnings),

    ?assertEqual(1, length(Formatted)),

    ok.

build_compiler_opts(_Config) ->
    Source = "/path/to/module.lfe",
    OutDir = "/path/to/ebin",
    BaseOpts = [verbose, debug_info],

    Opts = rb3lfe_compile_worker:build_compiler_opts(Source, OutDir, BaseOpts),

    %% Should contain required opts
    ?assert(lists:member(return, Opts)),
    ?assert(lists:member(report_errors, Opts)),
    ?assert(lists:keymember(outdir, 1, Opts)),

    %% Should contain base opts
    ?assert(lists:member(verbose, Opts)),
    ?assert(lists:member(debug_info, Opts)),

    ok.

relative_path_formatting(_Config) ->
    %% This tests path simplification for cleaner output
    {ok, Cwd} = file:get_cwd(),

    AbsPath = filename:join(Cwd, "src/module.lfe"),
    RelPath = rb3lfe_compile_worker:relative_path(AbsPath),

    %% Should be relative
    ?assertEqual("src/module.lfe", RelPath),

    ok.

compile_opts_change_detection(_Config) ->
    TestFile = "/path/to/test.lfe",
    Opts1 = [verbose],
    Opts2 = [verbose, debug_info],

    %% First check - no cached opts
    ?assert(rb3lfe_compile_opts:opts_changed(TestFile, Opts1)),

    %% Save options
    ok = rb3lfe_compile_opts:save_opts_hash(TestFile, Opts1),

    %% Get hash to verify save worked
    Hash1 = rb3lfe_compile_opts:get_opts_hash(Opts1),
    ?assert(is_binary(Hash1)),

    %% Different options should produce different hash
    Hash2 = rb3lfe_compile_opts:get_opts_hash(Opts2),
    ?assertNotEqual(Hash1, Hash2),

    ok.
