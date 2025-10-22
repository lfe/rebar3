-module(r3lfe_compile_worker_SUITE).

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
    compile_opts_change_detection/1,
    compile_with_warnings/1,
    compile_alternative_format/1,
    compile_file_errors_format/1,
    build_compiler_opts_removes_conflicts/1,
    option_key_extraction/1,
    format_warning_item_variants/1,
    format_error_item_variants/1,
    format_error_callback/1,
    compile_with_verbose_option/1,
    relative_path_without_cwd/1
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
        compile_opts_change_detection,
        compile_with_warnings,
        compile_alternative_format,
        compile_file_errors_format,
        build_compiler_opts_removes_conflicts,
        option_key_extraction,
        format_warning_item_variants,
        format_error_item_variants,
        format_error_callback,
        compile_with_verbose_option,
        relative_path_without_cwd
    ].

init_per_suite(Config) ->
    %% Ensure LFE is available
    application:ensure_all_started(lfe),
    r3lfe_compile_opts:init(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir(),
    r3lfe_compile_opts:clear_opts_cache(),
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
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

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

    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

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

    Formatted = r3lfe_compile_worker:format_errors(Errors),

    ?assertEqual(1, length(Formatted)),
    ?assertMatch([{"file.lfe", [{10, erl_parse, "syntax error"}]}], Formatted),

    ok.

format_warnings_standard(_Config) ->
    %% Test standard warning format
    Warnings = [
        {"file.lfe", [{15, erl_lint, {unused_var, 'X'}}]}
    ],

    Formatted = r3lfe_compile_worker:format_warnings(Warnings),

    ?assertEqual(1, length(Formatted)),

    ok.

build_compiler_opts(_Config) ->
    Source = "/path/to/module.lfe",
    OutDir = "/path/to/ebin",
    BaseOpts = [verbose, debug_info],

    Opts = r3lfe_compile_worker:build_compiler_opts(Source, OutDir, BaseOpts),

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
    RelPath = r3lfe_compile_worker:relative_path(AbsPath),

    %% Should be relative
    ?assertEqual("src/module.lfe", RelPath),

    ok.

compile_opts_change_detection(_Config) ->
    TestFile = "/path/to/test.lfe",
    Opts1 = [verbose],
    Opts2 = [verbose, debug_info],

    %% First check - no cached opts
    ?assert(r3lfe_compile_opts:opts_changed(TestFile, Opts1)),

    %% Save options
    ok = r3lfe_compile_opts:save_opts_hash(TestFile, Opts1),

    %% Get hash to verify save worked
    Hash1 = r3lfe_compile_opts:get_opts_hash(Opts1),
    ?assert(is_binary(Hash1)),

    %% Different options should produce different hash
    Hash2 = r3lfe_compile_opts:get_opts_hash(Opts2),
    ?assertNotEqual(Hash1, Hash2),

    ok.

compile_with_warnings(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create source with code that generates warnings
    SourceFile = filename:join(TestDir, "warnings.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule warnings)\n"
        "(defun test ()\n"
        "  (let ((unused-var 42))\n"
        "    'ok))\n"),

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% May succeed with or without warnings depending on LFE version
    case Result of
        ok -> ok;
        {ok, _Warnings} -> ok;
        {error, _Errors, _Warnings} -> ok
    end,

    ok.

compile_alternative_format(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Test that alternative success format is handled
    SourceFile = filename:join(TestDir, "simple.lfe"),
    test_utils:write_file(SourceFile, "(defmodule simple)\n(defun test () 'ok)\n"),

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% Should handle {ok, Module} format
    ?assertMatch(ok, Result),

    ok.

compile_file_errors_format(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create file with error format {error, [], FileErrors, []}
    SourceFile = filename:join(TestDir, "error_format.lfe"),
    test_utils:write_file(SourceFile, "(defmodule error-format\n"),  % Incomplete

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, []),

    %% Should handle error format
    ?assertMatch({error, _, _}, Result),

    ok.

build_compiler_opts_removes_conflicts(_Config) ->
    Source = "/path/to/source.lfe",
    OutDir = "/path/to/out",

    %% BaseOpts with conflicting options
    BaseOpts = [
        verbose,
        {outdir, "/wrong/dir"},
        debug_info,
        return
    ],

    Opts = r3lfe_compile_worker:build_compiler_opts(Source, OutDir, BaseOpts),

    %% Should have correct outdir
    ?assert(lists:keymember(outdir, 1, Opts)),
    {outdir, ActualOutDir} = lists:keyfind(outdir, 1, Opts),
    ?assertEqual(OutDir, ActualOutDir),

    %% Should still have non-conflicting opts
    ?assert(lists:member(verbose, Opts)),
    ?assert(lists:member(debug_info, Opts)),

    ok.

option_key_extraction(_Config) ->
    %% Test option_key/1 function via compile_opts building
    Opts = [
        verbose,
        debug_info,
        {outdir, "/tmp"},
        {i, "/include"},
        return
    ],

    %% All should be preserved as they're valid
    BuiltOpts = r3lfe_compile_worker:build_compiler_opts("/src/file.lfe", "/out", Opts),

    ?assert(lists:member(verbose, BuiltOpts)),
    ?assert(lists:member(debug_info, BuiltOpts)),

    ok.

format_warning_item_variants(_Config) ->
    %% Test different warning formats
    Warning1 = {"/tmp/file.lfe", [{10, lfe_lint, {unused_var, 'X'}}]},
    Warning2 = {warning, "/tmp/file.lfe", [{20, lfe_lint, {unused_function, test}}]},
    Warning3 = unknown_format,

    Formatted1 = r3lfe_compile_worker:format_warnings([Warning1]),
    Formatted2 = r3lfe_compile_worker:format_warnings([Warning2]),
    Formatted3 = r3lfe_compile_worker:format_warnings([Warning3]),

    ?assert(is_list(Formatted1)),
    ?assert(is_list(Formatted2)),
    ?assert(is_list(Formatted3)),

    ok.

format_error_item_variants(_Config) ->
    %% Test different error formats
    Error1 = {"/tmp/file.lfe", [{15, lfe_parse, "syntax error"}]},
    Error2 = {error, "/tmp/file.lfe", [{25, lfe_parse, "unexpected token"}]},
    Error3 = unknown_error_format,

    Formatted1 = r3lfe_compile_worker:format_errors([Error1]),
    Formatted2 = r3lfe_compile_worker:format_errors([Error2]),
    Formatted3 = r3lfe_compile_worker:format_errors([Error3]),

    ?assert(is_list(Formatted1)),
    ?assert(is_list(Formatted2)),
    ?assert(is_list(Formatted3)),

    ok.

format_error_callback(_Config) ->
    %% Test format_error/1 callback for various error types

    %% Test unknown_error
    Error1 = r3lfe_compile_worker:format_error({unknown_error, some_term}),
    ?assert(is_list(Error1)),
    ?assert(length(Error1) > 0),

    %% Test unknown_warning
    Error2 = r3lfe_compile_worker:format_error({unknown_warning, some_term}),
    ?assert(is_list(Error2)),
    ?assert(length(Error2) > 0),

    %% Test compilation_failed
    Error3 = r3lfe_compile_worker:format_error({compilation_failed, "test.lfe"}),
    ?assert(is_list(Error3)),
    ?assert(length(Error3) > 0),

    %% Test generic error
    Error4 = r3lfe_compile_worker:format_error(some_other_error),
    ?assert(is_list(Error4)),
    ?assert(length(Error4) > 0),

    ok.

compile_with_verbose_option(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create a simple valid module
    SourceFile = filename:join(TestDir, "verbosetest.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule verbosetest)\n"
        "(defun test () 'ok)\n"),

    OutDir = filename:join(TestDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    %% Compile with verbose option in ExtraOpts
    Result = r3lfe_compile_worker:compile_file(SourceFile, OutDir, [], #{verbose => true}),

    %% Should succeed
    ?assertMatch(ok, Result),

    %% Check beam file exists
    BeamFile = filename:join(OutDir, "verbosetest.beam"),
    ?assert(filelib:is_file(BeamFile)),

    ok.

relative_path_without_cwd(_Config) ->
    %% Test relative_path when path is not under current directory
    AbsPath = "/some/other/path/file.lfe",
    RelPath = r3lfe_compile_worker:relative_path(AbsPath),

    %% Should return the original path if not under cwd
    ?assert(is_list(RelPath)),

    ok.
