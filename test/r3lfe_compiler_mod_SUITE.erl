-module(r3lfe_compiler_mod_SUITE).

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
    compile_with_no_out_mappings/1,
    compile_with_multiple_mappings/1,
    source_to_target_no_mappings/1,
    source_to_target_with_mapping/1,
    needs_compilation_missing_source/1,
    needs_compilation_source_newer/1,
    needs_compilation_target_newer/1,
    clean_removes_beam_files/1,
    context_returns_map_with_correct_keys/1,
    format_error_returns_string/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_simple_module,
        compile_with_no_out_mappings,
        compile_with_multiple_mappings,
        source_to_target_no_mappings,
        source_to_target_with_mapping,
        needs_compilation_missing_source,
        needs_compilation_source_newer,
        needs_compilation_target_newer,
        clean_removes_beam_files,
        context_returns_map_with_correct_keys,
        format_error_returns_string
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
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

compile_simple_module(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile, "(defmodule simple)\n"),

    OutMappings = [{".beam", EbinDir}],
    Opts = [verbose, return],

    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),

    %% Should compile successfully
    ?assertEqual(ok, Result),

    %% BEAM file should exist
    BeamFile = filename:join(EbinDir, "simple.beam"),
    ?assert(filelib:is_file(BeamFile)),
    ok.

compile_with_no_out_mappings(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),

    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile, "(defmodule simple)\n"),

    %% Empty mappings - should use fallback or fail gracefully
    OutMappings = [],
    Opts = [],

    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),

    %% Should handle gracefully (may succeed or fail, but shouldn't crash)
    ?assert(is_tuple(Result) orelse Result =:= ok),
    ok.

compile_with_multiple_mappings(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    SrcDir = maps:get(src_dir, AppData),
    EbinDir = maps:get(ebin_dir, AppData),

    SourceFile = filename:join(SrcDir, "module.lfe"),
    test_utils:write_file(SourceFile, "(defmodule module)\n"),

    %% Multiple output mappings
    OutDir1 = filename:join(TestDir, "out1"),
    OutDir2 = filename:join(TestDir, "out2"),
    filelib:ensure_dir(filename:join(OutDir1, "dummy")),
    filelib:ensure_dir(filename:join(OutDir2, "dummy")),

    OutMappings = [{".beam", EbinDir}, {".out", OutDir1}],
    Opts = [verbose, return],

    Result = r3lfe_compiler_mod:compile(SourceFile, OutMappings, dict:new(), Opts),

    %% Should compile
    ?assertEqual(ok, Result),
    ok.

source_to_target_no_mappings(_Config) ->
    Source = "/tmp/test.lfe",
    OutMappings = [],

    Target = r3lfe_compiler_mod:source_to_target(Source, OutMappings),

    %% Should return something (possibly error or default)
    ?assert(is_list(Target) orelse Target =:= error),
    ok.

source_to_target_with_mapping(_Config) ->
    Source = "/tmp/src/test.lfe",
    OutMappings = [{".beam", "/tmp/ebin"}],

    Target = r3lfe_compiler_mod:source_to_target(Source, OutMappings),

    %% Should map to .beam in ebin
    ?assert(is_list(Target)),
    ?assert(string:str(Target, ".beam") > 0),
    ?assert(string:str(Target, "/tmp/ebin") > 0),
    ok.

needs_compilation_missing_source(_Config) ->
    %% Create DAG
    G = digraph:new([acyclic]),

    %% Source file that doesn't exist
    NonExistent = "/tmp/nonexistent.lfe",
    digraph:add_vertex(G, NonExistent),

    OutMappings = [{".beam", "/tmp"}],

    %% Should return true (needs compilation to get error)
    ?assert(r3lfe_compiler_mod:needs_compilation(G, NonExistent, OutMappings)),

    digraph:delete(G),
    ok.

needs_compilation_source_newer(Config) ->
    TestDir = ?config(test_dir, Config),
    G = digraph:new([acyclic]),

    %% Create source and target
    Source = filename:join(TestDir, "test.lfe"),
    Target = filename:join(TestDir, "test.beam"),

    test_utils:write_file(Source, "(defmodule test)\n"),
    test_utils:write_file(Target, "fake beam"),

    %% Make source newer
    timer:sleep(1100),
    test_utils:write_file(Source, "(defmodule test)\n%% modified\n"),

    digraph:add_vertex(G, Source),
    OutMappings = [{".beam", TestDir}],

    %% Should need compilation (source is newer)
    ?assert(r3lfe_compiler_mod:needs_compilation(G, Source, OutMappings)),

    digraph:delete(G),
    ok.

needs_compilation_target_newer(Config) ->
    TestDir = ?config(test_dir, Config),
    G = digraph:new([acyclic]),

    %% Create source and target
    Source = filename:join(TestDir, "test.lfe"),
    Target = filename:join(TestDir, "test.beam"),

    test_utils:write_file(Source, "(defmodule test)\n"),

    %% Make target newer
    timer:sleep(1100),
    test_utils:write_file(Target, "fake beam"),

    digraph:add_vertex(G, Source),
    OutMappings = [{".beam", TestDir}],

    %% Should not need compilation (target is newer)
    ?assertNot(r3lfe_compiler_mod:needs_compilation(G, Source, OutMappings)),

    digraph:delete(G),
    ok.


clean_removes_beam_files(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create some .beam files
    Files = [
        filename:join(TestDir, "module1.beam"),
        filename:join(TestDir, "module2.beam")
    ],

    lists:foreach(
        fun(File) ->
            test_utils:write_file(File, "fake beam content")
        end,
        Files
    ),

    %% Verify they exist
    lists:foreach(
        fun(File) ->
            ?assert(filelib:is_file(File))
        end,
        Files
    ),

    %% Clean them
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    ok = r3lfe_compiler_mod:clean(Files, AppInfo),

    %% Files should be deleted
    lists:foreach(
        fun(File) ->
            ?assertNot(filelib:is_file(File))
        end,
        Files
    ),
    ok.

context_returns_map_with_correct_keys(_Config) ->
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", "/tmp/test"),

    Context = r3lfe_compiler_mod:context(AppInfo),

    %% Should return a map with the expected keys
    ?assert(is_map(Context)),
    ?assert(maps:is_key(src_dirs, Context)),
    ?assert(maps:is_key(include_dirs, Context)),
    ?assert(maps:is_key(src_ext, Context)),
    ?assert(maps:is_key(out_mappings, Context)),
    ?assert(maps:is_key(dependencies_opts, Context)),

    %% Verify types
    ?assert(is_list(maps:get(src_dirs, Context))),
    ?assert(is_list(maps:get(include_dirs, Context))),
    ?assertEqual(".lfe", maps:get(src_ext, Context)),
    ?assert(is_list(maps:get(out_mappings, Context))),
    ok.

format_error_returns_string(_Config) ->
    %% Test that format_error/1 returns a string
    Result = r3lfe_compiler_mod:format_error(some_error),

    ?assert(is_list(Result) orelse is_binary(Result)),
    ok.
