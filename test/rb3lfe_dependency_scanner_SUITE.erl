-module(rb3lfe_dependency_scanner_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([parse_include_forms_single/1, parse_include_forms_multiple/1,
         parse_include_forms_none/1, extract_include_path_file/1,
         extract_include_path_lib/1, scan_content_mixed/1,
         resolve_include_file_in_include_dir/1, resolve_include_file_relative/1,
         resolve_include_lib_found/1, resolve_include_lib_not_found/1,
         scan_file_with_includes/1, scan_file_no_includes/1]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [parse_include_forms_single, parse_include_forms_multiple,
     parse_include_forms_none, extract_include_path_file,
     extract_include_path_lib, scan_content_mixed,
     resolve_include_file_in_include_dir, resolve_include_file_relative,
     resolve_include_lib_found, resolve_include_lib_not_found,
     scan_file_with_includes, scan_file_no_includes].

init_per_suite(Config) ->
    rb3lfe_dep_cache:init(),
    Config.

end_per_suite(_Config) ->
    rb3lfe_dep_cache:clear(),
    ok.

init_per_testcase(_TestCase, Config) ->
    [{test_dir, test_utils:create_temp_dir()} | Config].

end_per_testcase(_TestCase, Config) ->
    test_utils:cleanup_temp_dir(?config(test_dir, Config)),
    ok.

%%====================================================================
%% Test Cases - Parsing
%%====================================================================

parse_include_forms_single(_Config) ->
    Content = "(defmodule test)\n(include-file \"header.lfe\")\n",
    Forms = rb3lfe_dependency_scanner:parse_include_forms(Content),
    ?assertEqual(1, length(Forms)),
    ok.

parse_include_forms_multiple(_Config) ->
    Content =
        "(defmodule test)\n"
        "(include-file \"header1.lfe\")\n"
        "(include-lib \"app/include/header2.lfe\")\n"
        "(include-file \"header3.lfe\")\n",
    Forms = rb3lfe_dependency_scanner:parse_include_forms(Content),
    ?assertEqual(3, length(Forms)),
    ok.

parse_include_forms_none(_Config) ->
    Content = "(defmodule test)\n(defun hello () 'world)\n",
    Forms = rb3lfe_dependency_scanner:parse_include_forms(Content),
    ?assertEqual(0, length(Forms)),
    ok.

extract_include_path_file(_Config) ->
    Form = "(include-file \"records.lfe\")",
    Result = rb3lfe_dependency_scanner:extract_include_path(Form),
    ?assertMatch({ok, include_file, "records.lfe"}, Result),
    ok.

extract_include_path_lib(_Config) ->
    Form = "(include-lib \"lfe/include/clj.lfe\")",
    Result = rb3lfe_dependency_scanner:extract_include_path(Form),
    ?assertMatch({ok, include_lib, "lfe/include/clj.lfe"}, Result),
    ok.

scan_content_mixed(_Config) ->
    Content =
        "(defmodule test)\n"
        "(include-file \"local.lfe\")\n"
        "(include-lib \"lfe/include/clj.lfe\")\n"
        "(defun test () 'ok)\n",
    Forms = rb3lfe_dependency_scanner:scan_content(Content),
    ?assertEqual(2, length(Forms)),
    ?assert(lists:member({include_file, "local.lfe"}, Forms)),
    ?assert(lists:member({include_lib, "lfe/include/clj.lfe"}, Forms)),
    ok.

%%====================================================================
%% Test Cases - Resolution
%%====================================================================

resolve_include_file_in_include_dir(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    HeaderFile = filename:join(IncludeDir, "records.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord person name age)\n"),

    Result = rb3lfe_dependency_scanner:resolve_include(
        {include_file, "records.lfe"}, AppDir, [IncludeDir]),

    ?assertMatch({ok, _Path}, Result),
    {ok, ResolvedPath} = Result,
    ?assert(filelib:is_file(ResolvedPath)),
    ok.

resolve_include_file_relative(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    HeaderFile = filename:join(SrcDir, "local.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord data value)\n"),

    Result = rb3lfe_dependency_scanner:resolve_include(
        {include_file, "src/local.lfe"}, AppDir, []),

    ?assertMatch({ok, _Path}, Result),
    ok.

resolve_include_lib_found(_Config) ->
    Result = rb3lfe_dependency_scanner:resolve_include(
        {include_lib, "lfe/include/clj.lfe"}, "/tmp", []),

    case Result of
        {ok, Path} ->
            ?assert(filelib:is_file(Path)),
            ok;
        {error, _} ->
            ct:pal("Note: LFE not on code path, skipping include-lib test"),
            ok
    end.

resolve_include_lib_not_found(_Config) ->
    Result = rb3lfe_dependency_scanner:resolve_include(
        {include_lib, "nonexistent_app/include/file.lfe"}, "/tmp", []),
    ?assertMatch({error, _}, Result),
    ok.

%%====================================================================
%% Test Cases - Full Scanning
%%====================================================================

scan_file_with_includes(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    test_utils:write_file(filename:join(IncludeDir, "records.lfe"),
                          "(defrecord person name age)\n"),
    test_utils:write_file(filename:join(IncludeDir, "macros.lfe"),
                          "(defmacro debug (x) `(io:format \"~p\" ,x))\n"),

    SourceFile = filename:join(SrcDir, "test_module.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule test-module)\n"
        "(include-file \"records.lfe\")\n"
        "(include-file \"macros.lfe\")\n"
        "(defun test () 'ok)\n"),

    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Deps = rb3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    ?assertEqual(2, length(Deps)),
    ?assert(lists:any(fun(P) -> filename:basename(P) =:= "records.lfe" end, Deps)),
    ?assert(lists:any(fun(P) -> filename:basename(P) =:= "macros.lfe" end, Deps)),
    ok.

scan_file_no_includes(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),

    SourceFile = filename:join(SrcDir, "simple.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule simple)\n(defun hello () 'world)\n"),

    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Deps = rb3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    ?assertEqual(0, length(Deps)),
    ok.
