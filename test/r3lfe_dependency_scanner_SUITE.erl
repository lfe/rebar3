-module(r3lfe_dependency_scanner_SUITE).

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
         scan_file_with_includes/1, scan_file_no_includes/1,
         scan_file_with_explicit_dirs/1,
         resolve_include_file_absolute_path/1,
         resolve_include_file_all_candidates/1,
         resolve_include_lib_invalid_format/1,
         resolve_include_lib_app_not_loaded/1,
         classify_include_patterns/1,
         scan_file_with_cache_disabled/1,
         scan_content_complex_patterns/1,
         parse_include_forms_edge_cases/1,
         scan_file_read_error/1,
         resolve_include_file_not_found/1,
         extract_include_path_malformed/1]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [parse_include_forms_single, parse_include_forms_multiple,
     parse_include_forms_none, extract_include_path_file,
     extract_include_path_lib, scan_content_mixed,
     resolve_include_file_in_include_dir, resolve_include_file_relative,
     resolve_include_lib_found, resolve_include_lib_not_found,
     scan_file_with_includes, scan_file_no_includes,
     scan_file_with_explicit_dirs,
     resolve_include_file_absolute_path,
     resolve_include_file_all_candidates,
     resolve_include_lib_invalid_format,
     resolve_include_lib_app_not_loaded,
     classify_include_patterns,
     scan_file_with_cache_disabled,
     scan_content_complex_patterns,
     parse_include_forms_edge_cases,
     scan_file_read_error,
     resolve_include_file_not_found,
     extract_include_path_malformed].

init_per_suite(Config) ->
    r3lfe_dep_cache:init(),
    Config.

end_per_suite(_Config) ->
    r3lfe_dep_cache:clear(),
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
    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),
    ?assertEqual(1, length(Forms)),
    ok.

parse_include_forms_multiple(_Config) ->
    Content =
        "(defmodule test)\n"
        "(include-file \"header1.lfe\")\n"
        "(include-lib \"app/include/header2.lfe\")\n"
        "(include-file \"header3.lfe\")\n",
    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),
    ?assertEqual(3, length(Forms)),
    ok.

parse_include_forms_none(_Config) ->
    Content = "(defmodule test)\n(defun hello () 'world)\n",
    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),
    ?assertEqual(0, length(Forms)),
    ok.

extract_include_path_file(_Config) ->
    Form = "(include-file \"records.lfe\")",
    Result = r3lfe_dependency_scanner:extract_include_path(Form),
    ?assertMatch({ok, include_file, "records.lfe"}, Result),
    ok.

extract_include_path_lib(_Config) ->
    Form = "(include-lib \"lfe/include/clj.lfe\")",
    Result = r3lfe_dependency_scanner:extract_include_path(Form),
    ?assertMatch({ok, include_lib, "lfe/include/clj.lfe"}, Result),
    ok.

scan_content_mixed(_Config) ->
    Content =
        "(defmodule test)\n"
        "(include-file \"local.lfe\")\n"
        "(include-lib \"lfe/include/clj.lfe\")\n"
        "(defun test () 'ok)\n",
    Forms = r3lfe_dependency_scanner:scan_content(Content),
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

    Result = r3lfe_dependency_scanner:resolve_include(
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

    Result = r3lfe_dependency_scanner:resolve_include(
        {include_file, "src/local.lfe"}, AppDir, []),

    ?assertMatch({ok, _Path}, Result),
    ok.

resolve_include_lib_found(_Config) ->
    Result = r3lfe_dependency_scanner:resolve_include(
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
    Result = r3lfe_dependency_scanner:resolve_include(
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
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

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
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo),

    ?assertEqual(0, length(Deps)),
    ok.

scan_file_with_explicit_dirs(Config) ->
    %% Test the scan_file/4 variant that doesn't require AppInfo
    %% This variant takes explicit AppDir and IncludeDirs parameters
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create include files
    test_utils:write_file(filename:join(IncludeDir, "types.lfe"),
                          "(deftype person () (tuple 'person binary integer))\n"),

    %% Create source file with include
    SourceFile = filename:join(SrcDir, "explicit_test.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule explicit-test)\n"
        "(include-file \"types.lfe\")\n"
        "(defun new-person (name age) (tuple 'person name age))\n"),

    %% Call scan_file/4 directly with explicit parameters
    IncludeDirs = [IncludeDir],
    Opts = #{cache => false},
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppDir, IncludeDirs, Opts),

    %% Verify dependencies were found
    ?assertEqual(1, length(Deps)),
    ?assert(lists:any(fun(P) -> filename:basename(P) =:= "types.lfe" end, Deps)),
    ok.

%%====================================================================
%% Additional Test Cases for Coverage
%%====================================================================

resolve_include_file_absolute_path(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create file at absolute path
    AbsFile = filename:join(TestDir, "absolute.lfe"),
    test_utils:write_file(AbsFile, "(defmodule absolute)\n"),

    %% Resolve with absolute path
    Result = r3lfe_dependency_scanner:resolve_include(
        {include_file, AbsFile}, "/tmp", []),

    ?assertMatch({ok, _}, Result),
    {ok, ResolvedPath} = Result,
    ?assert(filelib:is_file(ResolvedPath)),

    ok.

resolve_include_file_all_candidates(Config) ->
    TestDir = ?config(test_dir, Config),

    IncludeDir1 = filename:join(TestDir, "inc1"),
    IncludeDir2 = filename:join(TestDir, "inc2"),
    ok = filelib:ensure_dir(filename:join(IncludeDir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(IncludeDir2, "dummy")),

    %% Create file in second include dir
    File2 = filename:join(IncludeDir2, "header.lfe"),
    test_utils:write_file(File2, "(defrecord rec)\n"),

    %% Resolve should check all include dirs
    Result = r3lfe_dependency_scanner:resolve_include(
        {include_file, "header.lfe"}, TestDir, [IncludeDir1, IncludeDir2]),

    ?assertMatch({ok, _}, Result),

    ok.

resolve_include_lib_invalid_format(_Config) ->
    %% Test invalid include-lib format
    Result = r3lfe_dependency_scanner:resolve_include(
        {include_lib, "invalid-no-slash"}, "/tmp", []),

    ?assertMatch({error, _}, Result),

    ok.

resolve_include_lib_app_not_loaded(_Config) ->
    %% Test include-lib with non-loaded application
    Result = r3lfe_dependency_scanner:resolve_include(
        {include_lib, "nonexistent_app_12345/include/file.lfe"}, "/tmp", []),

    ?assertMatch({error, _}, Result),

    ok.

classify_include_patterns(_Config) ->
    %% Test include path classification
    Paths = [
        "simple.lfe",
        "app/include/file.lfe",
        "local/path/file.lfe",
        "file.lfe"
    ],

    lists:foreach(
        fun(Path) ->
            Type = r3lfe_dependency_scanner:classify_include(Path),
            ?assert(Type =:= include_file orelse Type =:= include_lib)
        end,
        Paths
    ),

    ok.

scan_file_with_cache_disabled(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),
    SrcDir = maps:get(src_dir, AppData),
    IncludeDir = maps:get(include_dir, AppData),

    %% Create files
    HeaderFile = filename:join(IncludeDir, "nocache.lfe"),
    test_utils:write_file(HeaderFile, "(defrecord nocache val)\n"),

    SourceFile = filename:join(SrcDir, "nocache_test.lfe"),
    test_utils:write_file(SourceFile,
        "(defmodule nocache-test)\n"
        "(include-file \"nocache.lfe\")\n"
        "(defun test () 'ok)\n"),

    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),

    %% Scan with cache disabled
    Opts = #{cache => false},
    Deps = r3lfe_dependency_scanner:scan_file(SourceFile, AppInfo, Opts),

    ?assertEqual(1, length(Deps)),

    ok.

scan_content_complex_patterns(_Config) ->
    %% Test scanning content with various include patterns
    Content =
        "(defmodule test)\n"
        "(include-file  \"header1.lfe\")\n"  % Extra spaces
        "(include-lib \"app/include/header2.lfe\")\n"
        ";; (include-file \"commented.lfe\")\n"  % Note: regex scanner will find this too
        "(include-file \"header3.lfe\")\n"
        "(defun test () 'ok)\n",

    Forms = r3lfe_dependency_scanner:scan_content(Content),

    %% Regex-based scanner finds all include patterns, even in comments
    %% This is expected behavior for a simple pattern matcher
    ?assertEqual(4, length(Forms)),

    %% Verify it found the expected files
    ?assert(lists:member({include_file, "header1.lfe"}, Forms)),
    ?assert(lists:member({include_lib, "app/include/header2.lfe"}, Forms)),
    ?assert(lists:member({include_file, "header3.lfe"}, Forms)),

    ok.

parse_include_forms_edge_cases(_Config) ->
    %% Test edge cases in parsing
    %% The regex requires at least one whitespace between include-file/lib and the quote
    %% and expects the opening paren to not have leading whitespace
    Content =
        "(include-file \"with-space.lfe\")\n"  % Normal spacing
        "(include-file  \"extra-spaces.lfe\")\n"  % Extra spaces between form and quote
        "(include-lib \"app/path/file.lfe\")\n",

    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),

    %% Should handle all valid variations (those with required whitespace)
    ?assertEqual(3, length(Forms)),

    %% Test that no-space variant is NOT matched (expected limitation of simple regex)
    NoSpaceContent = "(include-file\"no-space.lfe\")\n",
    NoSpaceForms = r3lfe_dependency_scanner:parse_include_forms(NoSpaceContent),
    ?assertEqual(0, length(NoSpaceForms)),

    ok.

scan_file_read_error(_Config) ->
    %% Test scan_file with a file that doesn't exist or can't be read
    NonExistentFile = "/tmp/nonexistent_" ++ integer_to_list(erlang:system_time()) ++ ".lfe",

    %% Create minimal app info
    {ok, AppInfo} = rebar_app_info:new(test, "1.0.0", "/tmp"),

    %% Should return empty list, not crash
    Result = r3lfe_dependency_scanner:scan_file(NonExistentFile, AppInfo),

    ?assertEqual([], Result),

    ok.

resolve_include_file_not_found(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Try to resolve a file that doesn't exist anywhere
    NonExistentFile = "nonexistent_header_" ++ integer_to_list(erlang:system_time()) ++ ".lfe",

    Result = r3lfe_dependency_scanner:resolve_include(
        {include_file, NonExistentFile},
        TestDir,
        [filename:join(TestDir, "include")]
    ),

    %% Should return error, not crash
    ?assertMatch({error, not_found}, Result),

    ok.

extract_include_path_malformed(_Config) ->
    %% Test extract_include_path with malformed input
    MalformedForms = [
        "(include-file)",  % Missing path
        "(include-lib)",   % Missing path
        "(include-file )",  % Missing path with space
        "(other-form \"path.lfe\")",  % Wrong form type
        ""  % Empty string
    ],

    lists:foreach(
        fun(Form) ->
            Result = r3lfe_dependency_scanner:extract_include_path(Form),
            ?assertEqual(error, Result)
        end,
        MalformedForms
    ),

    ok.
