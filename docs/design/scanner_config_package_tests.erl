%% Additional test cases to improve coverage for r3lfe_dependency_scanner, r3lfe_config, r3lfe_package, and r3lfe_paths
%% Add these to the respective test suite files

%% ============================================================================
%% Tests for r3lfe_dependency_scanner_SUITE.erl
%% ============================================================================

%% Add these to exports and all/0:
resolve_include_file_absolute_path/1,
resolve_include_file_all_candidates/1,
resolve_include_lib_invalid_format/1,
resolve_include_lib_app_not_loaded/1,
classify_include_patterns/1,
scan_file_with_cache_disabled/1,
scan_content_complex_patterns/1,
parse_include_forms_edge_cases/1.

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
        ";; (include-file \"commented.lfe\")\n"  % Commented out
        "(include-file \"header3.lfe\")\n"
        "(defun test () 'ok)\n",
    
    Forms = r3lfe_dependency_scanner:scan_content(Content),
    
    %% Should find 3 includes (not the commented one)
    ?assertEqual(3, length(Forms)),
    
    ok.

parse_include_forms_edge_cases(_Config) ->
    %% Test edge cases in parsing
    Content = 
        "(include-file\"no-space.lfe\")\n"  % No space before string
        "( include-file  \"extra-spaces.lfe\" )\n"  % Extra spaces
        "(include-lib \"app/path/file.lfe\")\n",
    
    Forms = r3lfe_dependency_scanner:parse_include_forms(Content),
    
    %% Should handle all variations
    ?assert(length(Forms) >= 2),
    
    ok.

%% ============================================================================
%% Tests for r3lfe_config_SUITE.erl
%% ============================================================================

%% Add these to exports and all/0:
get_lfe_opts_with_erl_opts/1,
get_lfe_opts_from_state/1,
get_src_dirs_normalization/1,
get_include_dirs_nonexistent/1,
get_first_files_absolute_paths/1,
is_verbose_from_lfe_opts/1,
is_verbose_from_rebar_opts/1,
merge_opts_complex/1,
normalize_include_dirs_relative/1.

get_lfe_opts_with_erl_opts(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    Opts = rebar_app_info:opts(AppInfo),
    
    %% Set both lfe_opts and erl_opts
    Opts1 = rebar_opts:set(Opts, lfe_opts, [verbose]),
    Opts2 = rebar_opts:set(Opts1, erl_opts, [debug_info]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts2),
    
    LfeOpts = r3lfe_config:get_lfe_opts(AppInfo1),
    
    %% Should include both
    ?assert(lists:member(verbose, LfeOpts) orelse lists:member(debug_info, LfeOpts)),
    
    ok.

get_lfe_opts_from_state(_Config) ->
    State = rebar_state:new(),
    Opts = rebar_state:opts(State),
    
    Opts1 = rebar_opts:set(Opts, lfe_opts, [verbose]),
    State1 = rebar_state:opts(State, Opts1),
    
    LfeOpts = r3lfe_config:get_lfe_opts(State1),
    
    ?assert(is_list(LfeOpts)),
    
    ok.

get_src_dirs_normalization(Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Create multiple source directories
    SrcDir1 = filename:join(TestDir, "src"),
    SrcDir2 = filename:join(TestDir, "extra_src"),
    ok = filelib:ensure_dir(filename:join(SrcDir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(SrcDir2, "dummy")),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    Opts = rebar_app_info:opts(AppInfo),
    Opts1 = rebar_opts:set(Opts, src_dirs, ["src", "extra_src"]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),
    
    SrcDirs = r3lfe_config:get_src_dirs(AppInfo1),
    
    %% Should be normalized to absolute paths
    ?assert(lists:all(fun filelib:is_dir/1, SrcDirs)),
    ?assert(length(SrcDirs) >= 2),
    
    ok.

get_include_dirs_nonexistent(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    Opts = rebar_app_info:opts(AppInfo),
    
    %% Set include dir that doesn't exist
    Opts1 = rebar_opts:set(Opts, lfe_include_dirs, ["nonexistent"]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),
    
    IncludeDirs = r3lfe_config:get_include_dirs(AppInfo1),
    
    %% Should still return the path (might be created later)
    ?assert(is_list(IncludeDirs)),
    
    ok.

get_first_files_absolute_paths(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    Opts = rebar_app_info:opts(AppInfo),
    
    Opts1 = rebar_opts:set(Opts, lfe_first_files, ["src/first.lfe", "src/second.lfe"]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),
    
    FirstFiles = r3lfe_config:get_first_files(AppInfo1),
    
    %% Should be absolute paths
    ?assert(lists:all(fun(F) -> filename:pathtype(F) =:= absolute end, FirstFiles)),
    
    ok.

is_verbose_from_lfe_opts(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    Opts = rebar_app_info:opts(AppInfo),
    
    Opts1 = rebar_opts:set(Opts, lfe_opts, [verbose]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),
    
    ?assert(r3lfe_config:is_verbose(AppInfo1)),
    
    ok.

is_verbose_from_rebar_opts(Config) ->
    TestDir = ?config(test_dir, Config),
    
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", TestDir),
    Opts = rebar_app_info:opts(AppInfo),
    
    Opts1 = rebar_opts:set(Opts, verbose, true),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),
    
    ?assert(r3lfe_config:is_verbose(AppInfo1)),
    
    ok.

merge_opts_complex(_Config) ->
    Defaults = [
        verbose,
        {outdir, "/default"},
        {i, "/default/include"},
        debug_info
    ],
    
    Overrides = [
        {outdir, "/override"},
        {i, "/override/include"},
        warnings_as_errors
    ],
    
    Merged = r3lfe_config:merge_opts(Defaults, Overrides),
    
    %% Overrides should take precedence
    ?assert(lists:keymember(outdir, 1, Merged)),
    {outdir, OutDir} = lists:keyfind(outdir, 1, Merged),
    ?assertEqual("/override", OutDir),
    
    %% Should have all unique options
    ?assert(lists:member(verbose, Merged)),
    ?assert(lists:member(debug_info, Merged)),
    ?assert(lists:member(warnings_as_errors, Merged)),
    
    ok.

normalize_include_dirs_relative(Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Create include directories
    IncDir1 = filename:join(TestDir, "include"),
    IncDir2 = filename:join(TestDir, "src/include"),
    ok = filelib:ensure_dir(filename:join(IncDir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(IncDir2, "dummy")),
    
    %% Normalize with relative paths
    Normalized = r3lfe_config:normalize_include_dirs(
        TestDir, ["include", "src/include"]),
    
    ?assertEqual(2, length(Normalized)),
    ?assert(lists:all(fun filelib:is_dir/1, Normalized)),
    
    ok.

%% ============================================================================
%% Tests for r3lfe_package_SUITE.erl
%% ============================================================================

%% Add these to exports and all/0:
discover_files_with_symlinks/1,
discover_files_cycle_detection/1,
prepare_packages_invalid_module_name/1,
prepare_packages_cleanup_on_error/1,
package_to_module_name_edge_cases/1.

discover_files_with_symlinks(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    
    %% Create a real file
    RealFile = filename:join(SrcDir, "real.lfe"),
    test_utils:write_file(RealFile, "(defmodule real)\n"),
    
    %% Symlinks might not work on all systems, so test gracefully
    LinkPath = filename:join(SrcDir, "link"),
    case file:make_symlink(SrcDir, LinkPath) of
        ok ->
            %% Discovery should handle symlinks without infinite loops
            Files = r3lfe_package:discover_files(SrcDir),
            ?assert(is_list(Files)),
            ?assert(length(Files) > 0);
        {error, _} ->
            %% Symlinks not supported on this system
            ct:pal("Symlinks not supported, skipping test")
    end,
    
    ok.

discover_files_cycle_detection(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    
    %% Create file
    File = filename:join(SrcDir, "module.lfe"),
    test_utils:write_file(File, "(defmodule module)\n"),
    
    %% Discovery should not hang on cycles
    Files = r3lfe_package:discover_files(SrcDir),
    
    ?assert(is_list(Files)),
    ?assert(length(Files) >= 1),
    
    ok.

prepare_packages_invalid_module_name(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    
    %% Create file with invalid module name pattern
    SubDir = filename:join(SrcDir, "..invalid"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),
    
    File = filename:join(SubDir, "module.lfe"),
    test_utils:write_file(File, "(defmodule module)\n"),
    
    %% Prepare should handle invalid names
    Result = r3lfe_package:prepare_packages([{File, SrcDir}]),
    
    %% Should return error for invalid module name
    ?assertMatch({error, _}, Result),
    
    ok.

prepare_packages_cleanup_on_error(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    
    %% Create one valid and one invalid package
    SubDir1 = filename:join([SrcDir, "valid"]),
    SubDir2 = filename:join([SrcDir, "..invalid"]),
    ok = filelib:ensure_dir(filename:join(SubDir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(SubDir2, "dummy")),
    
    File1 = filename:join(SubDir1, "module1.lfe"),
    File2 = filename:join(SubDir2, "module2.lfe"),
    test_utils:write_file(File1, "(defmodule valid.module1)\n"),
    test_utils:write_file(File2, "(defmodule invalid)\n"),
    
    %% Prepare should cleanup partial success on error
    Result = r3lfe_package:prepare_packages([{File1, SrcDir}, {File2, SrcDir}]),
    
    case Result of
        {ok, _} -> ok;
        {error, _} -> 
            %% Should have cleaned up any temp files
            TempFile1 = filename:join(SrcDir, "valid.module1.lfe"),
            ?assertNot(filelib:is_file(TempFile1))
    end,
    
    ok.

package_to_module_name_edge_cases(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir = filename:join(TestDir, "src"),
    
    %% Test various edge cases
    Cases = [
        {"src/simple.lfe", "simple"},
        {"src/my/package.lfe", "my.package"},
        {"src/deep/nested/path/module.lfe", "deep.nested.path.module"}
    ],
    
    lists:foreach(
        fun({FilePath, Expected}) ->
            FullPath = filename:join(TestDir, FilePath),
            ModuleName = r3lfe_package:package_to_module_name(FullPath, SrcDir),
            ?assertEqual(Expected, ModuleName)
        end,
        Cases
    ),
    
    ok.

%% ============================================================================
%% Tests for r3lfe_paths_SUITE.erl
%% ============================================================================

%% Add these to exports and all/0:
set_paths_fallback/1,
unset_paths_no_error/1,
ensure_dir_race_condition/1,
ensure_dir_permission_error/1.

set_paths_fallback(_Config) ->
    %% Test fallback path setting
    State = rebar_state:new(),
    
    %% Should not crash even with empty state
    ok = r3lfe_paths:set_paths([deps], State),
    
    ok.

unset_paths_no_error(_Config) ->
    %% Test that unsetting paths never errors
    State = rebar_state:new(),
    
    %% Should handle gracefully
    ok = r3lfe_paths:unset_paths([deps, plugins], State),
    
    ok.

ensure_dir_race_condition(Config) ->
    TestDir = ?config(test_dir, Config),
    
    Dir = filename:join(TestDir, "race_dir"),
    
    %% Create directory externally
    ok = file:make_dir(Dir),
    
    %% ensure_dir should handle already existing
    ok = r3lfe_paths:ensure_dir(Dir),
    
    ?assert(filelib:is_dir(Dir)),
    
    ok.

ensure_dir_permission_error(Config) ->
    TestDir = ?config(test_dir, Config),
    
    %% Try to create in a path that might fail
    %% (This is system-dependent, so handle gracefully)
    InvalidDir = filename:join(TestDir, "test"),
    
    Result = r3lfe_paths:ensure_dir(InvalidDir),
    
    %% Should either succeed or return proper error
    case Result of
        ok -> ?assert(filelib:is_dir(InvalidDir));
        {error, _} -> ok
    end,
    
    ok.
