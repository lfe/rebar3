-module(rb3lfe_config_SUITE).

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
    get_lfe_opts_defaults/1,
    get_lfe_opts_merged/1,
    get_src_dirs_default/1,
    get_src_dirs_custom/1,
    get_include_dirs_default/1,
    get_include_dirs_custom/1,
    merge_opts_simple/1,
    merge_opts_override/1,
    normalize_src_dirs/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        get_lfe_opts_defaults,
        get_lfe_opts_merged,
        get_src_dirs_default,
        get_src_dirs_custom,
        get_include_dirs_default,
        get_include_dirs_custom,
        merge_opts_simple,
        merge_opts_override,
        normalize_src_dirs
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

get_lfe_opts_defaults(_Config) ->
    %% Create a minimal app info with no custom opts
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", "/tmp/test"),

    Opts = rb3lfe_config:get_lfe_opts(AppInfo),

    %% Should contain default options
    ?assert(lists:member(return, Opts)),
    ?assert(lists:member(verbose, Opts)),
    ok.

get_lfe_opts_merged(_Config) ->
    %% Create app info with custom LFE opts
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", "/tmp/test"),
    Opts = rebar_app_info:opts(AppInfo),
    Opts1 = rebar_opts:set(Opts, lfe_opts, [{debug_info, true}]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),

    LfeOpts = rb3lfe_config:get_lfe_opts(AppInfo1),

    %% Should contain both defaults and custom opts
    ?assert(lists:member(return, LfeOpts)),
    ?assert(lists:keymember(debug_info, 1, LfeOpts)),
    ok.

get_src_dirs_default(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),

    SrcDirs = rb3lfe_config:get_src_dirs(AppInfo),

    %% Should have default src directory
    ?assertEqual(1, length(SrcDirs)),
    ?assert(lists:any(
        fun(Dir) -> filename:basename(Dir) =:= "src" end,
        SrcDirs
    )),
    ok.

get_src_dirs_custom(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    %% Create additional source directory
    ExtraDir = filename:join(AppDir, "extra_src"),
    ok = filelib:ensure_dir(filename:join(ExtraDir, "dummy")),

    %% Configure custom source directories
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Opts = rebar_app_info:opts(AppInfo),
    Opts1 = rebar_opts:set(Opts, src_dirs, ["src", "extra_src"]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),

    SrcDirs = rb3lfe_config:get_src_dirs(AppInfo1),

    %% Should have both directories
    ?assertEqual(2, length(SrcDirs)),
    ok.

get_include_dirs_default(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),

    IncludeDirs = rb3lfe_config:get_include_dirs(AppInfo),

    %% Should have default include directory
    ?assert(length(IncludeDirs) >= 1),
    ?assert(lists:any(
        fun(Dir) -> filename:basename(Dir) =:= "include" end,
        IncludeDirs
    )),
    ok.

get_include_dirs_custom(Config) ->
    TestDir = ?config(test_dir, Config),
    AppData = test_utils:create_test_app(TestDir),
    AppDir = maps:get(dir, AppData),

    %% Configure custom include directories
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", AppDir),
    Opts = rebar_app_info:opts(AppInfo),
    Opts1 = rebar_opts:set(Opts, lfe_include_dirs, ["other_include"]),
    AppInfo1 = rebar_app_info:opts(AppInfo, Opts1),

    IncludeDirs = rb3lfe_config:get_include_dirs(AppInfo1),

    %% Should have custom directory
    ?assert(lists:any(
        fun(Dir) -> filename:basename(Dir) =:= "other_include" end,
        IncludeDirs
    )),
    ok.

merge_opts_simple(_Config) ->
    Defaults = [return, verbose],
    Overrides = [debug_info],

    Merged = rb3lfe_config:merge_opts(Defaults, Overrides),

    ?assert(lists:member(return, Merged)),
    ?assert(lists:member(verbose, Merged)),
    ?assert(lists:member(debug_info, Merged)),
    ok.

merge_opts_override(_Config) ->
    Defaults = [{outdir, "/tmp/default"}],
    Overrides = [{outdir, "/tmp/override"}],

    Merged = rb3lfe_config:merge_opts(Defaults, Overrides),

    %% Override should win
    ?assertEqual([{outdir, "/tmp/override"}], Merged),
    ok.

normalize_src_dirs(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create some test directories
    Dir1 = filename:join(TestDir, "src1"),
    Dir2 = filename:join(TestDir, "src2"),
    ok = filelib:ensure_dir(filename:join(Dir1, "dummy")),
    ok = filelib:ensure_dir(filename:join(Dir2, "dummy")),

    Normalized = rb3lfe_config:normalize_src_dirs(TestDir, ["src1", "src2"]),

    ?assertEqual(2, length(Normalized)),
    ?assert(lists:all(fun filelib:is_dir/1, Normalized)),
    ok.
