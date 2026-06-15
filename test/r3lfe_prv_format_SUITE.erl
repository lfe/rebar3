-module(r3lfe_prv_format_SUITE).

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
    format_provider_registers/1,
    format_inplace_reformats/1,
    format_unchanged_not_rewritten/1,
    format_path_to_file/1,
    format_path_to_dir/1,
    format_default_src_dirs/1,
    format_syntax_error_skips/1,
    format_nonexistent_path/1,
    format_dry_run_stub/1,
    format_check_stub/1,
    format_mutually_exclusive/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        format_provider_registers,
        format_inplace_reformats,
        format_unchanged_not_rewritten,
        format_path_to_file,
        format_path_to_dir,
        format_default_src_dirs,
        format_syntax_error_skips,
        format_nonexistent_path,
        format_dry_run_stub,
        format_check_stub,
        format_mutually_exclusive
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("format"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%% Input without trailing newline → formatter always adds one.
unformatted() -> <<"(foo 1 2 3)">>.
formatted()   -> <<"(foo 1 2 3)\n">>.

make_state(Opts) ->
    rebar_state:command_parsed_args(rebar_state:new(), {Opts, []}).

%%====================================================================
%% Test Cases
%%====================================================================

format_provider_registers(_Config) ->
    State = rebar_state:new(),
    {ok, State1} = r3lfe_prv_format:init(State),
    Providers = rebar_state:providers(State1),
    ?assert(length(Providers) > 0),
    ok.

format_inplace_reformats(Config) ->
    TestDir = ?config(test_dir, Config),
    File = filename:join(TestDir, "a.lfe"),
    test_utils:write_file(File, unformatted()),

    State = make_state([{path, TestDir}]),
    ?assertMatch({ok, _}, r3lfe_prv_format:do(State)),

    {ok, Content} = file:read_file(File),
    ?assertEqual(formatted(), Content),
    ok.

format_unchanged_not_rewritten(Config) ->
    TestDir = ?config(test_dir, Config),
    File = filename:join(TestDir, "a.lfe"),
    test_utils:write_file(File, formatted()),

    State = make_state([{path, TestDir}]),
    ?assertMatch({ok, _}, r3lfe_prv_format:do(State)),

    {ok, Content} = file:read_file(File),
    ?assertEqual(formatted(), Content),
    ok.

format_path_to_file(Config) ->
    TestDir = ?config(test_dir, Config),
    Target = filename:join(TestDir, "target.lfe"),
    Other  = filename:join(TestDir, "other.lfe"),
    test_utils:write_file(Target, unformatted()),
    test_utils:write_file(Other, unformatted()),

    %% Only format the target file; other should remain untouched.
    State = make_state([{path, Target}]),
    ?assertMatch({ok, _}, r3lfe_prv_format:do(State)),

    {ok, TargetContent} = file:read_file(Target),
    {ok, OtherContent}  = file:read_file(Other),
    ?assertEqual(formatted(), TargetContent),
    ?assertEqual(unformatted(), OtherContent),
    ok.

format_path_to_dir(Config) ->
    TestDir = ?config(test_dir, Config),
    SubDir  = filename:join(TestDir, "sub"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    File1 = filename:join(SubDir, "a.lfe"),
    File2 = filename:join(SubDir, "b.lfe"),
    Outside = filename:join(TestDir, "outside.lfe"),
    test_utils:write_file(File1, unformatted()),
    test_utils:write_file(File2, unformatted()),
    test_utils:write_file(Outside, unformatted()),

    %% Format only the subdir; outside file must remain untouched.
    State = make_state([{path, SubDir}]),
    ?assertMatch({ok, _}, r3lfe_prv_format:do(State)),

    {ok, C1}  = file:read_file(File1),
    {ok, C2}  = file:read_file(File2),
    {ok, COu} = file:read_file(Outside),
    ?assertEqual(formatted(), C1),
    ?assertEqual(formatted(), C2),
    ?assertEqual(unformatted(), COu),
    ok.

format_default_src_dirs(Config) ->
    TestDir = ?config(test_dir, Config),
    SrcDir  = filename:join(TestDir, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    File = filename:join(SrcDir, "a.lfe"),
    test_utils:write_file(File, unformatted()),

    {ok, AppInfo} = rebar_app_info:new(testapp, "1.0.0", TestDir),
    BaseState = rebar_state:current_app(rebar_state:new(), AppInfo),
    State = rebar_state:command_parsed_args(BaseState, {[], []}),

    ?assertMatch({ok, _}, r3lfe_prv_format:do(State)),

    {ok, Content} = file:read_file(File),
    ?assertEqual(formatted(), Content),
    ok.

format_syntax_error_skips(Config) ->
    TestDir = ?config(test_dir, Config),
    Good = filename:join(TestDir, "good.lfe"),
    Bad  = filename:join(TestDir, "bad.lfe"),
    test_utils:write_file(Good, unformatted()),
    test_utils:write_file(Bad, <<"(unclosed">>),

    State = make_state([{path, TestDir}]),
    Result = r3lfe_prv_format:do(State),

    ?assertMatch({error, _}, Result),

    %% Good file must have been formatted despite the bad file.
    {ok, GoodContent} = file:read_file(Good),
    {ok, BadContent}  = file:read_file(Bad),
    ?assertEqual(formatted(), GoodContent),
    ?assertEqual(<<"(unclosed">>, BadContent),
    ok.

format_nonexistent_path(_Config) ->
    State = make_state([{path, "/nonexistent/path/that/does/not/exist"}]),
    ?assertMatch({error, _}, r3lfe_prv_format:do(State)),
    ok.

format_dry_run_stub(_Config) ->
    State = make_state([{dry_run, true}]),
    ?assertMatch({error, _}, r3lfe_prv_format:do(State)),
    ok.

format_check_stub(_Config) ->
    State = make_state([{check, true}]),
    ?assertMatch({error, _}, r3lfe_prv_format:do(State)),
    ok.

format_mutually_exclusive(_Config) ->
    State = make_state([{dry_run, true}, {check, true}]),
    Result = r3lfe_prv_format:do(State),
    ?assertMatch({error, _}, Result),
    {error, Msg} = Result,
    ?assert(string:find(Msg, "mutually exclusive") =/= nomatch),
    ok.
