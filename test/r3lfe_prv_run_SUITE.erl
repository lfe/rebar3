-module(r3lfe_prv_run_SUITE).

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
    run_provider_registers/1,
    run_no_main_error/1,
    run_format_error_types/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        run_provider_registers,
        run_no_main_error,
        run_format_error_types
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("run"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

run_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_run:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Run provider should be registered"),

    ok.

run_no_main_error(_Config) ->
    %% No main specified
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[], []}),

    Result = r3lfe_prv_run:do(State1),

    ?assertMatch({error, _}, Result),

    ok.

run_format_error_types(_Config) ->
    Errors = [
        no_main_file,
        {file_not_found, "/tmp/missing.lfe"},
        {lfescript_error, some_reason},
        {run_error, another_reason}
    ],

    lists:foreach(
        fun(Error) ->
            Msg = r3lfe_prv_run:format_error(Error),
            ?assert(is_list(Msg)),
            ?assert(length(Msg) > 0)
        end,
        Errors
    ),

    ok.
