-module(r3lfe_rlwrap_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test cases
-export([
    has_rlwrap_returns_boolean/1,
    shell_quote_escapes_single_quotes/1,
    shell_quote_handles_no_quotes/1,
    get_history_file_default_location/1,
    get_history_file_custom_location/1,
    get_history_file_expands_tilde/1,
    get_completion_files_returns_list/1,
    should_use_rlwrap_default_true/1,
    should_use_rlwrap_respects_config/1,
    should_use_rlwrap_respects_no_rlwrap_flag/1,
    build_rlwrap_command_generates_valid_string/1,
    expand_home_with_tilde_slash/1,
    expand_home_without_tilde/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        has_rlwrap_returns_boolean,
        shell_quote_escapes_single_quotes,
        shell_quote_handles_no_quotes,
        get_history_file_default_location,
        get_history_file_custom_location,
        get_history_file_expands_tilde,
        get_completion_files_returns_list,
        should_use_rlwrap_default_true,
        should_use_rlwrap_respects_config,
        should_use_rlwrap_respects_no_rlwrap_flag,
        build_rlwrap_command_generates_valid_string,
        expand_home_with_tilde_slash,
        expand_home_without_tilde
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% rlwrap Integration Test Cases
%%====================================================================

has_rlwrap_returns_boolean(_Config) ->
    %% This test will vary by system
    Result = r3lfe_rlwrap:has_rlwrap(),
    ?assert(is_boolean(Result)),
    ok.

shell_quote_escapes_single_quotes(_Config) ->
    Input = "hello'world",
    Expected = "'hello'\\''world'",
    Result = r3lfe_rlwrap:shell_quote(Input),
    ?assertEqual(Expected, Result),
    ok.

shell_quote_handles_no_quotes(_Config) ->
    Input = "hello world",
    Expected = "'hello world'",
    Result = r3lfe_rlwrap:shell_quote(Input),
    ?assertEqual(Expected, Result),
    ok.

get_history_file_default_location(_Config) ->
    Opts = #{},
    Result = r3lfe_rlwrap:get_history_file(Opts),

    %% Should contain .lfe/history
    ?assert(string:str(Result, ".lfe/history") > 0),
    ok.

get_history_file_custom_location(_Config) ->
    CustomPath = "/tmp/my_history",
    Opts = #{history_file => CustomPath},
    Result = r3lfe_rlwrap:get_history_file(Opts),

    ?assertEqual(CustomPath, Result),
    ok.

get_history_file_expands_tilde(_Config) ->
    Opts = #{history_file => "~/custom/.lfe_history"},
    Result = r3lfe_rlwrap:get_history_file(Opts),

    %% Should not contain tilde
    ?assertEqual(false, string:str(Result, "~") > 0),
    %% Should contain HOME
    Home = os:getenv("HOME", "/tmp"),
    ?assert(string:str(Result, Home) > 0),
    ok.

get_completion_files_returns_list(_Config) ->
    Opts = #{},
    Result = r3lfe_rlwrap:get_completion_files(Opts),

    ?assert(is_list(Result)),
    ?assert(length(Result) >= 2),  % At least erlang.txt and lfe.txt
    ok.

should_use_rlwrap_default_true(_Config) ->
    Opts = #{},
    Result = r3lfe_rlwrap:should_use_rlwrap(Opts),

    ?assertEqual(true, Result),
    ok.

should_use_rlwrap_respects_config(_Config) ->
    Opts = #{use_rlwrap => false},
    Result = r3lfe_rlwrap:should_use_rlwrap(Opts),

    ?assertEqual(false, Result),
    ok.

should_use_rlwrap_respects_no_rlwrap_flag(_Config) ->
    Opts = #{no_rlwrap => true},
    Result = r3lfe_rlwrap:should_use_rlwrap(Opts),

    ?assertEqual(false, Result),
    ok.

build_rlwrap_command_generates_valid_string(_Config) ->
    %% Mock state - we just need something that won't crash
    State = rebar_state:new(),
    Opts = #{},

    Result = r3lfe_rlwrap:build_rlwrap_command(State, Opts),

    %% Should start with "rlwrap"
    ?assert(string:str(Result, "rlwrap") =:= 1),
    %% Should contain rebar3
    ?assert(string:str(Result, "rebar3") > 0),
    %% Should contain the active flag
    ?assert(string:str(Result, "--rlwrap-active") > 0),
    ok.

expand_home_with_tilde_slash(_Config) ->
    Input = "~/test/path",
    Result = r3lfe_rlwrap:expand_home(Input),

    %% Should not contain tilde
    ?assertEqual(false, string:str(Result, "~") > 0),
    %% Should start with HOME
    Home = os:getenv("HOME", "/tmp"),
    ?assert(string:str(Result, Home) =:= 1),
    ok.

expand_home_without_tilde(_Config) ->
    Input = "/absolute/path",
    Result = r3lfe_rlwrap:expand_home(Input),

    %% Should be unchanged
    ?assertEqual(Input, Result),
    ok.
