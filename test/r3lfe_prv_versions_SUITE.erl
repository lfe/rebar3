-module(r3lfe_prv_versions_SUITE).

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
    versions_provider_registers/1,
    versions_do_returns_ok/1,
    format_error_generic/1,
    get_app_versions_single_app/1,
    get_app_versions_multiple_apps/1,
    get_app_versions_binary_name/1,
    get_language_versions_structure/1,
    get_tool_versions_structure/1,
    get_version_existing_app/1,
    get_version_missing_app/1,
    get_rebar3_version_returns_string/1,
    display_versions_output/1,
    info_output_validation/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        versions_provider_registers,
        versions_do_returns_ok,
        format_error_generic,
        get_app_versions_single_app,
        get_app_versions_multiple_apps,
        get_app_versions_binary_name,
        get_language_versions_structure,
        get_tool_versions_structure,
        get_version_existing_app,
        get_version_missing_app,
        get_rebar3_version_returns_string,
        display_versions_output,
        info_output_validation
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

versions_provider_registers(_Config) ->
    %% Test that provider registers correctly
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_versions:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Versions provider should be registered"),

    ok.

versions_do_returns_ok(_Config) ->
    %% Test that do/1 returns ok
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_versions:do(State),

    ?assert(is_tuple(State1)),

    ok.

format_error_generic(_Config) ->
    %% Test format_error with various reasons
    Reasons = [
        unknown_error,
        {error, something},
        test_reason
    ],

    lists:foreach(
        fun(Reason) ->
            Msg = r3lfe_prv_versions:format_error(Reason),
            ?assert(is_list(Msg)),
            ?assert(length(Msg) > 0)
        end,
        Reasons
    ),

    ok.

%%====================================================================
%% Test get_app_versions/1
%%====================================================================

get_app_versions_single_app(_Config) ->
    %% Create a mock app
    {ok, AppInfo} = rebar_app_info:new(testapp, "1.0.0", "/tmp"),

    Result = r3lfe_prv_versions:get_app_versions([AppInfo]),

    ?assertMatch([{testapp, "1.0.0"}], Result),

    ok.

get_app_versions_multiple_apps(_Config) ->
    %% Create multiple mock apps
    {ok, App1} = rebar_app_info:new(app1, "1.0.0", "/tmp"),
    {ok, App2} = rebar_app_info:new(app2, "2.0.0", "/tmp"),

    Result = r3lfe_prv_versions:get_app_versions([App1, App2]),

    ?assertEqual(2, length(Result)),
    ?assert(lists:member({app1, "1.0.0"}, Result)),
    ?assert(lists:member({app2, "2.0.0"}, Result)),

    ok.

get_app_versions_binary_name(_Config) ->
    %% Test with binary app name
    {ok, AppInfo} = rebar_app_info:new(<<"binaryapp">>, "3.0.0", "/tmp"),

    Result = r3lfe_prv_versions:get_app_versions([AppInfo]),

    ?assertMatch([{binaryapp, "3.0.0"}], Result),

    ok.

%%====================================================================
%% Test get_language_versions/0
%%====================================================================

get_language_versions_structure(_Config) ->
    %% Test that get_language_versions returns proper structure
    Result = r3lfe_prv_versions:get_language_versions(),

    ?assertEqual(3, length(Result)),

    %% Check that it has lfe, erlang, and erts
    Keys = [K || {K, _} <- Result],
    ?assert(lists:member(lfe, Keys)),
    ?assert(lists:member(erlang, Keys)),
    ?assert(lists:member(erts, Keys)),

    %% Check that all values are strings
    lists:foreach(
        fun({_Name, Vsn}) ->
            ?assert(is_list(Vsn))
        end,
        Result
    ),

    ok.

%%====================================================================
%% Test get_tool_versions/0
%%====================================================================

get_tool_versions_structure(_Config) ->
    %% Test that get_tool_versions returns proper structure
    Result = r3lfe_prv_versions:get_tool_versions(),

    ?assertEqual(2, length(Result)),

    %% Check that it has rebar3 and r3lfe
    Keys = [K || {K, _} <- Result],
    ?assert(lists:member(rebar3, Keys)),
    ?assert(lists:member(r3lfe, Keys)),

    %% Check that all values are strings
    lists:foreach(
        fun({_Name, Vsn}) ->
            ?assert(is_list(Vsn))
        end,
        Result
    ),

    ok.

%%====================================================================
%% Test get_version/1
%%====================================================================

get_version_existing_app(_Config) ->
    %% Test with an existing application (stdlib should always exist)
    Result = r3lfe_prv_versions:get_version(stdlib),

    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),
    ?assertNotEqual("unknown", Result),

    ok.

get_version_missing_app(_Config) ->
    %% Test with a non-existent application
    Result = r3lfe_prv_versions:get_version(nonexistent_app_xyz),

    ?assertEqual("unknown", Result),

    ok.

%%====================================================================
%% Test get_rebar3_version/0
%%====================================================================

get_rebar3_version_returns_string(_Config) ->
    %% Test that get_rebar3_version returns a string
    Result = r3lfe_prv_versions:get_rebar3_version(),

    ?assert(is_list(Result)),

    ok.

%%====================================================================
%% Test display_versions/1
%%====================================================================

display_versions_output(_Config) ->
    %% Test that display_versions doesn't crash
    VersionInfo = #{
        apps => [{testapp, "1.0.0"}],
        languages => [{lfe, "2.1.3"}, {erlang, "26"}, {erts, "14.2"}],
        tools => [{rebar3, "3.22.0"}, {r3lfe, "0.4.8"}]
    },

    %% Should not crash
    ok = r3lfe_prv_versions:display_versions(VersionInfo),

    ok.

%%====================================================================
%% Test info/1
%%====================================================================

info_output_validation(_Config) ->
    %% Test info function output
    Result = r3lfe_prv_versions:info("Display versions"),

    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),

    %% Should contain key information
    Flat = lists:flatten(Result),
    ?assert(string:str(Flat, "version") > 0),

    ok.
