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
    get_tool_versions_includes_rebar3_lfe/1,
    get_version_existing_app/1,
    get_version_missing_app/1,
    lfe_version_not_unknown/1,
    get_rebar3_version_returns_string/1,
    get_deps_info_filters_lfe/1,
    get_deps_info_sorts_alphabetically/1,
    get_plugins_info_filters_build_tools/1,
    get_plugins_info_sorts_alphabetically/1,
    format_heading_correct_length/1,
    format_heading_centered_text/1,
    format_heading_all_same_length/1,
    format_heading_has_spaces/1,
    display_versions_output/1,
    display_versions_empty_sections/1,
    display_versions_with_deps_and_plugins/1,
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
        get_tool_versions_includes_rebar3_lfe,
        get_version_existing_app,
        get_version_missing_app,
        lfe_version_not_unknown,
        get_rebar3_version_returns_string,
        get_deps_info_filters_lfe,
        get_deps_info_sorts_alphabetically,
        get_plugins_info_filters_build_tools,
        get_plugins_info_sorts_alphabetically,
        format_heading_correct_length,
        format_heading_centered_text,
        format_heading_all_same_length,
        format_heading_has_spaces,
        display_versions_output,
        display_versions_empty_sections,
        display_versions_with_deps_and_plugins,
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

    %% Should have at least rebar3 and rebar3_lfe
    ?assert(length(Result) >= 2),

    %% Check that it has rebar3 and rebar3_lfe
    Keys = [K || {K, _} <- Result],
    ?assert(lists:member(rebar3, Keys)),
    ?assert(lists:member(rebar3_lfe, Keys)),

    %% Check that all values are strings
    lists:foreach(
        fun({_Name, Vsn}) ->
            ?assert(is_list(Vsn))
        end,
        Result
    ),

    ok.

get_tool_versions_includes_rebar3_lfe(_Config) ->
    %% Test that rebar3_lfe (not r3lfe) is in the result
    Result = r3lfe_prv_versions:get_tool_versions(),

    Keys = [K || {K, _} <- Result],
    ?assert(lists:member(rebar3_lfe, Keys)),
    ?assertNot(lists:member(r3lfe, Keys)),

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

lfe_version_not_unknown(_Config) ->
    %% Test that LFE version is detected (not "unknown")
    Result = r3lfe_prv_versions:get_version(lfe),

    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),
    ?assertNotEqual("unknown", Result),

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
%% Test get_deps_info/1
%%====================================================================

get_deps_info_filters_lfe(_Config) ->
    %% Test that LFE is filtered from dependencies
    State = rebar_state:new(),

    %% Get the result
    Result = r3lfe_prv_versions:get_deps_info(State),

    %% Check that LFE is not in the result
    Names = [maps:get(name, D) || D <- Result],
    ?assertNot(lists:member(lfe, Names)),

    ok.

get_deps_info_sorts_alphabetically(_Config) ->
    %% Test that dependencies are sorted alphabetically
    State = rebar_state:new(),

    Result = r3lfe_prv_versions:get_deps_info(State),

    %% Get the names
    Names = [maps:get(name, D) || D <- Result],

    %% Check that names are sorted
    ?assertEqual(Names, lists:sort(Names)),

    ok.

%%====================================================================
%% Test get_plugins_info/1
%%====================================================================

get_plugins_info_filters_build_tools(_Config) ->
    %% Test that rebar3_lfe and rebar3_hex are filtered from plugins
    State = rebar_state:new(),

    Result = r3lfe_prv_versions:get_plugins_info(State),

    %% Check that build tools are not in the result
    Names = [maps:get(name, P) || P <- Result],
    ?assertNot(lists:member(rebar3_lfe, Names)),
    ?assertNot(lists:member(rebar3_hex, Names)),

    ok.

get_plugins_info_sorts_alphabetically(_Config) ->
    %% Test that plugins are sorted alphabetically
    State = rebar_state:new(),

    Result = r3lfe_prv_versions:get_plugins_info(State),

    %% Get the names
    Names = [maps:get(name, P) || P <- Result],

    %% Check that names are sorted
    ?assertEqual(Names, lists:sort(Names)),

    ok.

%%====================================================================
%% Test format_heading/1
%%====================================================================

format_heading_correct_length(_Config) ->
    %% Test that all headings have the same length (28 chars)
    Headings = [
        r3lfe_prv_versions:format_heading("Languages"),
        r3lfe_prv_versions:format_heading("Build Tools"),
        r3lfe_prv_versions:format_heading("Dependencies"),
        r3lfe_prv_versions:format_heading("Plugins"),
        r3lfe_prv_versions:format_heading("Project Applications")
    ],

    %% All should be 28 characters
    lists:foreach(
        fun(Heading) ->
            ?assertEqual(28, length(Heading))
        end,
        Headings
    ),

    ok.

format_heading_centered_text(_Config) ->
    %% Test that text is centered with equal padding
    Heading = r3lfe_prv_versions:format_heading("Test"),

    %% Should be 28 characters total
    ?assertEqual(28, length(Heading)),
    ?assert(string:str(Heading, " Test ") > 0),

    %% Count equals signs before and after text
    [Before, After] = string:split(Heading, " Test ", all),
    BeforeEquals = length([C || C <- Before, C =:= $=]),
    AfterEquals = length([C || C <- After, C =:= $=]),

    %% Should be equal or differ by 1 (for odd-length text)
    ?assert(abs(BeforeEquals - AfterEquals) =< 1),

    ok.

format_heading_all_same_length(_Config) ->
    %% Test various heading texts all produce same length
    TestHeadings = ["A", "AB", "ABC", "ABCDEFGHIJ", "Short", "Very Long Heading"],

    Lengths = [length(r3lfe_prv_versions:format_heading(H)) || H <- TestHeadings],

    %% All should be 28
    lists:foreach(
        fun(Len) ->
            ?assertEqual(28, Len)
        end,
        Lengths
    ),

    ok.

format_heading_has_spaces(_Config) ->
    %% Test that heading has spaces around text
    Heading = r3lfe_prv_versions:format_heading("Languages"),

    %% Should start with "===" and end with "===" (28 chars total, so last 3 start at position 25)
    ?assertEqual("===", string:slice(Heading, 0, 3)),
    ?assertEqual("===", string:slice(Heading, 25, 3)),

    %% Should have space before and after text
    ?assert(string:str(Heading, " Languages ") > 0),

    ok.

%%====================================================================
%% Test display_versions/1
%%====================================================================

display_versions_output(_Config) ->
    %% Test that display_versions doesn't crash
    VersionInfo = #{
        apps => [{testapp, "1.0.0"}],
        languages => [{lfe, "2.1.3"}, {erlang, "26"}, {erts, "14.2"}],
        tools => [{rebar3, "3.22.0"}, {rebar3_lfe, "0.4.8"}],
        deps => [],
        plugins => []
    },

    %% Should not crash
    ok = r3lfe_prv_versions:display_versions(VersionInfo),

    ok.

display_versions_empty_sections(_Config) ->
    %% Test that empty sections don't display headers
    VersionInfo = #{
        apps => [],
        languages => [{lfe, "2.1.3"}, {erlang, "26"}, {erts, "14.2"}],
        tools => [{rebar3, "3.22.0"}, {rebar3_lfe, "0.4.8"}],
        deps => [],
        plugins => []
    },

    %% Should not crash
    ok = r3lfe_prv_versions:display_versions(VersionInfo),

    ok.

display_versions_with_deps_and_plugins(_Config) ->
    %% Test display with dependencies and plugins
    VersionInfo = #{
        apps => [{myapp, "1.0.0"}],
        languages => [{lfe, "2.1.3"}, {erlang, "26"}, {erts, "14.2"}],
        tools => [{rebar3, "3.22.0"}, {rebar3_lfe, "0.4.8"}],
        deps => [
            #{name => cowboy, version => "2.9.0", profile => default},
            #{name => jsx, version => "3.1.0", profile => default},
            #{name => meck, version => "0.9.2", profile => test}
        ],
        plugins => [
            #{name => rebar3_format, version => "1.3.0", profile => default},
            #{name => rebar3_proper, version => "0.12.1", profile => test}
        ]
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
