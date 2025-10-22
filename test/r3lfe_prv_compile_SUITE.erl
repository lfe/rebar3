-module(r3lfe_prv_compile_SUITE).

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
    compile_provider_init/1,
    compile_format_error_variants/1,
    compile_info_output/1,
    compile_get_dep_include_dirs/1,
    compile_provider_metadata/1,
    compile_format_error_with_counts/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        compile_provider_init,
        compile_format_error_variants,
        compile_info_output,
        compile_get_dep_include_dirs,
        compile_provider_metadata,
        compile_format_error_with_counts
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

compile_provider_init(_Config) ->
    %% Test that provider initializes correctly
    State = rebar_state:new(),

    {ok, State1} = r3lfe_prv_compile:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have registered the compile provider
    ?assert(length(Providers) > 0),

    ok.

compile_format_error_variants(_Config) ->
    %% Test all format_error clauses
    Errors = [
        {compilation_failed, 5},
        {compilation_error, "some reason"},
        {package_error, "package issue"},
        some_other_error
    ],

    lists:foreach(
        fun(Error) ->
            Result = r3lfe_prv_compile:format_error(Error),
            ?assert(is_list(Result)),
            ?assert(length(Result) > 0)
        end,
        Errors
    ),

    ok.

compile_info_output(_Config) ->
    %% Test that info/1 returns proper documentation
    Info = r3lfe_prv_compile:info("Test compile description"),

    ?assert(is_list(Info)),
    ?assert(length(Info) > 0),

    %% Should contain key documentation elements
    InfoStr = lists:flatten(Info),
    ?assert(string:find(InfoStr, "Test compile description") =/= nomatch),
    ?assert(string:find(InfoStr, "LFE") =/= nomatch orelse
            string:find(InfoStr, "compile") =/= nomatch),

    ok.

compile_get_dep_include_dirs(_Config) ->
    %% Test get_dep_include_dirs with a minimal state
    State = rebar_state:new(),

    %% Should return a list (even if empty)
    Result = r3lfe_prv_compile:get_dep_include_dirs(State),

    ?assert(is_list(Result)),

    ok.

compile_provider_metadata(_Config) ->
    %% Test provider metadata is correctly set
    State = rebar_state:new(),
    {ok, State1} = r3lfe_prv_compile:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider registered
    ?assert(length(Providers) > 0),

    %% The provider should be a valid record
    ?assert(is_tuple(hd(Providers))),

    ok.

compile_format_error_with_counts(_Config) ->
    %% Test format_error with specific count values
    Error1 = r3lfe_prv_compile:format_error({compilation_failed, 1}),
    ?assert(string:find(Error1, "1") =/= nomatch),

    Error5 = r3lfe_prv_compile:format_error({compilation_failed, 5}),
    ?assert(string:find(Error5, "5") =/= nomatch),

    Error10 = r3lfe_prv_compile:format_error({compilation_failed, 10}),
    ?assert(string:find(Error10, "10") =/= nomatch),

    ok.
