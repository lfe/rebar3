-module(confabulate_integration_SUITE).

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
    convert_erlang_config/1,
    convert_user_records/1,
    roundtrip_via_defabulate/1,
    batch_conversion_workflow/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        convert_erlang_config,
        convert_user_records,
        roundtrip_via_defabulate,
        batch_conversion_workflow
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("confab_integ"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

convert_erlang_config(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Write an Erlang config file
    ErlFile = filename:join(TestDir, "app.config"),
    test_utils:write_file(ErlFile,
        "{myapp, [{port, 8080}, {host, \"localhost\"}]}.\n"
        "{sasl, false}.\n"
        "{lager, info}.\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {
        [{input, ErlFile}, {output, filename:join(TestDir, "app.config.lfe")}],
        []
    }),

    Result = r3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    OutputFile = filename:join(TestDir, "app.config.lfe"),
    ?assert(filelib:is_file(OutputFile)),

    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    ?assert(string:find(ContentStr, "myapp") =/= nomatch),
    ?assert(string:find(ContentStr, "port") =/= nomatch),

    ct:pal("Successfully converted Erlang config to LFE"),

    ok.

convert_user_records(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Write user records as Erlang
    ErlFile = filename:join(TestDir, "users.erl"),
    test_utils:write_file(ErlFile,
        "{user, \"alice\", \"alice@example.com\", admin}.\n"
        "{user, \"bob\", \"bob@example.com\", user}.\n"
        "{user, \"charlie\", \"charlie@example.com\", user}.\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, ErlFile}], []}),

    Result = r3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    OutputFile = filename:join(TestDir, "users.lfe"),
    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    %% Should have 3 LFE tuples
    Matches = string:split(ContentStr, "#(user", all),
    ?assertEqual(4, length(Matches), "Should have 3 user tuples (4 splits)"),

    %% Check atoms (lfe_io:print1 renders char-list strings as integer lists)
    ?assert(string:find(ContentStr, "admin") =/= nomatch),
    ?assert(string:find(ContentStr, "user") =/= nomatch),

    ct:pal("Converted ~p user records to LFE", [length(Matches) - 1]),

    ok.

roundtrip_via_defabulate(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Original Erlang terms
    OriginalData = [
        {person, "Alice", 30, developer},
        {person, "Bob", 25, designer},
        {company, "ACME Corp", [employee1, employee2]}
    ],

    %% Write as Erlang
    ErlFile = filename:join(TestDir, "original.erl"),
    lists:foreach(
        fun(Term) ->
            FormattedTerm = io_lib:format("~p.~n", [Term]),
            file:write_file(ErlFile, FormattedTerm, [append])
        end,
        OriginalData
    ),

    %% Confabulate: Erlang → LFE
    LfeFile = filename:join(TestDir, "original.lfe"),
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {
        [{input, ErlFile}, {output, LfeFile}], []
    }),
    ?assertMatch({ok, _}, r3lfe_prv_confabulate:do(State1)),

    %% Defabulate: LFE → Erlang
    RtFile = filename:join(TestDir, "roundtrip.erl"),
    State2 = rebar_state:command_parsed_args(State, {
        [{input, LfeFile}, {output, RtFile}], []
    }),
    ?assertMatch({ok, _}, r3lfe_prv_defabulate:do(State2)),

    %% Read back and verify
    {ok, RoundTripped} = file:consult(RtFile),

    ?assertEqual(OriginalData, RoundTripped),

    ct:pal("Roundtrip successful: Erlang -> LFE -> Erlang, data preserved"),

    ok.

batch_conversion_workflow(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Simulate converting multiple Erlang files
    Files = [
        {"users.erl", "{user, alice, 30}.\n"},
        {"products.erl", "{product, widget, 9.99}.\n"},
        {"orders.erl", "{order, 1001, alice, widget}.\n"}
    ],

    %% Create files
    lists:foreach(
        fun({Name, Content}) ->
            File = filename:join(TestDir, Name),
            test_utils:write_file(File, Content)
        end,
        Files
    ),

    %% Convert each
    State = rebar_state:new(),

    ConvertFile = fun(Name) ->
        InputFile = filename:join(TestDir, Name),
        State1 = rebar_state:command_parsed_args(State, {
            [{input, InputFile}], []
        }),
        r3lfe_prv_confabulate:do(State1)
    end,

    Results = [ConvertFile(Name) || {Name, _} <- Files],

    %% All should succeed
    ?assert(lists:all(fun(R) -> element(1, R) =:= ok end, Results)),

    %% Verify .lfe outputs exist
    lists:foreach(
        fun({Name, _}) ->
            OutputName = filename:rootname(Name, ".erl") ++ ".lfe",
            OutputFile = filename:join(TestDir, OutputName),
            ?assert(filelib:is_file(OutputFile))
        end,
        Files
    ),

    ct:pal("Batch conversion successful: ~p files", [length(Files)]),

    ok.
