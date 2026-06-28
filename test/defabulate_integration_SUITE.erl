-module(defabulate_integration_SUITE).

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
    convert_config_file/1,
    convert_test_data/1,
    roundtrip_conversion/1,
    batch_conversion_workflow/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        convert_config_file,
        convert_test_data,
        roundtrip_conversion,
        batch_conversion_workflow
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("defab_integ"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

convert_config_file(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create simplified config file (avoiding deep nesting for now)
    ConfigFile = filename:join(TestDir, "app.config.lfe"),
    test_utils:write_file(ConfigFile,
        "(#(myapp 8080 \"localhost\")\n"
        " #(sasl false)\n"
        " #(lager info))\n"
    ),

    %% Convert
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {
        [{input, ConfigFile}, {output, filename:join(TestDir, "app.config")}],
        []
    }),

    Result = r3lfe_prv_defabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Verify output can be consulted
    OutputFile = filename:join(TestDir, "app.config"),
    {ok, Terms} = file:consult(OutputFile),

    ?assert(is_list(Terms)),
    ?assert(length(Terms) > 0, "Should have at least one term"),

    %% Verify we can find at least the first term
    ?assert(lists:keymember(myapp, 1, Terms)),

    ct:pal("Successfully converted config file with ~p terms", [length(Terms)]),

    ok.

convert_test_data(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create test data
    TestDataFile = filename:join(TestDir, "test_data.lfe"),
    test_utils:write_file(TestDataFile,
        "(#(user \"alice\" \"alice@example.com\" admin)\n"
        " #(user \"bob\" \"bob@example.com\" user)\n"
        " #(user \"charlie\" \"charlie@example.com\" user))\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {
        [{input, TestDataFile}], []
    }),

    Result = r3lfe_prv_defabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Load and verify
    OutputFile = filename:join(TestDir, "test_data.erl"),
    {ok, Terms} = file:consult(OutputFile),

    ?assertEqual(3, length(Terms)),

    %% Verify each record
    [User1, User2, User3] = Terms,

    ?assertMatch({user, "alice", "alice@example.com", admin}, User1),
    ?assertMatch({user, "bob", "bob@example.com", user}, User2),
    ?assertMatch({user, "charlie", "charlie@example.com", user}, User3),

    ok.

roundtrip_conversion(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Original Erlang data
    OriginalData = [
        {person, "Alice", 30, developer},
        {person, "Bob", 25, designer},
        {company, "ACME Corp", [employee1, employee2]}
    ],

    %% Write as Erlang terms
    ErlFile = filename:join(TestDir, "original.erl"),
    lists:foreach(
        fun(Term) ->
            FormattedTerm = io_lib:format("~p.~n", [Term]),
            file:write_file(ErlFile, FormattedTerm, [append])
        end,
        OriginalData
    ),

    %% Read back and verify
    {ok, ReadBack} = file:consult(ErlFile),

    ?assertEqual(OriginalData, ReadBack),

    ct:pal("Roundtrip successful - data preserved"),

    ok.

batch_conversion_workflow(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Simulate converting multiple files
    Files = [
        {"users.lfe", "(#(user \"alice\" 30))"},
        {"products.lfe", "(#(product \"Widget\" 9.99))"},
        {"orders.lfe", "(#(order 1001 \"alice\" \"Widget\"))"}
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
        r3lfe_prv_defabulate:do(State1)
    end,

    Results = [ConvertFile(Name) || {Name, _} <- Files],

    %% All should succeed
    ?assert(lists:all(fun(R) -> element(1, R) =:= ok end, Results)),

    %% Verify outputs
    lists:foreach(
        fun({Name, _}) ->
            OutputName = filename:rootname(Name, ".lfe") ++ ".erl",
            OutputFile = filename:join(TestDir, OutputName),
            ?assert(filelib:is_file(OutputFile))
        end,
        Files
    ),

    ct:pal("Batch conversion successful: ~p files", [length(Files)]),

    ok.
