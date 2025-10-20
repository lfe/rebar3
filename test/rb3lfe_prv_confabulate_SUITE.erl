-module(rb3lfe_prv_confabulate_SUITE).

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
    confabulate_provider_registers/1,
    confabulate_simple_data/1,
    confabulate_complex_data/1,
    confabulate_with_output_option/1,
    confabulate_force_overwrite/1,
    confabulate_no_input_error/1,
    confabulate_input_not_found_error/1,
    confabulate_output_exists_error/1,
    confabulate_multiple_forms/1,
    confabulate_nested_structures/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        confabulate_provider_registers,
        confabulate_simple_data,
        confabulate_complex_data,
        confabulate_with_output_option,
        confabulate_force_overwrite,
        confabulate_no_input_error,
        confabulate_input_not_found_error,
        confabulate_output_exists_error,
        confabulate_multiple_forms,
        confabulate_nested_structures
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TestDir = test_utils:create_temp_dir("confabulate"),
    [{test_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    TestDir = ?config(test_dir, Config),
    test_utils:cleanup_temp_dir(TestDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

confabulate_provider_registers(_Config) ->
    State = rebar_state:new(),

    {ok, State1} = rb3lfe_prv_confabulate:init(State),

    Providers = rebar_state:providers(State1),

    %% Should have at least one provider
    ?assert(length(Providers) > 0, "Confabulate provider should be registered"),

    ok.

confabulate_simple_data(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create simple LFE data file
    InputFile = filename:join(TestDir, "simple.lfe"),
    test_utils:write_file(InputFile,
        "(#(person \"Alice\" 30)\n"
        " #(person \"Bob\" 25))\n"
    ),

    %% Run confabulate
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Check output file exists
    OutputFile = filename:join(TestDir, "simple.erl"),
    ?assert(filelib:is_file(OutputFile)),

    %% Verify content
    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    ?assert(string:find(ContentStr, "{person,") =/= nomatch),
    ?assert(string:find(ContentStr, "Alice") =/= nomatch),
    ?assert(string:find(ContentStr, "Bob") =/= nomatch),

    ok.

confabulate_complex_data(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create complex LFE data
    InputFile = filename:join(TestDir, "complex.lfe"),
    test_utils:write_file(InputFile,
        "(#(name \"Project\")\n"
        " #(version \"1.0.0\")\n"
        " #(deps (#(lfe \"2.2.0\")\n"
        "         #(cowboy \"2.9.0\"))))\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Verify output
    OutputFile = filename:join(TestDir, "complex.erl"),
    ?assert(filelib:is_file(OutputFile)),

    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    ?assert(string:find(ContentStr, "name") =/= nomatch),
    ?assert(string:find(ContentStr, "deps") =/= nomatch),

    ok.

confabulate_with_output_option(Config) ->
    TestDir = ?config(test_dir, Config),

    InputFile = filename:join(TestDir, "data.lfe"),
    OutputFile = filename:join(TestDir, "custom.erl"),

    test_utils:write_file(InputFile, "(#(test data))\n"),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {
        [{input, InputFile}, {output, OutputFile}], []
    }),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Should use custom output name
    ?assert(filelib:is_file(OutputFile)),
    ?assertNot(filelib:is_file(filename:join(TestDir, "data.erl"))),

    ok.

confabulate_force_overwrite(Config) ->
    TestDir = ?config(test_dir, Config),

    InputFile = filename:join(TestDir, "data.lfe"),
    OutputFile = filename:join(TestDir, "data.erl"),

    test_utils:write_file(InputFile, "(#(new data))\n"),

    %% Create existing output file
    test_utils:write_file(OutputFile, "{old, data}.\n"),

    %% Without force, should error
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result1 = rb3lfe_prv_confabulate:do(State1),
    ?assertMatch({error, _}, Result1),

    %% With force, should succeed
    State2 = rebar_state:command_parsed_args(State, {
        [{input, InputFile}, {force, true}], []
    }),

    Result2 = rb3lfe_prv_confabulate:do(State2),
    ?assertMatch({ok, _}, Result2),

    %% Verify new content
    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    ?assert(string:find(ContentStr, "new") =/= nomatch),
    ?assert(string:find(ContentStr, "old") =:= nomatch),

    ok.

confabulate_no_input_error(_Config) ->
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({error, _}, Result),

    {error, ErrorMsg} = Result,
    ?assert(string:find(ErrorMsg, "No input") =/= nomatch),

    ok.

confabulate_input_not_found_error(Config) ->
    TestDir = ?config(test_dir, Config),

    InputFile = filename:join(TestDir, "nonexistent.lfe"),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({error, _}, Result),

    {error, ErrorMsg} = Result,
    ?assert(string:find(ErrorMsg, "not found") =/= nomatch),

    ok.

confabulate_output_exists_error(Config) ->
    TestDir = ?config(test_dir, Config),

    InputFile = filename:join(TestDir, "data.lfe"),
    OutputFile = filename:join(TestDir, "data.erl"),

    test_utils:write_file(InputFile, "(#(test))\n"),
    test_utils:write_file(OutputFile, "{existing}.\n"),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({error, _}, Result),

    {error, ErrorMsg} = Result,
    ?assert(string:find(ErrorMsg, "already exists") =/= nomatch),

    ok.

confabulate_multiple_forms(Config) ->
    TestDir = ?config(test_dir, Config),

    InputFile = filename:join(TestDir, "multiple.lfe"),
    test_utils:write_file(InputFile,
        "(#(type library)\n"
        " #(name \"mylib\")\n"
        " #(version \"1.0.0\")\n"
        " #(author \"Alice\")\n"
        " #(license apache2))\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    OutputFile = filename:join(TestDir, "multiple.erl"),
    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    %% Verify we have output
    ?assert(byte_size(Content) > 0),

    %% Should contain all our keys
    ?assert(string:find(ContentStr, "type") =/= nomatch),
    ?assert(string:find(ContentStr, "name") =/= nomatch),
    ?assert(string:find(ContentStr, "version") =/= nomatch),
    ?assert(string:find(ContentStr, "author") =/= nomatch),
    ?assert(string:find(ContentStr, "license") =/= nomatch),

    ok.

confabulate_nested_structures(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Simplified test - just verify tuples work
    InputFile = filename:join(TestDir, "nested.lfe"),
    test_utils:write_file(InputFile,
        "(#(config database logging)\n"
        " #(settings host port))\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    OutputFile = filename:join(TestDir, "nested.erl"),
    ?assert(filelib:is_file(OutputFile)),

    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    %% Should contain the atoms
    ?assert(string:find(ContentStr, "config") =/= nomatch),
    ?assert(string:find(ContentStr, "database") =/= nomatch),
    ?assert(string:find(ContentStr, "settings") =/= nomatch),

    ok.
