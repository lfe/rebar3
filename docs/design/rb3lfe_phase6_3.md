# Phase 6.3: Data Conversion (Confabulate)

## Overview

This phase implements the `confabulate` provider for converting LFE data files to Erlang data files. This is useful for configuration files, data transformations, and interoperability between LFE and Erlang codebases.

**Goal**: Provide a reliable tool for converting LFE data structures to Erlang format.

## Prerequisites

- Phase 1-5 completed with all tests passing
- Phase 6.1 and 6.2 completed
- Understanding of LFE data syntax
- Knowledge of Erlang term format
- Familiarity with `lfe_io:parse_file/1`

## Architecture Overview

```
Confabulate Workflow:

1. Input: LFE data file
   example.lfe:
   ((tuple 'person "Alice" 30)
    (tuple 'person "Bob" 25))

2. Parse: lfe_io:parse_file/1
   → Extract LFE data structures

3. Convert: Format as Erlang terms
   → {person, "Alice", 30}.
   → {person, "Bob", 25}.

4. Output: Erlang data file
   example.erl:
   {person, "Alice", 30}.
   {person, "Bob", 25}.

Use Cases:
- Convert LFE config to Erlang format
- Generate Erlang test data from LFE
- Data exchange between LFE and Erlang
- Migration tools
```

## Implementation Tasks

### Task 6.3.1: Implement Confabulate Provider

**File: `src/rb3lfe_prv_confabulate.erl`**

```erlang
-module(rb3lfe_prv_confabulate).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, confabulate).
-define(NAMESPACE, lfe).
-define(DEPS, []).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Convert LFE data files to Erlang data files",

    Opts = [
        {input, $i, "input", string,
         "Input LFE file to convert"},
        {output, $o, "output", string,
         "Output Erlang file (defaults to input.erl)"},
        {force, $f, "force", boolean,
         "Overwrite output file if it exists"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe confabulate --input data.lfe"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE confabulate provider starting", []),

    rebar_paths:set_paths([deps, plugins], State),

    try
        %% Get options
        {Opts, _} = rebar_state:command_parsed_args(State),

        InputFile = proplists:get_value(input, Opts),
        OutputFile = determine_output_file(InputFile, Opts),
        Force = proplists:get_value(force, Opts, false),

        case InputFile of
            undefined ->
                {error, format_error(no_input_file)};
            _ ->
                %% Convert file
                ?INFO("Converting ~s to ~s", [InputFile, OutputFile]),

                case convert_file(InputFile, OutputFile, Force) of
                    ok ->
                        ?INFO("Conversion successful!", []),
                        {ok, State};
                    {error, Reason} ->
                        {error, format_error(Reason)}
                end
        end
    catch
        throw:{error, Reason} ->
            {error, format_error(Reason)};
        error:Reason:Stack ->
            ?ERROR("Confabulate failed: ~p", [Reason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({confabulate_error, Reason})}
    end.

-spec format_error(term()) -> iolist().
format_error(no_input_file) ->
    "No input file specified. Use --input option:\n"
    "  rebar3 lfe confabulate --input data.lfe";
format_error({input_not_found, File}) ->
    io_lib:format("Input file not found: ~s", [File]);
format_error({output_exists, File}) ->
    io_lib:format(
        "Output file already exists: ~s~n"
        "Use --force to overwrite",
        [File]
    );
format_error({parse_error, File, Reason}) ->
    io_lib:format("Failed to parse ~s: ~p", [File, Reason]);
format_error({write_error, File, Reason}) ->
    io_lib:format("Failed to write ~s: ~p", [File, Reason]);
format_error({confabulate_error, Reason}) ->
    io_lib:format("Conversion failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Determine output file path
-spec determine_output_file(file:filename() | undefined, proplists:proplist()) ->
    file:filename() | undefined.
determine_output_file(undefined, _Opts) ->
    undefined;
determine_output_file(InputFile, Opts) ->
    case proplists:get_value(output, Opts) of
        undefined ->
            %% Default: replace .lfe with .erl
            filename:rootname(InputFile, ".lfe") ++ ".erl";
        OutputFile ->
            OutputFile
    end.

%% @doc Convert a single LFE file to Erlang format
-spec convert_file(file:filename(), file:filename(), boolean()) ->
    ok | {error, term()}.
convert_file(InputFile, OutputFile, Force) ->
    %% Validate input exists
    case filelib:is_file(InputFile) of
        false ->
            {error, {input_not_found, InputFile}};
        true ->
            %% Check output doesn't exist (unless force)
            case filelib:is_file(OutputFile) andalso not Force of
                true ->
                    {error, {output_exists, OutputFile}};
                false ->
                    do_conversion(InputFile, OutputFile)
            end
    end.

%% @doc Perform the actual conversion
-spec do_conversion(file:filename(), file:filename()) -> ok | {error, term()}.
do_conversion(InputFile, OutputFile) ->
    ?DEBUG("Parsing LFE file: ~s", [InputFile]),

    %% Parse LFE file
    case lfe_io:parse_file(InputFile) of
        {ok, Forms} ->
            ?DEBUG("Parsed ~p forms", [length(Forms)]),

            %% Convert and write
            write_erlang_file(OutputFile, Forms);

        {error, Reason} ->
            {error, {parse_error, InputFile, Reason}}
    end.

%% @doc Write forms as Erlang data
-spec write_erlang_file(file:filename(), [term()]) -> ok | {error, term()}.
write_erlang_file(OutputFile, Forms) ->
    try
        %% Delete existing file if present
        file:delete(OutputFile),

        %% Write each form
        lists:foreach(
            fun(Form) ->
                ok = append_form(OutputFile, Form)
            end,
            Forms
        ),

        ok
    catch
        error:Reason ->
            {error, {write_error, OutputFile, Reason}}
    end.

%% @doc Append a single form to the output file
-spec append_form(file:filename(), term()) -> ok.
append_form(OutputFile, {Form, _Line}) ->
    append_form(OutputFile, Form);
append_form(OutputFile, Form) ->
    %% Format as Erlang term
    FormattedTerm = io_lib:format("~p.~n", [Form]),

    %% Append to file
    case file:write_file(OutputFile, FormattedTerm, [append]) of
        ok ->
            ok;
        {error, Reason} ->
            throw({error, {write_error, OutputFile, Reason}})
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Converts LFE data files to Erlang data files. This is useful for:~n"
        "  - Converting LFE configuration to Erlang format~n"
        "  - Generating Erlang test data from LFE~n"
        "  - Data exchange between LFE and Erlang codebases~n"
        "  - Migration between languages~n"
        "~n"
        "The conversion parses LFE data structures and writes them as~n"
        "Erlang terms, one per line.~n"
        "~n"
        "Input Format (LFE):~n"
        "  ;; data.lfe~n"
        "  ((tuple 'person \"Alice\" 30)~n"
        "   (tuple 'person \"Bob\" 25))~n"
        "~n"
        "Output Format (Erlang):~n"
        "  %% data.erl~n"
        "  {person,\"Alice\",30}.~n"
        "  {person,\"Bob\",25}.~n"
        "~n"
        "Options:~n"
        "  --input FILE    Input LFE file (required)~n"
        "  --output FILE   Output Erlang file (default: input.erl)~n"
        "  --force         Overwrite existing output file~n"
        "~n"
        "Examples:~n"
        "  rebar3 lfe confabulate --input data.lfe~n"
        "  rebar3 lfe confabulate --input data.lfe --output config.erl~n"
        "  rebar3 lfe confabulate -i data.lfe -o config.erl --force~n"
        "~n"
        "Note: Only data files are supported, not code modules.~n"
        "The input should contain LFE data structures, not function~n"
        "definitions or module declarations.~n",
        [Description]
    ).
```

### Task 6.3.2: Register Confabulate Provider

**File: `src/rb3lfe.erl` (UPDATE)**

```erlang
%% Update the Providers list in init/1:

Providers = [
    rb3lfe_prv_compile,
    rb3lfe_prv_clean,
    rb3lfe_prv_repl,
    rb3lfe_prv_ltest,
    rb3lfe_prv_release,
    rb3lfe_prv_versions,
    %% Phase 6.1: Escript providers
    rb3lfe_prv_run,
    rb3lfe_prv_escriptize,
    rb3lfe_prv_run_escript,
    %% Phase 6.2: Release management
    rb3lfe_prv_run_release,
    %% Phase 6.3: Data conversion
    rb3lfe_prv_confabulate
],
```

### Task 6.3.3: Create Confabulate Tests

**File: `test/rb3lfe_prv_confabulate_SUITE.erl`**

```erlang
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

    Found = lists:any(
        fun(P) ->
            providers:get_provider_name(P) =:= confabulate andalso
            providers:get_provider_namespace(P) =:= lfe
        end,
        Providers
    ),

    ?assert(Found, "Confabulate provider should be registered"),

    ok.

confabulate_simple_data(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create simple LFE data file
    InputFile = filename:join(TestDir, "simple.lfe"),
    test_utils:write_file(InputFile,
        "((tuple 'person \"Alice\" 30)\n"
        " (tuple 'person \"Bob\" 25))\n"
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

    test_utils:write_file(InputFile, "((tuple 'test 'data))\n"),

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

    test_utils:write_file(InputFile, "((tuple 'new 'data))\n"),

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

    test_utils:write_file(InputFile, "((tuple 'test))\n"),
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
        "(#(type 'library)\n"
        " #(name \"mylib\")\n"
        " #(version \"1.0.0\")\n"
        " #(author \"Alice\")\n"
        " #(license 'apache2))\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    OutputFile = filename:join(TestDir, "multiple.erl"),
    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    %% Each form should be on its own line
    Lines = string:split(ContentStr, "\n", all),
    TermLines = [L || L <- Lines, string:find(L, "{") =/= nomatch],

    ?assertEqual(5, length(TermLines)),

    ok.

confabulate_nested_structures(Config) ->
    TestDir = ?config(test_dir, Config),

    InputFile = filename:join(TestDir, "nested.lfe"),
    test_utils:write_file(InputFile,
        "(#(config\n"
        "   (#(database\n"
        "      (#(host \"localhost\")\n"
        "       #(port 5432)\n"
        "       #(user \"admin\")))\n"
        "    #(logging\n"
        "      (#(level 'info)\n"
        "       #(output \"file\"))))))\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {[{input, InputFile}], []}),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    OutputFile = filename:join(TestDir, "nested.erl"),
    ?assert(filelib:is_file(OutputFile)),

    {ok, Content} = file:read_file(OutputFile),
    ContentStr = binary_to_list(Content),

    %% Should contain nested structure
    ?assert(string:find(ContentStr, "config") =/= nomatch),
    ?assert(string:find(ContentStr, "database") =/= nomatch),
    ?assert(string:find(ContentStr, "logging") =/= nomatch),

    ok.
```

### Task 6.3.4: Create Integration Tests

**File: `test/confabulate_integration_SUITE.erl`**

```erlang
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
    TestDir = test_utils:create_temp_dir("confab_integ"),
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

    %% Create realistic config file
    ConfigFile = filename:join(TestDir, "app.config.lfe"),
    test_utils:write_file(ConfigFile,
        "(#(myapp\n"
        "   (#(port 8080)\n"
        "    #(host \"0.0.0.0\")\n"
        "    #(workers 10)\n"
        "    #(database\n"
        "      (#(host \"db.example.com\")\n"
        "       #(port 5432)\n"
        "       #(name \"myapp_db\")))))\n"
        " #(sasl\n"
        "   (#(sasl_error_logger false)))\n"
        " #(lager\n"
        "   (#(handlers\n"
        "      ((tuple 'lager_console_backend 'info))))))\n"
    ),

    %% Convert
    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {
        [{input, ConfigFile}, {output, filename:join(TestDir, "app.config")}],
        []
    }),

    Result = rb3lfe_prv_confabulate:do(State1),

    ?assertMatch({ok, _}, Result),

    %% Verify output can be consulted
    OutputFile = filename:join(TestDir, "app.config"),
    {ok, Terms} = file:consult(OutputFile),

    ?assert(is_list(Terms)),
    ?assert(length(Terms) > 0),

    %% Verify structure
    MyappConfig = proplists:get_value(myapp, Terms),
    ?assert(is_list(MyappConfig)),

    Port = proplists:get_value(port, MyappConfig),
    ?assertEqual(8080, Port),

    ok.

convert_test_data(Config) ->
    TestDir = ?config(test_dir, Config),

    %% Create test data
    TestDataFile = filename:join(TestDir, "test_data.lfe"),
    test_utils:write_file(TestDataFile,
        "(#(user \"alice\" \"alice@example.com\" 'admin)\n"
        " #(user \"bob\" \"bob@example.com\" 'user)\n"
        " #(user \"charlie\" \"charlie@example.com\" 'user))\n"
    ),

    State = rebar_state:new(),
    State1 = rebar_state:command_parsed_args(State, {
        [{input, TestDataFile}], []
    }),

    Result = rb3lfe_prv_confabulate:do(State1),

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
        rb3lfe_prv_confabulate:do(State1)
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
```

### Task 6.3.5: Update Documentation

**File: `docs/commands-addendum-6.3.md`**

```markdown
# Commands Addendum - Phase 6.3

## confabulate

Convert LFE data files to Erlang data files.

```bash
rebar3 lfe confabulate --input FILE [OPTIONS]
```

**Purpose:**

Convert LFE data structures to Erlang term format. Useful for:
- Configuration file conversion
- Test data generation
- Interoperability between LFE and Erlang
- Data migration tools

### Options

- `--input FILE` or `-i FILE` - Input LFE file (required)
- `--output FILE` or `-o FILE` - Output Erlang file (default: input.erl)
- `--force` or `-f` - Overwrite existing output file

### Examples

**Basic Conversion:**
```bash
# Convert data.lfe to data.erl
rebar3 lfe confabulate --input data.lfe

# Short form
rebar3 lfe confabulate -i data.lfe
```

**Custom Output:**
```bash
# Specify output file
rebar3 lfe confabulate --input config.lfe --output app.config

# Short form
rebar3 lfe confabulate -i config.lfe -o app.config
```

**Force Overwrite:**
```bash
# Overwrite existing file
rebar3 lfe confabulate -i data.lfe --force
```

### Input Format

LFE data file containing data structures (not code):

```lfe
;; users.lfe
(#(user "alice" "alice@example.com" 'admin)
 #(user "bob" "bob@example.com" 'user)
 #(user "charlie" "charlie@example.com" 'user))
```

### Output Format

Erlang term format (one per line):

```erlang
%% users.erl
{user,"alice","alice@example.com",admin}.
{user,"bob","bob@example.com",user}.
{user,"charlie","charlie@example.com",user}.
```

### Use Cases

#### Configuration Files

**LFE Config:**
```lfe
;; app.config.lfe
(#(myapp
   (#(port 8080)
    #(host "0.0.0.0")
    #(database
      (#(host "localhost")
       #(port 5432)))))
 #(sasl
   (#(sasl_error_logger false))))
```

**Convert:**
```bash
rebar3 lfe confabulate -i app.config.lfe -o config/app.config
```

**Result:**
```erlang
%% config/app.config
{myapp,[{port,8080},{host,"0.0.0.0"},{database,[{host,"localhost"},{port,5432}]}]}.
{sasl,[{sasl_error_logger,false}]}.
```

#### Test Data

**Generate test fixtures:**

```lfe
;; test/fixtures.lfe
(#(test_case "success"
   #(input "hello world")
   #(expected "HELLO WORLD"))
 #(test_case "empty"
   #(input "")
   #(expected ""))
 #(test_case "numbers"
   #(input "test123")
   #(expected "TEST123")))
```

```bash
rebar3 lfe confabulate -i test/fixtures.lfe
```

Use in tests:
```erlang
% test/myapp_tests.erl
load_fixtures() ->
    {ok, Fixtures} = file:consult("test/fixtures.erl"),
    Fixtures.
```

#### Data Migration

**Migrate data from LFE format to Erlang:**

```bash
#!/bin/bash
# migrate_data.sh

for file in data/*.lfe; do
    basename=$(basename "$file" .lfe)
    rebar3 lfe confabulate -i "$file" -o "erlang_data/${basename}.erl" --force
done
```

### Limitations

**Only Data Files:**

Confabulate only works with LFE data files, not code:

```lfe
;; âœ… Valid - Data only
(#(name "Project")
 #(version "1.0.0"))

;; âŒ Invalid - Contains code
(defmodule myapp)
(defun test () 'ok)
```

**No Macros:**

Macros are not evaluated:

```lfe
;; âŒ Won't work as expected
(defmacro config () #(port 8080))
(config)  ; Won't be expanded
```

Instead, use evaluated data:

```lfe
;; âœ… Works
#(port 8080)
```

### Working with Results

**Read in Erlang:**
```erlang
% Load converted data
{ok, Terms} = file:consult("data.erl"),
% Terms is a list of Erlang terms
```

**Read in LFE:**
```lfe
; Load converted data
(let ((data (: file consult "data.erl")))
  (case data
    ((tuple 'ok terms) terms)
    ((tuple 'error reason) (error reason))))
```

### Batch Processing

**Convert multiple files:**

```bash
#!/bin/bash
# batch_convert.sh

for lfe_file in config/*.lfe; do
    erl_file="${lfe_file%.lfe}.erl"
    echo "Converting $lfe_file -> $erl_file"
    rebar3 lfe confabulate -i "$lfe_file" -o "$erl_file" --force
done
```

**Makefile example:**

```makefile
# Convert all LFE data files
convert: $(patsubst %.lfe,%.erl,$(wildcard data/*.lfe))

%.erl: %.lfe
	rebar3 lfe confabulate -i $< -o $@ --force
```

### Tips

1. **Verify Output:**
   ```bash
   rebar3 lfe confabulate -i data.lfe
   erl -eval "file:consult(\"data.erl\")." -s init stop
   ```

2. **Format Output:**
   ```bash
   rebar3 lfe confabulate -i data.lfe
   erl -eval "file:consult(\"data.erl\")." -s init stop
   ```

3. **Use in Build Pipeline:**
   ```erlang
   {pre_hooks, [
       {"compile", "rebar3 lfe confabulate -i config/app.config.lfe -o config/app.config --force"}
   ]}.
   ```

4. **Version Control:**
   - Commit .lfe source files
   - Add .erl generated files to .gitignore
   - Generate .erl files during build

### Troubleshooting

**Parse Errors:**
```
Error: Failed to parse data.lfe: {illegal, ...}
```

Check LFE syntax:
```bash
lfe
> (: lfe_io parse_file "data.lfe")
```

**Output Already Exists:**
```
Error: Output file already exists: data.erl
Use --force to overwrite
```

Use `--force` or delete the file:
```bash
rebar3 lfe confabulate -i data.lfe --force
```

**Invalid Data:**

Ensure file contains only data, not code:
```lfe
;; âœ… Good
(#(key "value"))

;; âŒ Bad
(defun test () 'ok)
```

### See Also

- [LFE Data Structures](https://lfe.io/reference/lfe/types.html)
- [Erlang Terms](https://www.erlang.org/doc/reference_manual/data_types.html)
- [Configuration Files](configuration.md)
```

## Testing Instructions

### Running Tests

```bash
# Run confabulate tests
rebar3 ct --suite=test/rb3lfe_prv_confabulate_SUITE

# Run integration tests
rebar3 ct --suite=test/confabulate_integration_SUITE

# Run all tests
rebar3 ct

# Full check
make check
```

### Manual Verification

#### Test Simple Conversion

```bash
# Create test data
mkdir test_confab
cd test_confab

cat > data.lfe <<EOF
(#(person "Alice" 30)
 #(person "Bob" 25)
 #(person "Charlie" 35))
EOF

# Convert
rebar3 lfe confabulate --input data.lfe

# Verify output
cat data.erl

# Should show:
# {person,"Alice",30}.
# {person,"Bob",25}.
# {person,"Charlie",35}.

# Test it works in Erlang
erl -eval "{ok, Terms} = file:consult(\"data.erl\"), io:format(\"~p~n\", [Terms])." -s init stop
```

#### Test Config Conversion

```bash
cat > app.config.lfe <<EOF
(#(myapp
   (#(port 8080)
    #(host "localhost")
    #(workers 4)))
 #(sasl
   (#(sasl_error_logger false))))
EOF

rebar3 lfe confabulate -i app.config.lfe -o app.config

cat app.config

# Load in Erlang shell
erl
> {ok, Config} = file:consult("app.config").
> MyappConfig = proplists:get_value(myapp, Config).
```

#### Test Force Overwrite

```bash
# Create existing output
echo "{old, data}." > data.erl

# Try without force (should fail)
rebar3 lfe confabulate -i data.lfe

# Try with force (should succeed)
rebar3 lfe confabulate -i data.lfe --force

cat data.erl  # Should show new data
```

## Expected Outcomes

At the end of Phase 6.3, you should have:

1. ✅ `rebar3 lfe confabulate` converting data files
2. ✅ Support for input/output options
3. ✅ Force overwrite functionality
4. ✅ Comprehensive error handling
5. ✅ Extensive tests
6. ✅ Complete documentation
7. ✅ Manual verification successful

### Integration Checklist

- [ ] All previous tests still pass
- [ ] Confabulate provider registers correctly
- [ ] Simple data converts correctly
- [ ] Complex/nested data converts correctly
- [ ] Custom output paths work
- [ ] Force overwrite works
- [ ] Error messages are clear
- [ ] Input validation works
- [ ] Output files are valid Erlang terms
- [ ] Works with real config files
- [ ] Dialyzer clean
- [ ] Code coverage >90%

## Completion of Phase 6

With Phase 6.3 complete, all Phase 6 functionality is implemented:

### Phase 6.1 ✅
- `rebar3 lfe run` - Run LFE scripts
- `rebar3 lfe escriptize` - Build escripts
- `rebar3 lfe run-escript` - Execute escripts
- Template auto-registration verified

### Phase 6.2 ✅
- `rebar3 lfe run-release` - Release management
- Integration with release provider
- Full release command support

### Phase 6.3 ✅
- `rebar3 lfe confabulate` - Data conversion
- LFE to Erlang data transformation
- Configuration and test data support

## Next Steps

Now we can create **Phase 7.1** (documentation addendum) covering:
- All new commands from Phase 6
- Template documentation
- Updated examples
- Migration notes
- Final polish

## Notes for Claude Code

### LFE Parsing

- Use `lfe_io:parse_file/1` to parse LFE files
- Returns `{ok, Forms}` where Forms is a list of `{Form, Line}` tuples
- Each Form is an LFE data structure (already parsed)

### Erlang Term Format

- Use `io_lib:format("~p.~n", [Term])` to format as Erlang
- The `~p` format specifier pretty-prints terms
- Add `.~n` for proper Erlang syntax (dot and newline)

### File Handling

- Always delete output file before writing (to ensure clean state)
- Use `[append]` mode to write multiple terms
- Handle errors gracefully with proper cleanup

### Data vs Code

- Confabulate is for **data only**, not code
- LFE modules with `defmodule`, `defun` should NOT be confabulated
- Only files containing data structures (lists, tuples, atoms, strings)

### Common Patterns

**Configuration files** - Most common use case
**Test fixtures** - Generate from LFE for Erlang tests
**Data migration** - Convert between formats
**Interop** - Share data between LFE and Erlang projects
