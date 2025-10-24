# Implementation Prompt: rebar3 lfe eval Command

## Objective

Implement a new rebar3_lfe provider that allows users to evaluate arbitrary LFE expressions from the command line without starting an interactive REPL session.

## Command Specification

**Command format:**
```bash
rebar3 lfe eval '<LFE-expression>'
```

**Expected behavior:**
```bash
$ rebar3 lfe eval '(* 2 (+ 1 2 3 4 5 6))'
42

$ rebar3 lfe eval '(lists:map (lambda (x) (* x x)) (list 1 2 3 4 5))'
(1 4 9 16 25)

$ rebar3 lfe eval '(: io format "Hello from LFE!~n" ())'
Hello from LFE!
ok
```

The command should:
1. Parse the LFE expression from the command-line argument
2. Evaluate it in an LFE environment
3. Print the result to stdout
4. Exit immediately (no REPL/shell should remain running)

## Implementation Tasks

### 1. Create Provider Module: `src/r3lfe_prv_eval.erl`

Create a new provider module following the pattern from `r3lfe_prv_ltest.erl` and other providers. The module should:

**Module structure:**
```erlang
-module(r3lfe_prv_eval).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, eval).
-define(DEPS, [{?NAMESPACE, compile}]).
```

**Key implementation details:**

- **Provider definition:** Use `providers:create/1` with appropriate metadata:
  - `namespace`: `?NAMESPACE` (which should be `lfe`)
  - `name`: `eval`
  - `deps`: Depends on `{lfe, compile}` to ensure code is compiled first
  - `bare`: `true` (allows running without other commands)
  - `example`: `"rebar3 lfe eval '(+ 1 2 3)'"`
  - `short_desc`: "Evaluate an LFE expression"
  - `opts`: Define a single positional argument for the expression

**Command-line options:**
```erlang
Opts = [
    {expr, undefined, undefined, string,
     "LFE expression to evaluate (required)"}
]
```

**The `do/1` function should:**

1. Extract command-line arguments using `rebar_state:command_parsed_args(State)`
2. Validate that an expression was provided (error if missing)
3. Set up code paths using `rebar_paths:set_paths([deps, plugins, runtime], State)`
4. Ensure LFE is available with `code:ensure_loaded(lfe_eval)`
5. Call the LFE evaluation function (see below)
6. Format and print the result
7. Return `{ok, State}` on success or `{error, Reason}` on failure

### 2. LFE Evaluation Function

The core evaluation should use LFE's built-in evaluation capabilities. Based on LFE's architecture, you should call:

```erlang
lfe_eval:expr(ExprString, Environment)
```

**However**, the expression needs to be parsed first. The correct sequence is:

1. **Parse the string into LFE forms:**
   ```erlang
   {ok, Tokens, _} = lfe_scan:string(ExprString),
   {ok, [Form]} = lfe_parse:sexpr(Tokens)
   ```

2. **Evaluate the form:**
   ```erlang
   {value, Result, _NewEnv} = lfe_eval:expr(Form, lfe_env:new())
   ```

3. **Format the result for output:**
   ```erlang
   %% Convert result to string representation
   ResultStr = lfe_io:print1(Result)
   ```

**Complete evaluation helper function:**
```erlang
-spec eval_expression(string()) -> {ok, term()} | {error, term()}.
eval_expression(ExprString) ->
    try
        %% Parse the expression
        case lfe_scan:string(ExprString) of
            {ok, Tokens, _Line} ->
                case lfe_parse:sexpr(Tokens) of
                    {ok, [Form]} ->
                        %% Evaluate in new environment
                        Env = lfe_env:new(),
                        {value, Result, _NewEnv} = lfe_eval:expr(Form, Env),
                        {ok, Result};
                    {ok, []} ->
                        {error, "Empty expression"};
                    {ok, _Multiple} ->
                        {error, "Multiple expressions not supported"};
                    {error, ParseError} ->
                        {error, {parse_error, ParseError}}
                end;
            {error, ScanError, _} ->
                {error, {scan_error, ScanError}}
        end
    catch
        error:Reason:Stack ->
            {error, {eval_error, Reason, Stack}};
        throw:Reason ->
            {error, {throw, Reason}};
        exit:Reason ->
            {error, {exit, Reason}}
    end.
```

### 3. Result Formatting

The result should be printed in LFE's standard format. Use:

```erlang
-spec format_result(term()) -> ok.
format_result(Result) ->
    %% Print result in LFE format
    ResultStr = lfe_io:print1(Result),
    io:format("~ts~n", [ResultStr]),
    ok.
```

**Error formatting:**
```erlang
-spec format_error(term()) -> iolist().
format_error({missing_expr}) ->
    "Error: No expression provided. Usage: rebar3 lfe eval '<expression>'";
format_error({scan_error, Error}) ->
    io_lib:format("Scan error: ~p", [Error]);
format_error({parse_error, Error}) ->
    io_lib:format("Parse error: ~p", [Error]);
format_error({eval_error, Reason, Stack}) ->
    io_lib:format("Evaluation error: ~p~n~p", [Reason, Stack]);
format_error({throw, Reason}) ->
    io_lib:format("Expression threw: ~p", [Reason]);
format_error({exit, Reason}) ->
    io_lib:format("Expression exited: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("Error: ~p", [Reason]).
```

### 4. Register Provider in Plugin

Add the new provider to the list in `src/rebar3_lfe.erl`:

```erlang
Providers = [
    r3lfe_prv_compile,
    r3lfe_prv_clean,
    r3lfe_prv_repl,
    r3lfe_prv_ltest,
    r3lfe_prv_eval,        %% ADD THIS LINE
    r3lfe_prv_release,
    r3lfe_prv_versions,
    r3lfe_prv_run,
    r3lfe_prv_escriptize,
    r3lfe_prv_run_escript,
    r3lfe_prv_run_release,
    r3lfe_prv_confabulate
],
```

### 5. Create Comprehensive Tests

Create `test/r3lfe_prv_eval_tests.erl` with test cases covering:

**Basic arithmetic:**
```erlang
simple_arithmetic_test() ->
    %% Test: (+ 1 2 3)
    %% Expected: 6
    
simple_multiplication_test() ->
    %% Test: (* 2 (+ 1 2 3 4 5 6))
    %% Expected: 42
```

**Function calls:**
```erlang
list_operations_test() ->
    %% Test: (length (list 1 2 3 4 5))
    %% Expected: 5

list_map_test() ->
    %% Test: (lists:map (lambda (x) (* x x)) (list 1 2 3))
    %% Expected: (1 4 9)
```

**Built-in functions:**
```erlang
io_format_test() ->
    %% Test: (: io format "test~n" ())
    %% Expected: Captures output "test\n" and returns 'ok'
```

**Error handling:**
```erlang
invalid_syntax_test() ->
    %% Test: (+ 1 2
    %% Expected: Parse error

undefined_function_test() ->
    %% Test: (nonexistent:function 1 2)
    %% Expected: Evaluation error

division_by_zero_test() ->
    %% Test: (div 10 0)
    %% Expected: Error with appropriate message
```

**Empty/malformed input:**
```erlang
empty_expression_test() ->
    %% Test: ""
    %% Expected: Error about empty expression

missing_expression_test() ->
    %% Test: No argument provided
    %% Expected: Usage error
```

**Complex expressions:**
```erlang
nested_expression_test() ->
    %% Test: (let ((x 10) (y 20)) (+ x y))
    %% Expected: 30

conditional_test() ->
    %% Test: (if (> 5 3) 'yes 'no)
    %% Expected: yes
```

**Test helper functions:**
```erlang
%% Mock rebar_state for testing
-spec mock_state_with_expr(string()) -> rebar_state:t().
mock_state_with_expr(Expr) ->
    %% Create mock state with command args set to include the expression
    State = rebar_state:new(),
    %% Set command_parsed_args to return the expression
    %% This may require using meck or similar mocking library
    State.

%% Helper to capture stdout
-spec capture_output(fun(() -> term())) -> {term(), string()}.
capture_output(Fun) ->
    %% Redirect stdout, run function, capture output, restore stdout
    %% Return {Result, CapturedOutput}
    {ok, StringIO} = io:get_device(),
    %% Implementation depends on testing approach
    ok.
```

### 6. Documentation

Add documentation to the provider's `info/1` function:

```erlang
-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Evaluates a single LFE expression and prints the result.~n"
        "~n"
        "Usage:~n"
        "  rebar3 lfe eval '<expression>'~n"
        "~n"
        "Examples:~n"
        "  rebar3 lfe eval '(+ 1 2 3)'~n"
        "  rebar3 lfe eval '(lists:map (lambda (x) (* x x)) (list 1 2 3))'~n"
        "  rebar3 lfe eval '(: io format \"Hello, LFE!~~n\" ())'~n"
        "~n"
        "Note: The expression must be quoted to prevent shell interpretation.~n"
        "      Use single quotes for simple expressions, or escape characters~n"
        "      as needed for your shell.~n",
        [Description]
    ).
```

## Implementation Checklist

- [ ] Create `src/r3lfe_prv_eval.erl` with provider behavior
- [ ] Implement `init/1` to register the provider with appropriate options
- [ ] Implement `do/1` to:
  - [ ] Extract expression from command-line arguments
  - [ ] Validate expression is provided
  - [ ] Set up code paths
  - [ ] Parse LFE expression using `lfe_scan` and `lfe_parse`
  - [ ] Evaluate expression using `lfe_eval:expr/2`
  - [ ] Format result using `lfe_io:print1/1`
  - [ ] Print result to stdout
  - [ ] Handle all error cases gracefully
- [ ] Implement `format_error/1` for comprehensive error messages
- [ ] Add provider to `rebar3_lfe.erl` providers list
- [ ] Create `test/r3lfe_prv_eval_tests.erl` with comprehensive test coverage
- [ ] Test manually with various expressions
- [ ] Document in README or user guide

## Testing Strategy

**Unit tests should:**
1. Mock `rebar_state` to provide test expressions
2. Test the parsing and evaluation logic in isolation
3. Verify error handling for all edge cases
4. Use property-based testing for random valid LFE expressions

**Integration tests should:**
1. Actually invoke `rebar3 lfe eval` via shell
2. Verify output matches expected results
3. Test with complex real-world expressions
4. Verify it works in projects with dependencies

## Edge Cases to Handle

1. **Empty string:** Error with usage message
2. **Multiple expressions:** Error or evaluate only the first
3. **Expressions with side effects:** Should work (e.g., file I/O, printing)
4. **Long-running expressions:** No timeout (user can Ctrl+C)
5. **Expressions requiring compiled modules:** Should work if project is compiled
6. **Unicode in expressions:** Should handle properly
7. **Very large results:** Should print without truncation

## Reference Implementation Pattern

Follow the pattern from `r3lfe_prv_ltest.erl`:

1. Provider registration in `init/1`
2. Main logic in `do/1`
3. Code path setup using `rebar_paths:set_paths/2`
4. Proper error handling and reporting
5. Use of rebar3 API (from `rebar3-api-reference.md`)
6. Logging with `?DEBUG`, `?INFO`, `?WARN`, `?ERROR` macros

## Key Rebar3 API Functions to Use

From the API reference document:

- `rebar_state:command_parsed_args/1` - Get command-line arguments
- `rebar_paths:set_paths/2` - Set up code paths
- `rebar_state:add_provider/2` - Register the provider
- `rebar_api:info/2`, `rebar_api:error/2` - Logging
- `providers:create/1` - Create provider specification

## Success Criteria

The implementation is complete when:

1. `rebar3 lfe eval '(+ 1 2 3)'` prints `6`
2. Complex expressions with lambdas and function calls work
3. All tests pass
4. Error messages are clear and helpful
5. The command exits immediately after evaluation
6. Documentation is complete and accurate
7. Code follows the style and patterns of other providers in the project

## Notes

- The expression must be a single, complete LFE form
- The environment should be clean (no pre-loaded modules beyond standard library)
- Side effects (I/O, etc.) should be allowed and executed
- The result should be printed in LFE's native format (not Erlang term format)
- Consider supporting a `--erlang` flag to print in Erlang format if desired (optional enhancement)
