# Implementation Prompt: rlwrap Integration for rebar3_lfe REPL

## Context

The rebar3_lfe plugin provides an LFE (Lisp Flavoured Erlang) REPL via the command `rebar3 lfe repl`. Currently, enhanced features like command history, tab completion, and better line editing require users to manually set up rlwrap in Makefiles or shell scripts. This creates an asymmetry with this one command where essential functionality cannot be controlled from within the system itself, in contrast to all the other rebar3 lfe functionality, which can.

The goal is to integrate rlwrap directly into the REPL provider using a "trampoline" pattern, where the provider detects whether it should run under rlwrap and, if so, re-executes the entire rebar3 command wrapped by rlwrap.

## Key Challenge

`rebar3 lfe repl` runs inside an already-running rebar3 Erlang VM. This means:

- Boot-time VM flags (like `-prompt`, `-kernel shell_history`) don't work
- We cannot wrap an already-running process with rlwrap from within itself
- rlwrap must be the **parent process** of the Erlang VM

## Solution: Trampoline Pattern

When `rebar3 lfe repl` is invoked:

1. Check if already running under rlwrap (via internal flag)
2. If not, check if rlwrap is available
3. If available, build an rlwrap command and re-execute the entire `rebar3 lfe repl` command under rlwrap
4. The re-executed command includes a special `--rlwrap-active` flag to prevent infinite loops
5. If rlwrap is unavailable or disabled, proceed with basic REPL and show a helpful warning

## Implementation Tasks

### 1. Create File, Add Constants and Exports

**File:** `src/r3lfe_rlwrap.erl`

Add to exports after the existing `-export()` declarations:

```erlang
%% Exported for rlwrap integration
-export([
    should_use_rlwrap/1,
    is_under_rlwrap/0,
    has_rlwrap/0,
    build_rlwrap_command/2,
    get_rebar3_command/0,
    shell_quote/1,
    get_history_file/1,
    get_completion_files/1
]).
```

Add the following:

```erlang
-define(RLWRAP_ACTIVE_FLAG, "--rlwrap-active").
```

### 2. Implement rlwrap Detection Functions

Add these functions in a new section after the trampoline logic:

```erlang
%%====================================================================
%% rlwrap Integration Functions
%%====================================================================

-spec should_use_rlwrap(map()) -> boolean().
should_use_rlwrap(Opts) ->
    %% Check if explicitly disabled
    case maps:get(use_rlwrap, Opts, true) of
        false -> false;
        true ->
            %% Check for --no-rlwrap flag
            not maps:get(no_rlwrap, Opts, false)
    end.

-spec is_under_rlwrap() -> boolean().
is_under_rlwrap() ->
    %% Method 1: Check for RLWRAP_COMMAND environment variable
    case os:getenv("RLWRAP_COMMAND") of
        false ->
            %% Method 2: Check parent process (Unix only)
            case os:type() of
                {unix, _} ->
                    check_parent_is_rlwrap();
                {win32, _} ->
                    false;
                _ ->
                    false
            end;
        _ ->
            true
    end.

-spec check_parent_is_rlwrap() -> boolean().
check_parent_is_rlwrap() ->
    %% Get parent process ID and check its name
    case os:cmd("ps -o comm= -p $PPID 2>/dev/null") of
        "rlwrap" ++ _ -> true;
        _ -> false
    end.

-spec has_rlwrap() -> boolean().
has_rlwrap() ->
    case os:find_executable("rlwrap") of
        false -> false;
        Path when is_list(Path) -> filelib:is_file(Path)
    end.

-spec warn_no_rlwrap() -> ok.
warn_no_rlwrap() ->
    Msg = "~n"
          "╔════════════════════════════════════════════════════════════════╗~n"
          "║  rlwrap not found - enhanced REPL features unavailable        ║~n"
          "╠════════════════════════════════════════════════════════════════╣~n"
          "║  Install rlwrap for:                                          ║~n"
          "║    • Command history with Up/Down arrows                      ║~n"
          "║    • Tab completion for functions and modules                 ║~n"
          "║    • Better line editing (Ctrl+A, Ctrl+E, etc.)              ║~n"
          "║                                                                ║~n"
          "║  Install with:                                                ║~n"
          "║    macOS:    brew install rlwrap                              ║~n"
          "║    Ubuntu:   apt-get install rlwrap                           ║~n"
          "║    Fedora:   dnf install rlwrap                               ║~n"
          "║                                                                ║~n"
          "║  To disable this warning:                                     ║~n"
          "║    rebar3 lfe repl --no-rlwrap                                ║~n"
          "║    or add to rebar.config:                                    ║~n"
          "║    {lfe, [{repl, [{use_rlwrap, false}]}]}                    ║~n"
          "╚════════════════════════════════════════════════════════════════╝~n~n",
    io:format(standard_error, Msg, []),
    ok.
```

### 3. Implement Trampoline Execution

Add after the detection functions:

```erlang
-spec trampoline_via_rlwrap(rebar_state:t(), map()) -> no_return().
trampoline_via_rlwrap(State, Opts) ->
    ?INFO("Restarting under rlwrap for enhanced REPL features...", []),

    %% Build the rlwrap command
    RlwrapCmd = build_rlwrap_command(State, Opts),

    ?DEBUG("Executing: ~s", [RlwrapCmd]),

    %% Execute the command, replacing current process
    %% Note: This will never return if successful
    Port = erlang:open_port({spawn, RlwrapCmd}, [exit_status]),

    %% Wait for the port to finish
    receive
        {Port, {exit_status, Status}} ->
            erlang:halt(Status)
    end.

-spec get_rebar3_command() -> string().
get_rebar3_command() ->
    %% Get the original command that was used to invoke rebar3
    Args = init:get_plain_arguments(),

    %% Find rebar3 executable
    Rebar3 = case os:find_executable("rebar3") of
        false ->
            %% Fallback: check common locations
            case filelib:is_file("./rebar3") of
                true -> "./rebar3";
                false -> "rebar3"  % Hope it's in PATH
            end;
        Path ->
            Path
    end,

    %% Filter out any existing --rlwrap-active or --no-rlwrap flags
    FilteredArgs = lists:filter(
        fun(?RLWRAP_ACTIVE_FLAG) -> false;
           ("--no-rlwrap") -> false;
           (_) -> true
        end,
        Args
    ),

    %% Build command: rebar3 lfe repl --rlwrap-active [original args]
    %% Find where "lfe" and "repl" are in the args
    case {lists:member("lfe", FilteredArgs), lists:member("repl", FilteredArgs)} of
        {true, true} ->
            %% Args already contain lfe and repl, just add our flag
            string:join([Rebar3 | FilteredArgs] ++ [?RLWRAP_ACTIVE_FLAG], " ");
        _ ->
            %% Need to add lfe repl
            string:join([Rebar3, "lfe", "repl", ?RLWRAP_ACTIVE_FLAG | FilteredArgs], " ")
    end.
```

### 4. Implement rlwrap Command Builder

Add after `get_rebar3_command/0`:

```erlang
-spec build_rlwrap_command(rebar_state:t(), map()) -> string().
build_rlwrap_command(State, Opts) ->
    %% Get configuration
    HistoryFile = get_history_file(Opts),
    CompletionFiles = get_completion_files(Opts),
    BreakChars = maps:get(break_chars, Opts, "(){}[]"),
    PromptColor = maps:get(prompt_color, Opts, "1;32"),  % Bright green

    %% Build rlwrap flags
    BaseFlags = [
        "-b", shell_quote(BreakChars),
        "-H", shell_quote(HistoryFile),
        "-p", PromptColor,
        "-c",  % Filename completion
        "-r",  % Remember multi-line commands
        "-s", "10000"  % History size
    ],

    %% Add completion files that exist
    CompletionFlags = lists:flatmap(
        fun(File) ->
            case filelib:is_file(File) of
                true -> ["-f", shell_quote(File)];
                false ->
                    ?DEBUG("Completion file not found: ~s", [File]),
                    []
            end
        end,
        CompletionFiles
    ),

    RlwrapFlags = BaseFlags ++ CompletionFlags,

    %% Get the rebar3 command to wrap
    Rebar3Cmd = get_rebar3_command(),

    %% Combine everything
    lists:flatten([
        "exec rlwrap ",
        string:join(RlwrapFlags, " "),
        " ",
        Rebar3Cmd
    ]).

-spec get_history_file(map()) -> string().
get_history_file(Opts) ->
    case maps:get(history_file, Opts, undefined) of
        undefined ->
            %% Default location
            Home = os:getenv("HOME", "/tmp"),
            LfeDir = filename:join([Home, ".lfe"]),
            %% Ensure directory exists
            filelib:ensure_dir(filename:join(LfeDir, "dummy")),
            filename:join(LfeDir, "history");
        Path ->
            %% Expand ~ if present
            expand_home(Path)
    end.

-spec get_completion_files(map()) -> [string()].
get_completion_files(Opts) ->
    %% Base completion files
    Home = os:getenv("HOME", "/tmp"),
    LfeDir = filename:join(Home, ".lfe"),
    CompletionDir = filename:join(LfeDir, "completions"),

    BaseFiles = [
        filename:join(CompletionDir, "erlang.txt"),
        filename:join(CompletionDir, "lfe.txt")
    ],

    %% User-provided additional files
    UserFiles = maps:get(completion_files, Opts, []),
    ExpandedUserFiles = [expand_home(F) || F <- UserFiles],

    %% Return all files (we filter for existence in build_rlwrap_command)
    BaseFiles ++ ExpandedUserFiles.

-spec shell_quote(string()) -> string().
shell_quote(Str) ->
    %% Escape single quotes by replacing ' with '\''
    Escaped = re:replace(Str, "'", "'\\\\''", [global, {return, list}]),
    "'" ++ Escaped ++ "'".

-spec expand_home(string()) -> string().
expand_home("~/" ++ Rest) ->
    Home = os:getenv("HOME", "/tmp"),
    filename:join(Home, Rest);
expand_home("~" ++ Rest) ->
    Home = os:getenv("HOME", "/tmp"),
    filename:join(Home, Rest);
expand_home(Path) ->
    Path.
```

### 5. Update Provider `do/1` Function

**File:** `src/r3lfe_prv_repl.erl`

Replace the current `do/1` function (starts around line 65) with:

```erlang
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE REPL provider starting", []),

    %% Check if we should wrap with rlwrap
    {Opts, Args} = rebar_state:command_parsed_args(State),

    %% Check for internal flag (prevents infinite loop)
    case lists:member(?RLWRAP_ACTIVE_FLAG, Args) of
        true ->
            %% Already under rlwrap, proceed normally
            do_repl(State, Opts);
        false ->
            %% Potentially trampoline through rlwrap
            maybe_trampoline_rlwrap(State, Opts)
    end.
```

### 6. Extract Core REPL Logic

Rename the existing logic in `do/1` (code path setup, app starting, version check, etc.) to a new function:

```erlang
%% Internal function that does the actual REPL work
-spec do_repl(rebar_state:t(), proplists:proplist()) ->
    {ok, rebar_state:t()} | {error, string()}.
do_repl(State, Opts) ->
    ?DEBUG("LFE REPL provider starting (rlwrap-checked)", []),

    %% Set up code paths (deps, plugins, and project apps)
    rebar_paths:set_paths([deps, plugins, runtime], State),

    %% Get REPL configuration
    LfeConfig = rebar_state:get(State, lfe, []),
    ReplConfig = proplists:get_value(repl, LfeConfig, []),

    %% Merge options
    MergedOpts = merge_repl_opts(ReplConfig, Opts),

    %% Start apps if requested
    maybe_start_apps(MergedOpts, State),

    %% Run script if provided
    maybe_run_script(MergedOpts),

    %% Check Erlang version and start appropriate REPL
    OTPRelease = erlang:system_info(otp_release),

    if
        OTPRelease >= "26" ->
            start_modern_repl(MergedOpts, State);
        true ->
            start_legacy_repl(MergedOpts, State)
    end,

    {ok, State}.
```

### 7. Implement Trampoline Decision Logic

Add this new function after `do_repl/2`:

```erlang
-spec maybe_trampoline_rlwrap(rebar_state:t(), proplists:proplist()) ->
    {ok, rebar_state:t()} | {error, string()}.
maybe_trampoline_rlwrap(State, Opts) ->
    %% Get config
    LfeConfig = rebar_state:get(State, lfe, []),
    ReplConfig = proplists:get_value(repl, LfeConfig, []),
    MergedOpts = merge_repl_opts(ReplConfig, Opts),

    case r3lfe_rlwrap:should_use_rlwrap(MergedOpts) of
        false ->
            %% User disabled or not available
            do_repl(State, Opts);
        true ->
            case is_under_rlwrap() of
                true ->
                    %% Already wrapped (shouldn't happen, but be safe)
                    do_repl(State, Opts);
                false ->
                    %% Need to trampoline!
                    case r3lfe_rlwrap:has_rlwrap() of
                        true ->
                           r3lfe_rlwrap:trampoline_via_rlwrap(State, MergedOpts);
                        false ->
                            r3lfe_rlwrap:warn_no_rlwrap(),
                            do_repl(State, Opts)
                    end
            end
    end.
```

### 8. Update Provider Options

In the `init/1` function, add new options to the `Opts` list (around line 40):

```erlang
Opts = [
    {name, undefined, "name", atom,
     "Give a long name to the node"},
    {sname, undefined, "sname", atom,
     "Give a short name to the node"},
    {setcookie, undefined, "setcookie", atom,
     "Set the cookie for distributed node"},
    {apps, undefined, "apps", string,
     "List of apps to start (comma-separated)"},
    {script, undefined, "script", string,
     "Script to run before starting REPL"},
    {prompt, undefined, "prompt", string,
     "Custom REPL prompt (use 'classic' for old-style '> ')"},
    {erl, undefined, "erl", string,
     "Additional Erlang VM arguments (e.g., for -prompt)"},
    {vm_args, undefined, "vm_args", string,
     "Path to vm.args file for VM configuration"},
    {no_rlwrap, undefined, "no-rlwrap", boolean,
     "Disable rlwrap integration"}
],
```

### 9. Update Documentation in `info/1`

Add a new section to the `info/1` function after the existing VM Arguments section:

```erlang
"~n"
"rlwrap Integration:~n"
"  The REPL automatically uses rlwrap if available, providing:~n"
"    • Command history with Up/Down arrows~n"
"    • Tab completion for modules and functions~n"
"    • Better line editing (Emacs-style keybindings)~n"
"    • Persistent history across sessions~n"
"~n"
"  rlwrap is detected and enabled automatically. No configuration needed!~n"
"~n"
"  Configuration via rebar.config:~n"
"    {lfe, [{repl, [~n"
"        {use_rlwrap, true},                    %% Enable/disable~n"
"        {history_file, \"~~/.lfe/history\"},     %% Custom history location~n"
"        {break_chars, \"()\"},                   %% Word break chars for completion~n"
"        {prompt_color, \"1;32\"},                %% ANSI color code (bright green)~n"
"        {completion_files, [                   %% Additional completion files~n"
"            \"~~/.lfe/completions/myapp.txt\"~n"
"        ]}~n"
"    ]}]}.~n"
"~n"
"  Command line options:~n"
"    --no-rlwrap           Disable rlwrap for this session~n"
"~n"
"  Color codes for prompt_color:~n"
"    \"1;32\" - Bright green (default)~n"
"    \"1;34\" - Bright blue~n"
"    \"1;33\" - Bright yellow~n"
"    \"31\"   - Red~n"
"    \"36\"   - Cyan~n"
"~n"
"  Break characters control where word breaks occur for tab completion.~n"
"  For LFE, parentheses should typically NOT break words, so they're~n"
"  included in break_chars by default: \"(){}[]\"~n"
"~n"
"  Completion files:~n"
"    Create text files with one completion entry per line.~n"
"    Default locations (auto-loaded if present):~n"
"      ~~/.lfe/completions/erlang.txt~n"
"      ~~/.lfe/completions/lfe.txt~n"
"~n"
```

### 10. Create Test Cases

**File:** `test/r3lfe_rlwrap_SUITE.erl`

Using Erlang CT patterns and best practices, and following the example of the other `r3lfe_*SUITE.erl` files in the `./test` directory, create a new SUITE file for testing rlwrap functionality.

Add an `-export([` section with the following:

```erlang
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
```

Create the `all()` function:

```erlang
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
```

Add test case implementations:

```erlang
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

    %% Should start with "exec rlwrap"
    ?assert(string:str(Result, "exec rlwrap") =:= 1),
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
```

### 11. Create Completion File Generator

Create a new file: `src/r3lfe_completion.erl`

This module will generate completion files for rlwrap:

```erlang
-module(r3lfe_completion).

-export([
    generate_erlang_completions/1,
    generate_lfe_completions/1,
    ensure_files/0
]).

%% Generate completion file for Erlang standard library
-spec generate_erlang_completions(string()) -> ok | {error, term()}.
generate_erlang_completions(OutputPath) ->
    %% Get all loaded modules
    Modules = [atom_to_list(M) || M <- erlang:loaded()],

    %% Write to file, one per line
    Content = string:join(lists:sort(Modules), "\n") ++ "\n",

    filelib:ensure_dir(OutputPath),
    file:write_file(OutputPath, Content).

%% Generate completion file for LFE
-spec generate_lfe_completions(string()) -> ok | {error, term()}.
generate_lfe_completions(OutputPath) ->
    %% Common LFE forms and functions
    LfeForms = [
        "defun", "defmacro", "defmodule", "defrecord",
        "lambda", "match-lambda", "let", "let*", "letrec-function",
        "case", "if", "cond", "when", "receive", "try", "catch",
        "cons", "car", "cdr", "list", "tuple", "map",
        "+", "-", "*", "/", "div", "rem", "mod",
        "=:=", "=/=", "<", ">", "=<", ">=",
        "andalso", "orelse", "not",
        "progn", "eval-when-compile", "include-file",
        "binary", "bitstring",
        "map-get", "map-set", "map-update",
        "lists:map", "lists:filter", "lists:foldl", "lists:foldr",
        "io:format", "io:get_line"
    ],

    Content = string:join(lists:sort(LfeForms), "\n") ++ "\n",

    filelib:ensure_dir(OutputPath),
    file:write_file(OutputPath, Content).

%% Ensure default completion files exist
-spec ensure_files() -> ok.
ensure_files() ->
    Home = os:getenv("HOME", "/tmp"),
    CompletionDir = filename:join([Home, ".lfe", "completions"]),

    filelib:ensure_dir(filename:join(CompletionDir, "dummy")),

    ErlangFile = filename:join(CompletionDir, "erlang.txt"),
    LfeFile = filename:join(CompletionDir, "lfe.txt"),

    %% Generate if they don't exist
    case filelib:is_file(ErlangFile) of
        false -> generate_erlang_completions(ErlangFile);
        true -> ok
    end,

    case filelib:is_file(LfeFile) of
        false -> generate_lfe_completions(LfeFile);
        true -> ok
    end,

    ok.
```

Call this in `maybe_trampoline_rlwrap/2` before building the rlwrap command:

```erlang
%% Ensure completion files exist
r3lfe_completion:ensure_files(),
```

## Testing the Implementation

### Manual Testing

1. **Without rlwrap installed:**

   ```bash
   rebar3 lfe repl
   # Should show warning box and continue with basic REPL
   ```

2. **With rlwrap installed:**

   ```bash
   rebar3 lfe repl
   # Should automatically restart under rlwrap
   # You should see: "Restarting under rlwrap for enhanced REPL features..."
   # Try Up arrow for history, Tab for completion
   ```

3. **Disabling rlwrap:**

   ```bash
   rebar3 lfe repl --no-rlwrap
   # Should skip rlwrap and go straight to REPL
   ```

4. **With configuration:**

   In `rebar.config`:

   ```erlang
   {lfe, [
       {repl, [
           {prompt_color, "1;34"},  % Blue prompt
           {break_chars, "()"},
           {history_file, "~/.lfe/my_history"}
       ]}
   ]}.
   ```

   Then run:

   ```bash
   rebar3 lfe repl
   ```

### Automated Testing

Run the test suite:

```bash
rebar3 ct
```

All new test cases should pass.

## Expected Behavior

### First Run

```
$ rebar3 lfe repl
===> Restarting under rlwrap for enhanced REPL features...

╔═════════════════════════════════════════════════════════════════╗
║ ┌─────────────────────────────────────────────────────────────┐ ║
║ │                                                             │ ║
║ │     ..-~.~_~---..                                           │ ║
║ │    (      \\     )    A Lisp-2+ on the Erlang VM            │ ║
║ │    |`-.._/_\\_.-':    Type (help) for usage info.           │ ║
║ │    |         g |_ \                                         │ ║
║ │    |        n    | |  Docs: http://docs.lfe.io/             │ ║
║ │    |       a    / /   Source: http://github.com/lfe/lfe     │ ║
║ │     \     l    |_/                                          │ ║
║ │      \   r     /      LFE v2.1.2 (abort with ^G)            │ ║
║ │       `-E___.-'                                             │ ║
║ │                                                             │ ║
║ └─────────────────────────────────────────────────────────────┘ ║
╚═════════════════════════════════════════════════════════════════╝

lfe>
```

- Up/Down arrows work for history
- Tab completion works for modules/functions
- Ctrl+A, Ctrl+E, etc. work for line editing
- History persists across sessions

### Without rlwrap installed

```
$ rebar3 lfe repl

╔════════════════════════════════════════════════════════════════╗
║  rlwrap not found - enhanced REPL features unavailable        ║
╠════════════════════════════════════════════════════════════════╣
║  Install rlwrap for:                                          ║
║    • Command history with Up/Down arrows                      ║
║    • Tab completion for functions and modules                 ║
║    • Better line editing (Ctrl+A, Ctrl+E, etc.)              ║
║                                                                ║
║  Install with:                                                ║
║    macOS:    brew install rlwrap                              ║
║    Ubuntu:   apt-get install rlwrap                           ║
║    Fedora:   dnf install rlwrap                               ║
║                                                                ║
║  To disable this warning:                                     ║
║    rebar3 lfe repl --no-rlwrap                                ║
║    or add to rebar.config:                                    ║
║    {lfe, [{repl, [{use_rlwrap, false}]}]}                    ║
╚════════════════════════════════════════════════════════════════╝

[Basic REPL starts without rlwrap features]
```

## Key Design Principles

1. **Transparent**: Users don't need to know rlwrap exists—it just works
2. **Configurable**: Power users can customize via `rebar.config`
3. **Graceful degradation**: Works fine without rlwrap, just shows a helpful warning
4. **No infinite loops**: The `--rlwrap-active` flag prevents re-trampolining
5. **Cross-platform aware**: Detects OS and handles Windows gracefully
6. **Testable**: All key functions are exported and tested

## Potential Issues and Solutions

### Issue: `os:cmd` blocks until completion

**Solution:** Use `erlang:open_port` with `exit_status` to properly handle the subprocess

### Issue: Finding the rebar3 executable

**Solution:** Check multiple locations: `os:find_executable`, `./rebar3`, fallback to `"rebar3"`

### Issue: Quoting arguments for shell

**Solution:** Implement proper `shell_quote/1` that handles single quotes

### Issue: User has custom rebar3 location

**Solution:** Use the actual rebar3 that was invoked by checking `init:get_plain_arguments()`

## Future Enhancements

1. **Auto-generate completion files** from loaded modules and LFE forms
2. **Smart completion** that understands LFE context (in strings, in comments, etc.)
3. **Custom key bindings** via rlwraprc file
4. **Integration with language server** for semantic completion
5. **Windows support** via alternative readline library

## Conclusion

This implementation solves the "asymmetry problem" by bringing rlwrap control into the rebar3_lfe system itself. Users get enhanced REPL features automatically, with full configurability through the standard rebar.config mechanism.
