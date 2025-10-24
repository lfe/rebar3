-module(r3lfe_rlwrap).

%% Exported for rlwrap integration
-export([
    should_use_rlwrap/1,
    is_under_rlwrap/0,
    has_rlwrap/0,
    build_rlwrap_command/2,
    get_rebar3_command/0,
    shell_quote/1,
    get_history_file/1,
    get_completion_files/1,
    trampoline_via_rlwrap/2,
    warn_no_rlwrap/0,
    expand_home/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-define(RLWRAP_ACTIVE_FLAG, "--rlwrap-active").

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

%%====================================================================
%% Trampoline Execution Functions
%%====================================================================

-spec trampoline_via_rlwrap(rebar_state:t(), map()) -> no_return().
trampoline_via_rlwrap(State, Opts) ->
    ?INFO("Restarting under rlwrap for enhanced REPL features...", []),

    %% Build the rlwrap command as a shell command string
    %% We have to use the shell approach because rlwrap needs a TTY
    RlwrapCmd = build_rlwrap_command(State, Opts),

    ?DEBUG("Executing: ~s", [RlwrapCmd]),

    %% Write a small shell script that will exec rlwrap
    %% This ensures rlwrap replaces the shell process and gets the TTY
    ScriptPath = "/tmp/rebar3_lfe_rlwrap_" ++ os:getpid() ++ ".sh",
    %% Delete the script file before exec (it's already loaded into memory)
    Script = "#!/bin/sh\nrm -f " ++ ScriptPath ++ "\nexec " ++ RlwrapCmd ++ "\n",

    ok = file:write_file(ScriptPath, Script),
    ok = file:change_mode(ScriptPath, 8#755),

    ?DEBUG("Running script: ~s", [ScriptPath]),

    %% Execute the script using open_port with nouse_stdio
    %% This allows the child process to inherit stdin/stdout/stderr
    %% and interact directly with the terminal
    Port = erlang:open_port(
        {spawn, ScriptPath},
        [nouse_stdio, exit_status]
    ),

    %% Wait for the script to complete
    %% The port will exit when rlwrap/rebar3 exits
    receive
        {Port, {exit_status, Status}} ->
            erlang:halt(Status)
    end.

-spec get_rebar3_command() -> string().
get_rebar3_command() ->
    %% Get the original command that was used to invoke rebar3
    %% We need to be careful here - init:get_plain_arguments() returns
    %% all arguments, including Erlang VM args, not just rebar3 command args
    Args = init:get_plain_arguments(),

    ?DEBUG("All init args: ~p", [Args]),

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

    %% Filter out Erlang VM arguments and flags we don't want
    %% Keep only arguments that look like rebar3 commands and their options
    FilteredArgs = lists:filter(
        fun(?RLWRAP_ACTIVE_FLAG) -> false;
           ("--no-rlwrap") -> false;
           ("-extra" ++ _) -> false;  % Erlang VM arg
           ("-noshell") -> false;      % Erlang VM arg
           ("-noinput") -> false;      % Erlang VM arg
           ("+sbtu") -> false;         % Erlang VM arg
           ("+sbwt") -> false;         % Erlang VM arg
           ([C|_]) when C >= $0, C =< $9 -> false;  % Numeric args (like "1")
           (Arg) ->
               %% Keep arguments that start with - or are words
               case Arg of
                   "-" ++ _ -> true;  % Keep flags
                   _ ->
                       %% Keep if it's a word (lfe, repl, compile, etc.)
                       not lists:any(fun(Ch) -> Ch >= $0 andalso Ch =< $9 end, Arg)
                           orelse lists:member(Arg, ["lfe", "repl", "compile", "test"])
               end
        end,
        Args
    ),

    ?DEBUG("Filtered args: ~p", [FilteredArgs]),

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

%%====================================================================
%% rlwrap Command Builder
%%====================================================================

%% Kept for potential future use with spawn_executable
%% Currently unused because rlwrap needs TTY access which requires shell
-ifdef(UNUSED).
-spec build_rlwrap_args(rebar_state:t(), map()) -> {string(), [string()]}.
build_rlwrap_args(_State, Opts) ->
    %% Get configuration
    HistoryFile = get_history_file(Opts),
    CompletionFiles = get_completion_files(Opts),
    BreakChars = maps:get(break_chars, Opts, "(){}[]"),
    PromptColor = maps:get(prompt_color, Opts, "1;32"),  % Bright green

    %% Find rlwrap executable
    RlwrapExe = case os:find_executable("rlwrap") of
        false -> "rlwrap";  % Hope it's in PATH
        RlwrapPath -> RlwrapPath
    end,

    %% Build rlwrap arguments as a list (no quoting needed!)
    %% Note: The -p flag requires its argument to be attached (e.g., -p1;32)
    %% as per rlwrap documentation
    BaseArgs = [
        "-b", BreakChars,
        "-H", HistoryFile,
        "-p" ++ PromptColor,  % Attach color code to flag
        "-c",  % Filename completion
        "-r",  % Remember multi-line commands
        "-s", "10000"  % History size
    ],

    %% Add completion files that exist
    CompletionArgs = lists:flatmap(
        fun(File) ->
            case filelib:is_file(File) of
                true -> ["-f", File];
                false ->
                    ?DEBUG("Completion file not found: ~s", [File]),
                    []
            end
        end,
        CompletionFiles
    ),

    %% Get the rebar3 command parts
    Rebar3 = case os:find_executable("rebar3") of
        false ->
            case filelib:is_file("./rebar3") of
                true -> "./rebar3";
                false -> "rebar3"
            end;
        Path ->
            Path
    end,

    %% Build complete argument list
    AllArgs = BaseArgs ++ CompletionArgs ++ [Rebar3, "lfe", "repl", ?RLWRAP_ACTIVE_FLAG],

    {RlwrapExe, AllArgs}.
-endif.

%% Build rlwrap command as shell command string
-spec build_rlwrap_command(rebar_state:t(), map()) -> string().
build_rlwrap_command(_State, Opts) ->
    %% Get configuration
    HistoryFile = get_history_file(Opts),
    CompletionFiles = get_completion_files(Opts),
    BreakChars = maps:get(break_chars, Opts, "(){}[]"),
    %% rlwrap expects format: <attr>;<fg> or <attr>;<fg>;<bg>
    %% According to error message: attr=[0-8], fg=[30-37], bg=[40-47]
    %% Let's use just "1;32" which should work (bold green)
    %% If rlwrap is being strict, we may need to test without -p flag
    _PromptColor = maps:get(prompt_color, Opts, "1;32"),  % Bright green

    %% Build rlwrap flags for shell command
    %% The -p flag requires its argument attached (e.g., -p1;32)
    %% Temporarily disable -p to test if everything else works
    BaseFlags = [
        "-b", shell_quote(BreakChars),
        "-H", shell_quote(HistoryFile),
        %% Skip -p for now to test basic functionality
        %% "-p" ++ PromptColor,  % Attach directly to avoid semicolon issues
        "-c",  % Filename completion
        "-r",  % Remember multi-line commands
        "-s", "10000",  % History size
        "--always-readline",  % Required for REPLs that use character-at-a-time input
        "-n"  % No warnings (silences the "appears to do nothing" warning)
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
        "rlwrap ",
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
