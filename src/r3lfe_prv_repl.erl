-module(r3lfe_prv_repl).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-export([build_banner/0]).

%% Exported for testing
-export([
    read_vm_args/1,
    build_shell_args/1,
    merge_repl_opts/2
]).

-include("r3lfe.hrl").

-define(PROVIDER, repl).
-define(DEPS, [{?NAMESPACE, compile}]).
-define(RLWRAP_ACTIVE_FLAG, "--rlwrap-active").
%% Coloured strings for the LFE banner, red, green, yellow and blue.
-define(RED(Str), "\e[31m" ++ Str ++ "\e[0m").
-define(GRN(Str), "\e[1;32m" ++ Str ++ "\e[0m").
-define(YLW(Str), "\e[1;33m" ++ Str ++ "\e[0m").
-define(YLW_REG(Str), "\e[33m" ++ Str ++ "\e[0m").  % Regular (non-bold) yellow
-define(BLU(Str), "\e[1;34m" ++ Str ++ "\e[0m").
-define(BOLD(Str), "\e[1m" ++ Str ++ "\e[0m").
-define(DGRY(Str), "\e[90m" ++ Str ++ "\e[0m").  % Dark grey for outer border

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Start an LFE REPL with project apps loaded",

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
         "Disable rlwrap integration"},
        {rlwrap_active, undefined, "rlwrap-active", boolean,
         "Internal flag - indicates REPL is already running under rlwrap"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe repl"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    ?DEBUG("LFE REPL provider starting", []),

    %% Check if we should wrap with rlwrap
    {Opts, _Args} = rebar_state:command_parsed_args(State),

    %% Check for internal flag (prevents infinite loop)
    %% The flag is parsed into Opts as rlwrap_active
    case proplists:get_bool(rlwrap_active, Opts) of
        true ->
            %% Already under rlwrap, proceed normally
            ?DEBUG("Already running under rlwrap (--rlwrap-active detected)", []),
            do_repl(State, Opts);
        false ->
            %% Potentially trampoline through rlwrap
            maybe_trampoline_rlwrap(State, Opts)
    end.

-spec format_error(term()) -> iolist().
format_error({app_start_failed, App, Reason}) ->
    io_lib:format("Failed to start application ~s: ~p", [App, Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

%% Internal function that does the actual REPL work
-spec do_repl(rebar_state:t(), proplists:proplist()) ->
    {ok, rebar_state:t()}.
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

-spec maybe_trampoline_rlwrap(rebar_state:t(), proplists:proplist()) ->
    {ok, rebar_state:t()}.
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
            case r3lfe_rlwrap:is_under_rlwrap() of
                true ->
                    %% Already wrapped (shouldn't happen, but be safe)
                    do_repl(State, Opts);
                false ->
                    %% Need to trampoline!
                    case r3lfe_rlwrap:has_rlwrap() of
                        true ->
                            %% Ensure completion files exist
                            r3lfe_completion:ensure_files(),
                            r3lfe_rlwrap:trampoline_via_rlwrap(State, MergedOpts);
                        false ->
                            r3lfe_rlwrap:warn_no_rlwrap(),
                            do_repl(State, Opts)
                    end
            end
    end.

-spec merge_repl_opts(proplists:proplist(), proplists:proplist()) -> map().
merge_repl_opts(ConfigOpts, CmdOpts) ->
    %% Command line options take precedence
    maps:merge(
        maps:from_list(ConfigOpts),
        maps:from_list(CmdOpts)
    ).

-spec maybe_start_apps(map(), rebar_state:t()) -> ok.
maybe_start_apps(Opts, State) ->
    case maps:get(apps, Opts, undefined) of
        undefined ->
            %% Start project apps by default
            Apps = rebar_state:project_apps(State),
            lists:foreach(
                fun(AppInfo) ->
                    App = rebar_app_info:name(AppInfo),
                    case is_binary(App) of
                        true ->
                            start_app(binary_to_atom(App, utf8));
                        false ->
                            start_app(App)
                    end
                end,
                Apps
            );
        AppsStr ->
            %% Parse comma-separated list
            AppNames = string:split(AppsStr, ",", all),
            lists:foreach(
                fun(AppStr) ->
                    App = list_to_atom(string:trim(AppStr)),
                    start_app(App)
                end,
                AppNames
            )
    end.

-spec start_app(atom()) -> ok.
start_app(App) ->
    case application:ensure_all_started(App) of
        {ok, Started} ->
            ?DEBUG("Started applications: ~p", [Started]),
            ok;
        {error, Reason} ->
            ?WARN("Failed to start ~s: ~p", [App, Reason]),
            ok
    end.

-spec maybe_run_script(map()) -> ok.
maybe_run_script(Opts) ->
    case maps:get(script, Opts, undefined) of
        undefined ->
            ok;
        ScriptPath ->
            ?INFO("Running script: ~s", [ScriptPath]),
            case lfescript:run([ScriptPath]) of
                ok -> ok;
                {error, Reason} ->
                    ?WARN("Script failed: ~p", [Reason]),
                    ok
            end
    end.

-spec start_modern_repl(map(), rebar_state:t()) -> ok.
start_modern_repl(Opts, State) ->
    ?DEBUG("Starting modern REPL (OTP 26+)", []),

    %% Display LFE banner unless disabled
    NoBanner = maps:get(nobanner, Opts, false),
    case NoBanner of
        false ->
            io:put_chars(build_banner());
        true ->
            ok
    end,

    %% For OTP 26+, use shell:start_interactive/1
    ShellArgs = build_shell_args(Opts),

    %% Update state with shell configuration
    State1 = rebar_state:set(State, shell, ShellArgs),

    %% Use rebar3's shell provider infrastructure
    rebar_prv_shell:do(State1),

    ok.

-spec start_legacy_repl(map(), rebar_state:t()) -> ok.
start_legacy_repl(Opts, _State) ->
    ?DEBUG("Starting legacy REPL (OTP < 26)", []),

    %% For older OTP, use custom REPL starter
    ReplModule = maps:get(start_module, Opts, lfe_shell),
    NoBanner = maps:get(nobanner, Opts, false),

    %% Build banner
    Banner = case NoBanner of
        true -> "";
        false -> build_banner()
    end,

    %% Start LFE REPL
    case NoBanner of
        false ->
            io:put_chars(Banner);
        true ->
            ok
    end,

    %% Start the REPL module
    ReplModule:start(),

    ok.

-spec read_vm_args(string()) -> string() | undefined.
read_vm_args(VmArgsPath) ->
    case filelib:is_file(VmArgsPath) of
        true ->
            case file:read_file(VmArgsPath) of
                {ok, Content} ->
                    %% Parse vm.args file: filter comments and empty lines, join with spaces
                    Lines = binary:split(Content, <<"\n">>, [global, trim]),
                    Args = lists:filtermap(
                        fun(Line) ->
                            Trimmed = string:trim(Line),
                            case Trimmed of
                                <<>> -> false;
                                <<"#", _/binary>> -> false;
                                _ -> {true, binary_to_list(Trimmed)}
                            end
                        end,
                        Lines
                    ),
                    string:join(Args, " ");
                {error, Reason} ->
                    ?WARN("Failed to read vm.args file ~s: ~p", [VmArgsPath, Reason]),
                    undefined
            end;
        false ->
            ?WARN("vm.args file not found: ~s", [VmArgsPath]),
            undefined
    end.

-spec build_shell_args(map()) -> proplists:proplist().
build_shell_args(Opts) ->
    ReplModule = maps:get(start_module, Opts, lfe_shell),

    %% For OTP 26+, shell:start_interactive/1 is used which supports edline
    %% We pass the shell_args to tell it which shell module to use
    %% We always set nobanner to true since we display our own banner
    BaseArgs = [{shell_args, [{ReplModule, start, []}]},
                {nobanner, true}],

    %% Collect VM arguments from various sources
    VmArgs = case maps:get(vm_args, Opts, undefined) of
        undefined ->
            %% No vm_args file, check for erl option
            maps:get(erl, Opts, undefined);
        VmArgsPath ->
            %% Read vm.args file
            case read_vm_args(VmArgsPath) of
                undefined ->
                    %% Failed to read, fall back to erl option
                    maps:get(erl, Opts, undefined);
                FileArgs ->
                    %% If both vm_args and erl are provided, combine them
                    case maps:get(erl, Opts, undefined) of
                        undefined -> FileArgs;
                        ErlArgs -> FileArgs ++ " " ++ ErlArgs
                    end
            end
    end,

    %% Add VM args if present
    case VmArgs of
        undefined ->
            BaseArgs;
        Args ->
            [{erl_args, Args} | BaseArgs]
    end.

-spec build_banner() -> string().
build_banner() ->
    LfeVersion = lfe_version(),
    QuitMsg = "(abort with ^G)",
    W = 65,  % Total outer width

    %% Outer border (dark grey, double-line)
    OuterTop = ?RED("╔" ++ lists:duplicate(W, $═) ++ "╗") ++ "\n",
    OuterBot = ?RED("╚" ++ lists:duplicate(W, $═) ++ "╝") ++ "\n",
    OuterSide = ?RED("║"),

    %% Inner border (red, single-line) - width is outer width minus 4 (2 for outer borders, 2 for padding)
    InnerTop = OuterSide ++ " " ++ ?YLW("┌" ++ lists:duplicate(W-4, $─) ++ "┐") ++ " " ++ OuterSide ++ "\n",
    InnerBot = OuterSide ++ " " ++ ?YLW("└" ++ lists:duplicate(W-4, $─) ++ "┘") ++ " " ++ OuterSide ++ "\n",
    InnerSide = ?YLW("│"),

    %% Empty line at top
    BlankLine = OuterSide ++ " " ++ InnerSide ++
                lists:duplicate(W-4, $ ) ++ InnerSide ++ " " ++ OuterSide ++ "\n",

    %% Content lines with both borders (each line is W-2 chars wide inside inner border)
    Line1 = OuterSide ++ " " ++ InnerSide ++ ?GRN("     ..-~") ++ ?YLW(".~_") ++ ?GRN("~---..") ++
            "                                           " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line2 = OuterSide ++ " " ++ InnerSide ++ ?GRN("    (      ") ++ ?YLW("\\\\") ++ ?GRN("     )") ++
            "    A Lisp-2+ on the Erlang VM            " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line3 = OuterSide ++ " " ++ InnerSide ++ ?GRN("    |`-.._") ++ ?YLW("/") ++ ?GRN("_") ++ ?YLW("\\\\") ++ ?GRN("_.-':") ++
            "    Type " ++ ?GRN("(help)") ++ " for usage info.           " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line4 = OuterSide ++ " " ++ InnerSide ++ ?GRN("    |         ") ++ ?RED("g") ++ ?GRN(" |_ \\") ++
            "                                         " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line5 = OuterSide ++ " " ++ InnerSide ++ ?GRN("    |        ") ++ ?RED("n") ++ ?GRN("    | |") ++
            "  Docs: " ++ ?BLU("http://docs.lfe.io/") ++ "             " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line6 = OuterSide ++ " " ++ InnerSide ++ ?GRN("    |       ") ++ ?RED("a") ++ ?GRN("    / /") ++
            "   Source: " ++ ?BLU("http://github.com/lfe/lfe") ++ "     " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line7 = OuterSide ++ " " ++ InnerSide ++ ?GRN("     \\     ") ++ ?RED("l") ++ ?GRN("    |_/") ++
            "                                          " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line8 = OuterSide ++ " " ++ InnerSide ++ ?GRN("      \\   ") ++ ?RED("r") ++ ?GRN("     /") ++
            "      " ++ ?YLW("LFE") ++ " " ++ ?YLW_REG("v" ++ LfeVersion) ++ " " ++ QuitMsg ++
            "            " ++ InnerSide ++ " " ++ OuterSide ++ "\n",
    Line9 = OuterSide ++ " " ++ InnerSide ++ ?GRN("       `-") ++ ?RED("E") ++ ?GRN("___.-'") ++
            "                                             " ++ InnerSide ++ " " ++ OuterSide ++ "\n",

    "\n\n" ++ OuterTop ++ InnerTop ++ BlankLine ++ BlankLine ++
    Line1 ++ Line2 ++ Line3 ++ Line4 ++ Line5 ++ Line6 ++ Line7 ++ Line8 ++ Line9 ++
    BlankLine ++ BlankLine ++ InnerBot ++ OuterBot ++ "\n".

-spec lfe_version() -> string().
lfe_version() ->
    case application:get_key(lfe, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Starts an LFE REPL with the project and its dependencies on~n"
        "the code path. Project applications can optionally be started.~n"
        "~n"
        "Options:~n"
        "  --name NAME       Give the node a long name~n"
        "  --sname NAME      Give the node a short name~n"
        "  --apps APPS       Comma-separated list of apps to start~n"
        "  --script PATH     Script to run before REPL starts~n"
        "  --prompt PROMPT   Custom REPL prompt (use 'classic' for '> ')~n"
        "  --vm_args PATH    Path to vm.args file for VM configuration~n"
        "  --erl ARGS        Additional Erlang VM arguments~n"
        "~n"
        "Configuration via rebar.config:~n"
        "  {lfe, [{repl, [{start_module, Module},~n"
        "                 {nobanner, true},~n"
        "                 {prompt, \"custom> \"},~n"
        "                 {vm_args, \"config/vm.args\"}]}]}.~n"
        "~n"
        "VM Arguments:~n"
        "  You can provide VM arguments in three ways:~n"
        "  1. Via vm.args file (--vm_args or {vm_args, Path} in config)~n"
        "  2. Via --erl flag on command line~n"
        "  3. Via {erl, Args} in the repl config~n"
        "~n"
        "  If both vm_args and erl are specified, they will be combined.~n"
        "~n"
        "  IMPORTANT LIMITATION:~n"
        "  Since rebar3 lfe repl runs inside an already-running rebar3 VM,~n"
        "  only certain VM flags work. Boot-time flags like -kernel, -prompt,~n"
        "  and shell history settings MUST be set via ERL_AFLAGS before~n"
        "  starting rebar3, or they will be ignored.~n"
        "~n"
        "  Flags that WORK in vm.args:~n"
        "    +pc unicode          - Character encoding~n"
        "    +A 10                - Async thread pool size~n"
        "    +K true              - Kernel poll~n"
        "    -setcookie mycookie  - Distributed Erlang cookie~n"
        "    -sname nodename      - Node name~n"
        "~n"
        "  Flags that DON'T WORK (use ERL_AFLAGS instead):~n"
        "    -kernel shell_history enabled~n"
        "    -kernel shell_history_path '\"~~/.lfe_history\"'~n"
        "    -prompt 'lfe> '~n"
        "~n"
        "  Example vm.args file (comments start with ~~):~n"
        "    ~~ Runtime VM configuration~n"
        "    +pc unicode~n"
        "    +A 10~n"
        "    +K true~n"
        "~n"
        "  For shell history and custom prompts, use ERL_AFLAGS:~n"
        "    export ERL_AFLAGS='-kernel shell_history enabled'~n"
        "    rebar3 lfe repl~n"
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
        "Prompt Customization:~n"
        "  NOTE: Custom prompts require modifying LFE's lfe_shell module~n"
        "  or starting Erlang with -prompt before running rebar3.~n"
        "~n"
        "  The --erl and --prompt options are reserved for future use~n"
        "  when LFE supports runtime prompt configuration.~n"
        "~n"
        "  Current workaround for colored prompts (use $'...' for escapes):~n"
        "  export ERL_AFLAGS=$'-prompt \\033[1;32mlfe\\033[0m\\033[33m>\\033[0m '~n"
        "  rebar3 lfe repl~n"
        "~n"
        "  Or in one line:~n"
        "  ERL_AFLAGS=$'-prompt \\033[1;32mlfe\\033[0m\\033[33m>\\033[0m ' rebar3 lfe repl~n"
        "~n"
        "  WARNING: Using ERL_AFLAGS with -prompt disables readline/edline!~n"
        "  On OTP 26+, you must choose between colored prompts OR line editing.~n"
        "  For the best editing experience, skip the custom prompt.~n"
        "~n"
        "  The $'...' syntax enables bash escape sequence interpretation.~n"
        "  ANSI codes: \\033[1;32m=bright green, \\033[33m=dark yellow,~n"
        "              \\033[1;33m=bright yellow, \\033[31m=red, \\033[0m=reset~n",
        [Description]
    ).
