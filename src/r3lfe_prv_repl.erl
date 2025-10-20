-module(r3lfe_prv_repl).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, repl).
-define(DEPS, [{?NAMESPACE, compile}]).

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
         "Script to run before starting REPL"}
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

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE REPL provider starting", []),

    %% Set up code paths
    rebar_paths:set_paths([deps, plugins], State),

    %% Get REPL configuration
    {Opts, _Args} = rebar_state:command_parsed_args(State),
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

-spec format_error(term()) -> iolist().
format_error({app_start_failed, App, Reason}) ->
    io_lib:format("Failed to start application ~s: ~p", [App, Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

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

-spec build_shell_args(map()) -> proplists:proplist().
build_shell_args(Opts) ->
    ReplModule = maps:get(start_module, Opts, lfe_shell),
    NoBanner = maps:get(nobanner, Opts, false),

    [{shell_args, [{ReplModule, start, []}]},
     {nobanner, NoBanner}].

-spec build_banner() -> string().
build_banner() ->
    LfeVersion = lfe_version(),

    "\n"
    "   ..-~.~_~---..   \n"
    "  (      \\     )    |   A Lisp-2+ on the Erlang VM\n"
    "  |`-.._/_\\_.-':    |   Type (help) for usage info.\n"
    "  |         g |_ \\   |\n"
    "  |        n    | |  |   Docs: http://docs.lfe.io/\n"
    "  |       a    / /   |   Source: http://github.com/lfe/lfe\n"
    "   \\     l    |_/    |\n"
    "    \\   r     /      |   LFE v" ++ LfeVersion ++ "\n"
    "     `-E___.-'       \n\n".

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
        "~n"
        "Configuration via rebar.config:~n"
        "  {lfe, [{repl, [{start_module, Module},~n"
        "                 {nobanner, true}]}]}.~n",
        [Description]
    ).
