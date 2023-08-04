-module(rebar3_lfe_prv_repl).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_lfe.hrl").

-define(PROVIDER, repl).
-define(DEPS, [compile]).
-define(DEFAULT_CFG,
        #{nobanner => false,
          version => rebar3_lfe_repl:lfe_version(),
          quit_message => rebar3_lfe_repl:quit_message(),
          banner_template => ""
         }).
%% ===================================================================
%% Public API
%% ===================================================================

init(State) ->
    Description = "Run an LFE REPL with project apps and deps in path.",
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                {namespace, ?NAMESPACE},
                {name, ?PROVIDER},
                {module, ?MODULE},
                {bare, true},
                {deps, ?DEPS},
                {example, "rebar3 lfe repl"},
                {short_desc, Description},
                {desc, info(Description)},
                {opts, opts()}
            ])
    ),
    {ok, State1}.

do(Config) ->
    {ok, repl(Config)}.

format_error({unknown_app, Unknown}) ->
    io_lib:format("Applications list for repl contains an unrecognizable application definition: ~p", [Unknown]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

opts() ->
    [
      {name, undefined, "name", atom,
        "Gives a long name to the node."},
      {sname, undefined, "sname", atom,
        "Gives a short name to the node."},
      {setcookie, undefined, "setcookie", atom,
        "Sets the cookie if the node is distributed."},
      {script_file, undefined, "script", string,
        "Path to an escript file to run before "
        "starting the project apps. Defaults to "
        "rebar.config {shell, [{script_file, File}]} "
        "if not specified."},
      {apps, undefined, "apps", string,
        "A list of apps to boot before starting the "
        "REPL. (E.g. --apps app1,app2,app3) Defaults "
        "to rebar.config {shell, [{apps, Apps}]} or "
        "relx apps if not specified."},
      {relname, $r, "relname", atom,
        "Name of the release to use as a template for the "
        "REPL session"},
      {relvsn, $v, "relvsn", string,
        "Version of the release to use for the shell "
        "session"},
      {start_clean, undefined, "start_clean", boolean,
        "Cancel any applications in the 'apps' list "
        "or release."},
      {env_file, undefined, "env_file", string,
        "Path to file of os environment variables to setup "
        "before expanding vars in config files."},
      {user_drv_args, undefined, "user_drv_args", string,
        "Arguments passed to user_drv start function for "
        "creating custom shells."}
      ].

info(Description) ->
  io_lib:format(
        "~n~s~n"
        "~n"
        "This is very simular as to what's done with:~n"
        "\t'lfe -pa ebin -pa deps/*/ebin'.~n",
        [Description]).

repl(State) ->
    rebar_api:debug("\tStarting LFE REPL ...", []),
    rebar_paths:set_paths([deps, plugins], State),
    LfeCfg = rebar_state:get(State, lfe, []),
    rebar_api:debug("\t\tLFECfg: ~p", [LfeCfg]),
    ReplCfg = make_config(LfeCfg),
    ShellArgs = set_shell_args(ReplCfg),
    rebar_api:debug("\t\tShellArgs config: ~p", [ShellArgs]),
    State1 = rebar_state:set(State, shell, ShellArgs),
    rebar_api:debug("\t\tCalling underlying rebar3 shell 'do' function ...", []),
    rebar_prv_shell:do(State1),
    State1.

set_shell_args(ReplCfg) ->
    rebar_api:debug("\t\tGetting REPL config ...", []),
    rebar_api:debug("\t\tUsing REPL config: ~p", [ReplCfg]),
    OTPRelease = erlang:system_info(otp_release),
    if OTPRelease >= "26" ->
            rebar_api:debug("\t\tRunning newer Erlang ...", []),
            [{shell_args, #{initial_shell => {rebar3_lfe_repl,start,[ReplCfg]}}}];
       true ->
            [{shell_args, ['tty_sl -c -e',{rebar3_lfe_repl,start,[ReplCfg]}]}]
    end.

make_config(Parsed) ->
    maps:merge(?DEFAULT_CFG, maps:from_list(proplists:get_value(repl, Parsed, []))).
