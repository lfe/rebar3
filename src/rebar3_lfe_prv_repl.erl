-module(rebar3_lfe_prv_repl).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, repl).
-define(NAMESPACE, lfe).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
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

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Config) ->
    {ok, repl(Config)}.

-spec format_error(any()) -> iolist().
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
      {start_clean, undefined, "start-clean", boolean,
        "Cancel any applications in the 'apps' list "
        "or release."},
      {env_file, undefined, "env-file", string,
        "Path to file of os environment variables to setup "
        "before expanding vars in config files."}
      ].

info(Description) ->
  io_lib:format(
        "~n~s~n"
        "~n"
        "This is very simular as to what's done with:~n"
        "\t'lfe -pa ebin -pa deps/*/ebin'.~n",
        [Description]).

repl(Config) ->
    rebar_api:debug("\tStarting LFE REPL ...", []),
    rebar_api:debug("\t\tPlain args: ~p", [init:get_plain_arguments()]),
    rebar_api:debug("\t\tSetting shell args ...", []),
    Config1 = rebar_state:set(Config, shell, [{shell_args, ['tty_sl -c -e',{lfe_shell,start,[]}]}]),
    rebar_api:debug("\t\tCalling underlying rebar3 shell 'do' function ...", []),
    rebar_prv_shell:do(Config1),
    Config1.
