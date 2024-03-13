-module(rebar3_lfe_prv_repl).

-export([init/1,
         do/1,
         format_error/1]).

-export([repl/1]).

-include("rebar3_lfe.hrl").

-define(PROVIDER, repl).
-define(DEPS, [compile]).
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
      {start_module, $s, "start_module", atom,
        "The module to use to override the default shell; "
        "must define a start/0."},
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
    ReplOpts = cfg_opts(LfeCfg),
    rebar_api:debug("\t\tReplOpts: ~p", [ReplOpts]),
    {PluginOpts, _} = rebar_state:command_parsed_args(State),
    Overrides = make_opts(PluginOpts),
    rebar_api:debug("\t\tOverrides: ~p", [Overrides]),
    Opts = maps:merge(ReplOpts, Overrides),
    rebar_api:debug("\t\tOpts: ~p", [Opts]),
    OTPRelease = erlang:system_info(otp_release),
    if OTPRelease >= "26" ->
            rebar_api:debug("\t\tRunning newer Erlang ...", []),
            ShellArgs  = [{shell_args, [{rebar3_lfe_repl,start,[Opts]}]}],
            State1 = rebar_state:set(State, shell, ShellArgs),
            shell(State1),
            State1;
       true ->
            ShellArgs = [{shell_args, ['tty_sl -c -e',{rebar3_lfe_repl,start,[Opts]}]}],
            rebar_api:debug("\t\tShellArgs config: ~p", [ShellArgs]),
            State1 = rebar_state:set(State, shell, ShellArgs),
            rebar_api:debug("\t\tCalling underlying rebar3 shell 'do' function ...", []),
            rebar_prv_shell:do(State1),
            State1
    end.

cfg_opts(Parsed) ->
    make_opts(proplists:get_value(repl, Parsed, [])).

make_opts(Proplist) ->
    maps:from_list(Proplist).

%%% With Erlang 26.0 and rebar3 3.22.1+, the LFE REPL no longer loads. This is
%%% due to the following change combined with the changes in Erlang 26:
%%% * https://github.com/erlang/rebar3/commit/5944a8ed870417403863144c770307d45e6edaf6
%%%
%%% The rebar3_lfe ticket that summarised the current state:
%%% * https://github.com/lfe/rebar3/issues/79
%%%
%%% As an attempt to work around this, code from rebar_prv_shell has been
%%% pasted below; if 1) the root cause is ever addressed there, or 2) we figure
%%% out the correst way to start up the LFE REPL in the rebar3 process, then
%%% we can delete the code pasted below.
%%%
%%% -author("Kresten Krab Thorup <krab@trifork.com>").
%%% -author("Fred Hebert <mononcqc@ferd.ca>").

shell(State) ->
    setup_name(State),
    setup_paths(State),
    ShellArgs = debug_get_value(shell_args, rebar_state:get(State, shell, []), undefined,
                                "Found shell args from command line option or plugin."),
    setup_shell(ShellArgs),
    maybe_run_script(State),
    %% apps must be started after the change in shell because otherwise
    %% their application masters never gets the new group leader (held in
    %% their internal state)
    maybe_boot_apps(State),
    simulate_proc_lib(),
    maybe_run_eval(State),
    true = register(rebar_agent, self()),
    {ok, GenState} = rebar_agent:init(State),
    %% Hack to fool the init process into thinking we have stopped and the normal
    %% node start process can go on. Without it, init:get_status() always return
    %% '{starting, started}' instead of '{started, started}'
    init ! {'EXIT', self(), normal},
    gen_server:enter_loop(rebar_agent, [], GenState, {local, rebar_agent}, hibernate).

setup_shell(ShellArgs) ->
    rebar_api:debug("Setting up shell ...", []),
    code:ensure_loaded(shell),
    case erlang:function_exported(shell, start_interactive, 0) of
        false ->
            LoggerState = maybe_remove_logger(),
            OldUser = maybe_kill_old_user(),
            %% Test for support here
            NewUser = try erlang:open_port({spawn,"tty_sl -c -e"}, []) of
                          Port when is_port(Port) ->
                              true = port_close(Port),
                              rebar_api:debug("*** Getting ready to setup new shell ...", []),
                              setup_new_shell(ShellArgs)
                      catch
                          error:X ->
                              rebar_api:debug("*** Error: ~p", [X]),
                              rebar_api:debug("*** Getting ready to setup old shell ...", []),
                              setup_old_shell()
                      end,
            rewrite_leaders(OldUser, NewUser),
            maybe_reset_logger(LoggerState);
        true ->
            rebar_api:debug("*** Getting ready to start interactive ...", []),
            ok = start_interactive(ShellArgs)
    end.

start_interactive(ShellArgs) ->
    rebar_api:debug("*** Starting the shell w/Erlang apply ...", []),
    rebar_api:debug("(using args: ~p", [ShellArgs]),
    apply(shell, start_interactive, ShellArgs).

maybe_remove_logger() ->
    case erlang:function_exported(logger, module_info, 0) of
        false ->
            ignore;
        true ->
            {ok, Cfg} = logger:get_handler_config(default),
            logger:remove_handler(default),
            {restart, Cfg}
    end.

maybe_reset_logger(ignore) ->
    ok;
maybe_reset_logger({restart, Config = #{module := Mod}}) ->
    logger:add_handler(default, Mod, Config).

maybe_kill_old_user() ->
    OldUser = whereis(user),
    rebar_api:debug("*** OldUser: ~p", [OldUser]),
    %% terminate the current user's port, in a way that makes it shut down,
    %% but without taking down the supervision tree so that the escript doesn't
    %% fully die
    PortResult = [P || P <- element(2,process_info(whereis(user), links)), is_port(P)],
    case PortResult of
        [P] ->
            user ! {'EXIT', P, normal}, % pretend the port died, then the port can die!
            exit(P, kill),
            wait_for_port_death(1000, P);
        _ ->
            rebar_api:debug("No port to kill ...", [])
    end,
    OldUser.

wait_for_port_death(N, _) when N < 0 ->
    %% This risks displaying a warning!
    whatever;
wait_for_port_death(N, P) ->
    case erlang:port_info(P) of
        undefined ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_port_death(N-10, P)
    end.

setup_new_shell(ShellArgs) ->
    %% terminate the current user supervision structure, if any
    _ = supervisor:terminate_child(kernel_sup, user),
    %% start a new shell (this also starts a new user under the correct group)
    case ShellArgs of
        undefined ->
            _ = user_drv:start();
        _ ->
            _ = user_drv:start(ShellArgs)
    end,
    %% wait until user_drv and user have been registered (max 3 seconds)
    ok = wait_until_user_started(3000),
    whereis(user).

setup_old_shell() ->
    %% scan all processes for any with references to the old user and save them to
    %% update later
    NewUser = rebar_user:start(), % hikack IO stuff with fake user
    NewUser = whereis(user),
    NewUser.

rewrite_leaders(OldUser, NewUser) ->
    %% set any process that had a reference to the old user's group leader to the
    %% new user process. Catch the race condition when the Pid exited after the
    %% liveness check.
    _ = [catch erlang:group_leader(NewUser, Pid)
         || Pid <- erlang:processes(),
            [_|_] = Info <- [erlang:process_info(Pid)],
            proplists:get_value(group_leader, Info) == OldUser,
            is_process_alive(Pid)],
    %% Application masters have the same problem, but they hold the old group
    %% leader in their state and hold on to it. Re-point the processes whose
    %% leaders are application masters. This can mess up a few things around
    %% shutdown time, but is nicer than the current lock-up.
    OldMasters = [Pid
         || Pid <- erlang:processes(),
            Pid < NewUser, % only change old masters
            {_,Dict} <- [erlang:process_info(Pid, dictionary)],
            {application_master,init,4} == proplists:get_value('$initial_call', Dict)],
    _ = [catch erlang:group_leader(NewUser, Pid)
         || Pid <- erlang:processes(),
            lists:member(proplists:get_value(group_leader, erlang:process_info(Pid)),
                         OldMasters)],
    
        case erlang:function_exported(logger, module_info, 0) of
            false ->
                %% Old style logger had a lock-up issue and other problems related
                %% to group leader handling.
                %% enable error_logger's tty output
                error_logger:swap_handler(tty),
                %% disable the simple error_logger (which may have been added
                %% multiple times). removes at most the error_logger added by
                %% init and the error_logger added by the tty handler
                remove_error_handler(3),
                %% reset the tty handler once more for remote shells
                error_logger:swap_handler(tty);
            true ->
                %% This is no longer a problem with the logger interface
                ok
        end.

setup_paths(State) ->
    %% Add deps to path
    code:add_pathsa(rebar_state:code_paths(State, all_deps)),
    %% add project app test paths
    ok = add_test_paths(State).

maybe_run_script(State) ->
    case first_value([fun find_script_option/1,
                      fun find_script_rebar/1], State) of
        no_value ->
            rebar_api:debug("No script_file specified.", []),
            ok;
        "none" ->
            rebar_api:debug("Shell script execution skipped (--script none).", []),
            ok;
        RelFile ->
            File = filename:absname(RelFile),
            run_script_file(File)
    end.

-spec find_script_option(rebar_state:t()) -> no_value | list().
find_script_option(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    debug_get_value(script_file, Opts, no_value,
                    "Found script file from command line option.").

-spec find_script_rebar(rebar_state:t()) -> no_value | list().
find_script_rebar(State) ->
    Config = rebar_state:get(State, shell, []),
    %% Either a string, or undefined
    debug_get_value(script_file, Config, no_value,
                    "Found script file from rebar config file.").

run_script_file(File) ->
    rebar_api:debug("Extracting escript from ~p", [File]),
    {ok, Script} = escript:extract(File, [compile_source]),
    Beam = proplists:get_value(source, Script),
    Mod = proplists:get_value(module, beam_lib:info(Beam)),
    rebar_api:debug("Compiled escript as ~p", [Mod]),
    FakeFile = "/fake_path/" ++ atom_to_list(Mod),
    {module, Mod} = code:load_binary(Mod, FakeFile, Beam),
    rebar_api:debug("Evaling ~p:main([]).", [Mod]),
    Result = Mod:main([]),
    rebar_api:debug("Result: ~p", [Result]),
    Result.

maybe_boot_apps(State) ->
    _ = maybe_set_env_vars(State),
    case find_apps_to_boot(State) of
        undefined ->
            %% try to read in sys.config file
            ok = reread_config([], State);
        Apps ->
            %% load apps, then check config, then boot them.
            load_apps(Apps),
            ok = reread_config(Apps, State),
            ShellOpts = rebar_state:get(State, shell, []),
            BootLogLevel = debug_get_value(log, ShellOpts, info,
                "Found boot log verbosity mode from config."),
            boot_apps(Apps, BootLogLevel)
    end.

simulate_proc_lib() ->
    FakeParent = spawn_link(fun() -> timer:sleep(infinity) end),
    put('$ancestors', [FakeParent]),
    put('$initial_call', {rebar_agent, init, 1}).

maybe_run_eval(State) ->
    Exprs = find_evals_to_run(State),
    lists:map(fun(Expr) ->
        rebar_api:info("Evaluating: ~p",[Expr]),
        {ok, _} = eval(Expr)
    end, Exprs).

find_evals_to_run(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    debug_get_all_values(eval, Opts,
        "Found shell evals from command line option.").

eval(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    case erl_parse:parse_exprs(Tokens) of
        {error, {_, _, Msg}} ->
            throw(?PRV_ERROR({eval_parse, Expression, Msg}));
        {ok, Parsed} ->
            try erl_eval:exprs(Parsed, []) of
                {value, Result, _} ->
                    {ok, Result}
            catch
                C:E:S ->
                    throw(?PRV_ERROR({eval_exprs, Expression, {C, E, S}}))
            end
    end.

setup_name(State) ->
    {Long, Short, Opts} = rebar_dist_utils:find_options(State),
    rebar_dist_utils:either(Long, Short, Opts).

find_apps_to_boot(State) ->
    %% Try the shell_apps option
    case first_value([fun find_apps_option/1,
                      fun find_apps_rebar/1,
                      fun find_apps_relx/1], State) of
        no_value ->
            undefined;
        Apps ->
            Apps
    end.

-spec find_apps_option(rebar_state:t()) -> no_value | [atom()].
find_apps_option(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case debug_get_value(apps, Opts, no_value,
                         "Found shell apps from command line option.") of
        no_value ->
            case debug_get_value(start_clean, Opts, false,
                                "Found start-clean argument to disable apps") of
                false -> no_value;
                true -> []
            end;
        AppsStr ->
            [ list_to_atom(AppStr)
              || AppStr <- rebar_string:lexemes(AppsStr, " ,:") ]
    end.

-spec find_apps_rebar(rebar_state:t()) -> no_value | list().
find_apps_rebar(State) ->
    ShellOpts = rebar_state:get(State, shell, []),
    debug_get_value(apps, ShellOpts, no_value,
                    "Found shell opts from command line option.").

-spec find_apps_relx(rebar_state:t()) -> no_value | list().
find_apps_relx(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    RelxOpts = rebar_state:get(State, relx, []),
    {Defname, Defvsn} = debug_get_value(default_release, RelxOpts,
                                        {undefined, undefined},
                                        "Found default release from config"),
    Relname = debug_get_value(relname, Opts, Defname,
                              "Found relname from command line option"),
    Relvsn = debug_get_value(relvsn, Opts, Defvsn,
                             "Found relvsn from command line option"),
    Releases = [Rel || Rel <- rebar_state:get(State, relx, []),
                       is_tuple(Rel), element(1, Rel) =:= release,
                       tuple_size(Rel) =:= 3 orelse tuple_size(Rel) =:= 4,
                       {Name, Vsn} <- [element(2, Rel)],
                       Relname == undefined orelse Name == Relname,
                       Relvsn == undefined orelse Vsn == Relvsn],
    case Releases of
        [] ->
            no_value;
        [{_, _, Apps}|_] ->
            rebar_api:debug("Found shell apps from relx.", []),
            Apps;
        [{_, _, Apps, _}|_] ->
            rebar_api:debug("Found shell apps from relx.", []),
            Apps
    end.

load_apps(Apps) ->
    [case application:load(App) of
        ok ->
             {ok, Ks} = application:get_all_key(App),
             load_apps(proplists:get_value(applications, Ks));
        _ ->
            error % will be caught when starting the app
     end || App <- normalize_load_apps(Apps),
            not lists:keymember(App, 1, application:loaded_applications())],
    ok.

reread_config(AppsToStart, State) ->
    case find_config(State) of
        no_config ->
            ok;
        ConfigList ->
            %% This allows people who use applications that are also
            %% depended on by rebar3 or its plugins to change their
            %% configuration at runtime based on the configuration files.
            %%
            %% To do this, we stop apps that are already started before
            %% reloading their configuration.
            %%
            %% We make an exception for apps that:
            %%  - are not already running
            %%  - would not be restarted (and hence would break some
            %%    compatibility with rebar3)
            %%  - are not in the config files and would see no config
            %%    changes
            %%  - are not in a blacklist, where changing their config
            %%    would be risky to the shell or the rebar3 agent
            %%    functionality (i.e. changing inets may break proxy
            %%    settings, stopping `kernel' would break everything)
            Running = [App || {App, _, _} <- application:which_applications()],
            BlackList = [inets, stdlib, kernel, rebar],
            _ = [application:stop(App)
                 || Config <- ConfigList,
                    {App, _} <- Config,
                    lists:member(App, Running),
                    lists:member(App, AppsToStart),
                    not lists:member(App, BlackList)],
            _ = rebar_utils:reread_config(ConfigList, [update_logger]),
            ok
    end.

boot_apps(Apps, BootLogLevel) ->
    Normalized = normalize_boot_apps(Apps),
    Res = [application:ensure_all_started(App) || App <- Normalized],
    print_booted([App || {ok, Booted} <- Res, App <- Booted], BootLogLevel),
    %% errors are not suppressed
    _ = [rebar_api:error("Failed to boot ~p for reason ~p", [App, Reason])
            || {error, {App, Reason}} <- Res],
    ok.

print_booted(Booted, debug) ->
    _ = [rebar_api:debug("Booted ~p", [App]) || App <- Booted];
print_booted(Booted, info) ->
    _ = [rebar_api:info("Booted ~p", [App]) || App <- Booted].

normalize_load_apps([]) -> [];
normalize_load_apps([{_App, none} | T]) -> normalize_load_apps(T);
normalize_load_apps([{App, _} | T]) -> [App | normalize_load_apps(T)];
normalize_load_apps([{App, _Vsn, load} | T]) -> [App | normalize_load_apps(T)];
normalize_load_apps([{_App, _Vsn, none} | T]) -> normalize_load_apps(T);
normalize_load_apps([{App, _Vsn, Operator} | T]) when is_atom(Operator) ->
    [App | normalize_load_apps(T)];
normalize_load_apps([App | T]) when is_atom(App) -> [App | normalize_load_apps(T)];
normalize_load_apps([Unknown | _]) ->
    erlang:error(?PRV_ERROR({unknown_app, Unknown})).

normalize_boot_apps([]) -> [];
normalize_boot_apps([{_App, load} | T]) -> normalize_boot_apps(T);
normalize_boot_apps([{_App, _Vsn, load} | T]) -> normalize_boot_apps(T);
normalize_boot_apps([{_App, none} | T]) -> normalize_boot_apps(T);
normalize_boot_apps([{_App, _Vsn, none} | T]) -> normalize_boot_apps(T);
normalize_boot_apps([{App, _Vsn, Operator} | T]) when is_atom(Operator) ->
    [App | normalize_boot_apps(T)];
normalize_boot_apps([{App, _Vsn} | T]) -> [App | normalize_boot_apps(T)];
normalize_boot_apps([App | T]) when is_atom(App) -> [App | normalize_boot_apps(T)];
normalize_boot_apps([Unknown | _]) ->
    erlang:error(?PRV_ERROR({unknown_app, Unknown})).

remove_error_handler(0) ->
    rebar_api:warn("Unable to remove simple error_logger handler", []);
remove_error_handler(N) ->
    case gen_event:delete_handler(error_logger, error_logger, []) of
        {error, module_not_found} -> ok;
        {error_logger, _} -> remove_error_handler(N-1)
    end.

%% Timeout is a period to wait before giving up
wait_until_user_started(0) ->
    rebar_api:abort("Timeout exceeded waiting for `user` to register itself", []),
    erlang:error(timeout);
wait_until_user_started(Timeout) ->
    case whereis(user) of
        %% if user is not yet registered wait a tenth of a second and try again
        undefined -> timer:sleep(100), wait_until_user_started(Timeout - 100);
        _ -> ok
    end.

add_test_paths(State) ->
    _ = [begin
            AppDir = rebar_app_info:out_dir(App),
            %% ignore errors resulting from non-existent directories
            _ = code:add_path(filename:join([AppDir, "test"]))
         end || App <- rebar_state:project_apps(State)],
    _ = code:add_path(filename:join([rebar_dir:base_dir(State), "test"])),
    ok.

% First try the --config flag, then try the relx sys_config
-spec find_config(rebar_state:t()) -> [[tuple()]] | no_config.
find_config(State) ->
    case first_value([fun find_config_option/1,
                      fun find_config_rebar/1,
                      fun find_config_relx/1], State) of
        no_value ->
            no_config;
        Filename when is_list(Filename) ->
            case is_src_config(Filename) of
                false ->
                    rebar_file_utils:consult_config(State, Filename);
                true ->
                    consult_env_config(State, Filename)
            end
    end.

-spec first_value([Fun], State) -> no_value | Value when
      Value :: any(),
      State :: rebar_state:t(),
      Fun :: fun ((State) -> no_value | Value).
first_value([], _) -> no_value;
first_value([Fun | Rest], State) ->
    case Fun(State) of
        no_value ->
            first_value(Rest, State);
        Value ->
            Value
    end.

debug_get_value(Key, List, Default, Description) ->
    case proplists:get_value(Key, List, Default) of
        Default -> Default;
        Value ->
            rebar_api:debug(Description, []),
            Value
    end.

debug_get_all_values(Key, List, Description) ->
    case proplists:get_all_values(Key, List) of
        [] -> [];
        Values ->
            rebar_api:debug(Description, []),
            Values
    end.

-spec find_config_option(rebar_state:t()) -> Filename::list() | no_value.
find_config_option(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    debug_get_value(config, Opts, no_value,
                    "Found config from command line option.").

-spec find_config_rebar(rebar_state:t()) -> [tuple()] | no_value.
find_config_rebar(State) ->
    debug_get_value(config, rebar_state:get(State, shell, []), no_value,
                    "Found config from rebar config file.").

-spec find_config_relx(rebar_state:t()) -> [tuple()] | no_value.
find_config_relx(State) ->
    %% The order in relx is to load the src version first;
    %% we do the same.
    RelxCfg = rebar_state:get(State, relx, []),
    Src = debug_get_value(sys_config_src, RelxCfg, no_value,
                          "Found config.src from relx."),
    case Src of
        no_value ->
            debug_get_value(sys_config, RelxCfg, no_value,
                            "Found config from relx.");
        _ ->
            Src
    end.

-spec is_src_config(file:filename()) -> boolean().
is_src_config(Filename) ->
    filename:extension(Filename) =:= ".src".

-spec consult_env_config(rebar_state:t(), file:filename()) -> [[tuple()]].
consult_env_config(State, Filename) ->
    RawString = case file:read_file(Filename) of
        {error, _} -> "[].";
        {ok, Bin} -> unicode:characters_to_list(Bin)
    end,
    ReplacedStr = replace_env_vars(RawString),
    case rebar_string:consult(unicode:characters_to_list(ReplacedStr)) of
        {error, Reason} ->
            throw(?PRV_ERROR({bad_term_file, Filename, Reason}));
        [Terms] ->
            rebar_file_utils:consult_config_terms(State, Terms)
    end.

maybe_set_env_vars(State) ->
    EnvFile =debug_get_value(env_file, rebar_state:get(State, shell, []), undefined,
                             "Found env_file from config."),
    {Opts, _} = rebar_state:command_parsed_args(State),
    EnvFile1 = debug_get_value(env_file, Opts, EnvFile,
                               "Found env_file from command line option."),

    case maybe_read_file(EnvFile1) of
        ignore ->
            ok;
        {error, _} ->
            rebar_api:warn("Failed to read file with environment variables: ~p", [EnvFile1]);
        {ok, Bin} ->
            Lines = re:split(unicode:characters_to_list(Bin), "\n|\r\n", [{return, list}]),
            [handle_env_var_line(Line) || Line <- Lines]
    end.

handle_env_var_line(Line) ->
    Trimmed = rebar_string:trim(Line, both, [$\s]),
    %% ignore lines starting with # and
    %% fail if there are spaces around =
    case re:run(Trimmed, "^(?<key>[^#][^\s=]*)=(?<value>[^\s]\.*)",
                [{capture, [key, value], list}, unicode]) of
        {match, [Key, Value]} ->
            os:putenv(Key, Value);
        _ ->
            case Trimmed of
                [$# | _] -> ignore;
                [] -> ignore;
                Other ->
                    rebar_api:warn("Unable to parse environment variable from this line: ~ts", [Other])
            end
    end.

maybe_read_file(undefined) ->
    ignore;
maybe_read_file(EnvFile) ->
    file:read_file(EnvFile).

%% @doc quick and simple variable substitution writeup.
%% Supports `${varname}' but not `$varname' nor nested
%% values such as `${my_${varname}}'.
%% The variable are also defined as only supporting
%% the form `[a-zA-Z_]+[a-zA-Z0-9_]*' as per the POSIX
%% standard.
-spec replace_env_vars(string()) -> unicode:charlist().
replace_env_vars("") -> "";
replace_env_vars("${" ++ Str) ->
    case until_var_end(Str) of
        {ok, VarName, Rest} ->
            replace_varname(VarName) ++ replace_env_vars(Rest);
        error ->
            "${" ++ replace_env_vars(Str)
    end;
replace_env_vars([Char|Str]) ->
    [Char | replace_env_vars(Str)].

until_var_end(Str) ->
    case re:run(Str, "([a-zA-Z_]+[a-zA-Z0-9_]*)}", [{capture, [1], list}]) of
        nomatch ->
            error;
        {match, [Name]} ->
            {ok, Name, drop_varname(Name, Str)}
    end.

replace_varname(Var) ->
    %% os:getenv(Var, "") is only available in OTP-18.0
    case os:getenv(Var) of
        false -> "";
        Val -> Val
    end.

drop_varname("", "}" ++ Str) -> Str;
drop_varname([_|Var], [_|Str]) -> drop_varname(Var, Str).
