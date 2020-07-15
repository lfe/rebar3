-module(rebar3_lfe_prv_run).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, run).
-define(NAMESPACE, lfe).
-define(DEPS, [compile]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Opts     = [ { main, $m, "main", string
               , "Provide project file that contains a main function"
               }
             ],
  Description = "Run the project's main function.",
  Provider = providers:create([ 
      {namespace, ?NAMESPACE},
      {name, ?PROVIDER},
      {module, ?MODULE},
      {bare, true},
      {deps, ?DEPS},
      {example, "rebar3 lfe run -m scripts/main.lfe -- 1 2 5"},
      {opts, Opts},
      {short_desc, Description},
      {desc, info(Description)}
    ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  run(State),
  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

info(Description) ->
  io_lib:format(
        "~n~s~n"
        "~n"
        "Calls the main function in the file specified in '{lfe, ... {main, X}}'~n"
        "in rebar.config, which can be overriden using the --main option.~n"
        "Arguments to the function can be provided after --.~n",
        [Description]).

-spec run(rebar_state:t()) -> ok.
run(State) ->
  rebar_api:debug("\tRunning LFE via main function ...", []),
  MainFile = main_file(State),
  rebar_api:debug("\tmain file: ~p", [MainFile]),
  %% Get main arguments
  RawArgs = rebar_state:command_args(State),
  rebar_api:debug("\t\tRaw args: ~p", [RawArgs]),
  Args = parse_args(RawArgs),
  rebar_api:debug("\t\tArgs: ~p", [Args]),
  lfescript:run([MainFile | Args]),
  ok.

-spec parse_args([string()]) -> [binary()].
parse_args(RawArgs) ->
  case lists:dropwhile(fun(X) -> X =/= "--" end, RawArgs) of
    [] -> [];
    [_ | Args] ->
      [rebar_utils:to_binary(X) || X <- Args]
  end.

find_main_option(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(main, Opts, no_value).

find_main_rebar(State) ->
    LfeConfig = rebar3_lfe_utils:lfe_config(State),
    rebar_api:debug("\tLFE config values: ~p", [LfeConfig]),
    proplists:get_value(main, LfeConfig).

main_file(State) ->
    case rebar3_lfe_utils:first_value([fun find_main_option/1,
                                       fun find_main_rebar/1], State) of
        no_value ->
            rebar_api:debug("No script_file specified.", []),
            ok;
        "none" ->
            rebar_api:debug("Shell script execution skipped (--script none).", []),
            ok;
        RelFile ->
            filename:absname(RelFile)
    end.
