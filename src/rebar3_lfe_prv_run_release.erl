-module(rebar3_lfe_prv_run_release).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_lfe.hrl").

-define(PROVIDER, 'run-release').
-define(NAMESPACE, lfe).
%% Re-examine the DEPS definition once the following ticket is addressed:
%% * https://github.com/lfe-rebar3/rebar3_lfe/issues/21
-define(DEPS, [compile]).
-define(DEFAULT_RELEASE_DIR, "rel").

%% =============================================================================
%% Plugin API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description = "Run an LFE release",
  Provider = providers:create([
      {namespace,  ?NAMESPACE},
      {name,       ?PROVIDER},
      {module,     ?MODULE},
      {bare,       true},
      {deps,       ?DEPS},
      {example,    "rebar3 lfe run-release COMMAND"},
      {opts,       []},
      {short_desc, Description},
      {desc,       info(Description)}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    run(State),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(release_script_not_found) ->
    "The release script was not found; be sure to run rebar3 release first.";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "This runs an LFE release project's release script, taking the~n"
        "same arguments that the generated release script takes, with~n"
        "COMMAND being any of the supported non-interactive release script~n"
        "commands.~n",
        [Description]).

%% =============================================================================
%% Internal functions
%% =============================================================================

run(State) ->
    Path = rebar_state:dir(State),
    rebar_api:debug("Path: ~p~n", [Path]),
    Args = rebar_state:command_args(State),
    rebar_api:debug("Args: ~p~n", [Args]),
    ReleaseScript = release_script(State),
    case filelib:is_file(ReleaseScript) of
        true ->
            run_release_script(ReleaseScript, Args);
        false ->
            rebar_api:debug("Release script not found: ~s", [ReleaseScript]),
            throw(?PRV_ERROR(release_script_not_found))
    end.


% %% =============================================================================
% %% Internal functions
% %% =============================================================================

run_release_script(ReleaseScript, Args) ->
    Cmd = string:join([ReleaseScript | Args], " "),
    rebar_api:debug("Cmd: ~p~n", [Cmd]),
    Result = os:cmd(Cmd),
    io:format("~s~n", [Result]).

release_name(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    rebar_api:debug("RelxConfig: ~p~n", [RelxConfig]),
    {release, {ReleaseName, _ReleaseVersion}, _Apps} = proplists:lookup(release, RelxConfig),
    rebar_api:debug("ReleaseName: ~p~n", [ReleaseName]),
    atom_to_list(ReleaseName).

output_dir(State) ->
    filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR).

release_script(State) ->
    Name = release_name(State),
    ReleaseDir = output_dir(State),
    rebar_api:debug("ReleaseDir: ~p~n", [ReleaseDir]),
    filename:join([ReleaseDir, Name, "bin", Name]).
