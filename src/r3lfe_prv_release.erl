-module(r3lfe_prv_release).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

%% For testing
-ifdef(TEST).
-export([
    get_release_name/1,
    get_release_output_dir/1,
    update_app_files/1,
    update_app_file/1,
    show_usage_info/1,
    info/1
]).
-endif.

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, release).
-define(DEPS, [{?NAMESPACE, compile}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Build an LFE release",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe release"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE release provider starting", []),

    %% Update app files with modules list
    update_app_files(State),

    %% Delegate to rebar3's release provider
    try
        rebar_relx:do(rlx_prv_release, "release", release, State),

        %% Show helpful usage information
        show_usage_info(State),

        {ok, State}
    catch
        error:undef ->
            %% Try older API
            rebar_relx:do(release, State),
            show_usage_info(State),
            {ok, State}
    end.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec update_app_files(rebar_state:t()) -> ok.
update_app_files(State) ->
    %% Get all apps
    Apps = rebar_state:project_apps(State),

    %% Update each app file
    lists:foreach(fun update_app_file/1, Apps),

    ok.

-spec update_app_file(rebar_app_info:t()) -> ok.
update_app_file(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    AppFile = rebar_app_info:app_file(AppInfo),

    %% Find all beam files
    BeamFiles = filelib:wildcard(filename:join(EbinDir, "*.beam")),

    %% Extract module names
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- BeamFiles],

    %% Read current app file
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppData}]} ->
            %% Update modules list
            AppData1 = lists:keystore(modules, 1, AppData, {modules, Modules}),

            %% Write updated app file
            Content = io_lib:format("~p.~n", [{application, AppName, AppData1}]),

            ok = file:write_file(AppFile, Content),

            ?DEBUG("Updated ~s with ~p modules", [AppFile, length(Modules)]),
            ok;

        {error, Reason} ->
            ?WARN("Failed to read app file ~s: ~p", [AppFile, Reason]),
            ok
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Builds a release for the LFE project using relx.~n"
        "~n"
        "Requires relx configuration in rebar.config:~n"
        "  {relx, [~n"
        "    {release, {myapp, \"0.1.0\"}, [myapp]},~n"
        "    {dev_mode, false},~n"
        "    {include_erts, true}~n"
        "  ]}.~n",
        [Description]
    ).

%% @doc Show usage information after successful build
-spec show_usage_info(rebar_state:t()) -> ok.
show_usage_info(State) ->
    ReleaseName = get_release_name(State),
    ReleaseDir = get_release_output_dir(State),

    ?INFO("~n", []),
    ?INFO("Release built successfully!", []),
    ?INFO("~n", []),
    ?INFO("To run the release:", []),
    ?INFO("  rebar3 lfe run-release start      # Start in background", []),
    ?INFO("  rebar3 lfe run-release console    # Start with console", []),
    ?INFO("  rebar3 lfe run-release foreground # Start in foreground", []),
    ?INFO("~n", []),
    ?INFO("Or run directly:", []),
    ?INFO("  ~s/~s/bin/~s start", [ReleaseDir, ReleaseName, ReleaseName]),
    ?INFO("~n", []),
    ok.

%% @doc Get release name from relx config
-spec get_release_name(rebar_state:t()) -> string().
get_release_name(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    case proplists:lookup(release, RelxConfig) of
        {release, {Name, _Version}, _Apps} when is_atom(Name) ->
            atom_to_list(Name);
        {release, {Name, _Version}, _Apps} when is_list(Name) ->
            Name;
        none ->
            case rebar_state:project_apps(State) of
                [AppInfo | _] ->
                    Name = rebar_app_info:name(AppInfo),
                    case is_binary(Name) of
                        true -> binary_to_list(Name);
                        false when is_atom(Name) -> atom_to_list(Name);
                        false when is_list(Name) -> Name
                    end;
                [] ->
                    "myapp"
            end
    end.

%% @doc Get release output directory
-spec get_release_output_dir(rebar_state:t()) -> file:filename().
get_release_output_dir(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    CustomDir = proplists:get_value(output_dir, RelxConfig, undefined),
    case CustomDir of
        undefined ->
            filename:join(rebar_dir:base_dir(State), "rel");
        Dir ->
            case filename:pathtype(Dir) of
                absolute -> Dir;
                relative -> filename:join(rebar_dir:base_dir(State), Dir)
            end
    end.
