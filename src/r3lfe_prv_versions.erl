-module(r3lfe_prv_versions).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

%% Exported for testing
-ifdef(TEST).
-export([
    get_app_versions/1,
    get_language_versions/0,
    get_tool_versions/0,
    get_version/1,
    get_rebar3_version/0,
    get_deps_info/1,
    get_plugins_info/1,
    format_heading/1,
    display_versions/1,
    info/1
]).
-endif.

-include("r3lfe.hrl").

-define(PROVIDER, versions).
-define(DEPS, [{default, install_deps}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Display version information",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe versions"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Apps = rebar_state:project_apps(State),

    %% Collect version information
    VersionInfo = #{
        apps => get_app_versions(Apps),
        languages => get_language_versions(),
        tools => get_tool_versions(),
        deps => get_deps_info(State),
        plugins => get_plugins_info(State)
    },

    %% Display nicely formatted output
    display_versions(VersionInfo),

    {ok, State}.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_app_versions([rebar_app_info:t()]) -> [{atom(), string()}].
get_app_versions(Apps) ->
    lists:map(
        fun(AppInfo) ->
            Name = rebar_app_info:name(AppInfo),
            Vsn = rebar_app_info:original_vsn(AppInfo),

            AppName = case is_binary(Name) of
                true -> binary_to_atom(Name, utf8);
                false -> Name
            end,

            {AppName, Vsn}
        end,
        Apps
    ).

-spec get_language_versions() -> [{atom(), string()}].
get_language_versions() ->
    [
        {lfe, get_version(lfe)},
        {erlang, erlang:system_info(otp_release)},
        {erts, erlang:system_info(version)}
    ].

-spec get_tool_versions() -> [{atom(), string()}].
get_tool_versions() ->
    BaseTools = [
        {rebar3, get_rebar3_version()},
        {rebar3_lfe, get_version(rebar3_lfe)}
    ],
    %% Add rebar3_hex if it's available
    case get_version(rebar3_hex) of
        "unknown" -> BaseTools;
        Vsn -> BaseTools ++ [{rebar3_hex, Vsn}]
    end.

-spec get_deps_info(rebar_state:t()) -> [#{name => atom(), version => string(), profile => atom()}].
get_deps_info(State) ->
    AllDeps = r3lfe_util:get_all_deps_with_profiles(State),
    %% Filter out LFE (it's shown in Languages section)
    FilteredDeps = [D || D <- AllDeps, maps:get(name, D) =/= lfe],
    %% Sort alphabetically by name
    lists:sort(fun(#{name := N1}, #{name := N2}) -> N1 =< N2 end, FilteredDeps).

-spec get_plugins_info(rebar_state:t()) -> [#{name => atom(), version => string(), profile => atom()}].
get_plugins_info(State) ->
    AllPlugins = r3lfe_util:get_all_plugins_with_profiles(State),
    %% Filter out rebar3_lfe and rebar3_hex (they're in Build Tools)
    FilteredPlugins = [P || P <- AllPlugins,
                            maps:get(name, P) =/= rebar3_lfe,
                            maps:get(name, P) =/= rebar3_hex],
    %% Sort alphabetically by name
    lists:sort(fun(#{name := N1}, #{name := N2}) -> N1 =< N2 end, FilteredPlugins).

-spec get_version(atom()) -> string().
get_version(App) ->
    case r3lfe_util:ensure_app_loaded(App) of
        ok ->
            case application:get_key(App, vsn) of
                {ok, Vsn} -> Vsn;
                undefined -> "unknown"
            end;
        error ->
            "unknown"
    end.

-spec get_rebar3_version() -> string().
get_rebar3_version() ->
    case application:get_key(rebar, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.

%% @doc Format a section heading with centered text and equal padding.
%% All headings will be the same length as "=== Project Applications ===" (28 chars).
-spec format_heading(string()) -> string().
format_heading(Text) ->
    %% Total length is based on "=== Project Applications ===" = 28 chars
    TotalLength = 28,
    TextLength = length(Text),
    %% Format is: "===" + equals_padding + " " + Text + " " + equals_padding + "==="
    %% So: 3 + LeftPad + 1 + TextLen + 1 + RightPad + 3 = 28
    %% Therefore: LeftPad + TextLen + RightPad = 20
    TotalPadding = TotalLength - 8 - TextLength,  %% 8 = "===" + " " + " " + "==="
    LeftPadding = TotalPadding div 2,
    RightPadding = TotalPadding - LeftPadding,
    %% Build the heading
    lists:flatten([
        "===",
        lists:duplicate(LeftPadding, $=),
        " ", Text, " ",
        lists:duplicate(RightPadding, $=),
        "==="
    ]).

-spec display_versions(map()) -> ok.
display_versions(#{apps := Apps, languages := Langs, tools := Tools, deps := Deps, plugins := Plugins}) ->
    %% Always display Languages section
    io:format("~n~s~n", [format_heading("Languages")]),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Langs
    ),

    %% Always display Build Tools section
    io:format("~n~s~n", [format_heading("Build Tools")]),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Tools
    ),

    %% Only display Dependencies section if there are dependencies (after filtering LFE)
    case Deps of
        [] -> ok;
        _ ->
            io:format("~n~s~n", [format_heading("Dependencies")]),
            lists:foreach(
                fun(#{name := Name, version := Vsn, profile := Profile}) ->
                    case Profile of
                        default -> io:format("  ~-20s ~s~n", [Name, Vsn]);
                        _ -> io:format("  ~-20s ~s (~s)~n", [Name, Vsn, Profile])
                    end
                end,
                Deps
            )
    end,

    %% Only display Plugins section if there are plugins (after filtering rebar3_lfe and rebar3_hex)
    case Plugins of
        [] -> ok;
        _ ->
            io:format("~n~s~n", [format_heading("Plugins")]),
            lists:foreach(
                fun(#{name := Name, version := Vsn, profile := Profile}) ->
                    case Profile of
                        default -> io:format("  ~-20s ~s~n", [Name, Vsn]);
                        _ -> io:format("  ~-20s ~s (~s)~n", [Name, Vsn, Profile])
                    end
                end,
                Plugins
            )
    end,

    %% Only display Project Applications section if there are multiple apps
    %% (skip if just one app - the project itself)
    case Apps of
        [] -> ok;
        [_SingleApp] -> ok;  %% Skip if only one application
        _ ->
            io:format("~n~s~n", [format_heading("Project Applications")]),
            lists:foreach(
                fun({Name, Vsn}) ->
                    io:format("  ~-20s ~s~n", [Name, Vsn])
                end,
                Apps
            )
    end,

    io:format("~n"),
    ok.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Displays version information for:~n"
        "  - Languages (LFE, Erlang/OTP)~n"
        "  - Build tools (rebar3, rebar3_lfe, rebar3_hex)~n"
        "  - Dependencies~n"
        "  - Plugins~n"
        "  - Project applications~n",
        [Description]
    ).
