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
    display_versions/1,
    info/1
]).
-endif.

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-define(PROVIDER, versions).
-define(DEPS, []).

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
        tools => get_tool_versions()
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
    [
        {rebar3, get_rebar3_version()},
        {r3lfe, get_version(r3lfe)}
    ].

-spec get_version(atom()) -> string().
get_version(App) ->
    case application:get_key(App, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.

-spec get_rebar3_version() -> string().
get_rebar3_version() ->
    case application:get_key(rebar, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.

-spec display_versions(map()) -> ok.
display_versions(#{apps := Apps, languages := Langs, tools := Tools}) ->
    io:format("~n=== Project Applications ===~n"),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Apps
    ),

    io:format("~n=== Languages ===~n"),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Langs
    ),

    io:format("~n=== Build Tools ===~n"),
    lists:foreach(
        fun({Name, Vsn}) ->
            io:format("  ~-20s ~s~n", [Name, Vsn])
        end,
        Tools
    ),

    io:format("~n"),
    ok.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Displays version information for:~n"
        "  - Project applications~n"
        "  - LFE and Erlang/OTP~n"
        "  - Build tools (rebar3, r3lfe)~n",
        [Description]
    ).
