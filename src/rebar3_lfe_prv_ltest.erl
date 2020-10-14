-module(rebar3_lfe_prv_ltest).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_lfe.hrl").

-define(PROVIDER, ltest).
-define(DEPS, [compile]).
-define(DEFAULT_TEST_TYPE, unit).

%% ===================================================================
%% Public API
%% ===================================================================

init(State) ->
    Description = "Run LFE tests that have been created with the ltest project.",
    OptsHelp = opts(State),
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                {namespace, ?NAMESPACE},
                {name, ?PROVIDER},
                {module, ?MODULE},
                {bare, true},
                {deps, ?DEPS},
                {example, "rebar3 lfe ltest"},
                {short_desc, Description},
                {desc, info(Description)},
                {opts, OptsHelp}
            ])
    ),
    {ok, State1}.

do(State) ->
    test(State).

format_error({unknown_app, Unknown}) ->
    io_lib:format("Applications list for test contains an unrecognizable application definition: ~p", [Unknown]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

opts(State) ->
    EunitOpts = rebar_prv_eunit:eunit_opts(State),
    rebar_api:debug("Default eunit options help: ~p", [EunitOpts]),
    Dir = lists:nth(6, EunitOpts),
    File = lists:nth(7, EunitOpts),
    Genr = lists:nth(10, EunitOpts),
    Mod = lists:nth(8, EunitOpts),
    Name = lists:nth(12, EunitOpts),
    Profile = lists:nth(5, EunitOpts),
    SetCookie = lists:nth(14, EunitOpts),
    SName = lists:nth(13, EunitOpts),
    Suite = lists:nth(9, EunitOpts),
    Verbose = lists:nth(11, EunitOpts),
    lists:append([
        lists:sublist(EunitOpts, 1, 2),
        [{color, undefined, "color", boolean, "Whether to display tests "
             "in ANSI-highlighted colors."}],
        lists:sublist(EunitOpts, 3, 2),
        [Dir,
         File,
         Genr,
         Mod,
         Name,
         Profile,
         SetCookie,
         SName,
         Suite,
         {'test-listener', $l, "test-listener", atom, "Which test listener to "
            "run; legal values are 'ltest-listener', 'eunit_progress', and  "
            "'eunit_surefire'."},
         {'test-type', $t, "test-type", atom, "type of tests to run; "
            "legal valuues are unit, system, integration, or all"},
         Verbose]
    ]).

info(Description) ->
    io_lib:format(
            "~n~s~n"
            "~n",
            [Description]).

test(State) ->
    rebar_api:debug("Running the test command ...", []),
    rebar_paths:set_paths([deps, plugins], State),
    LtestOpts = find_ltest_options(State),
    rebar_api:debug("\tStarting test run ...", []),
    ltest:run(LtestOpts),
    rebar_api:debug("\tFinished test run.", []),
    {ok, State}.

find_ltest_options(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    rebar_api:debug("\tGot CLI opts: ~p", [Opts]),
    DefaultOpts = ltest:'default-opts'(),
    rebar_api:debug("\tGot default opts: ~p", [DefaultOpts]),
    MapOpts = maps:merge(DefaultOpts, maps:from_list(Opts)),
    rebar_api:debug("\tGot opts: ~p", [MapOpts]),
    MapOpts.
