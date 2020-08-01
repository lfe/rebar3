-module(rebar3_lfe_prv_ltest).

-export([init/1,
         do/1,
         format_error/1]).

-define(NAMESPACE, lfe).
-define(PROVIDER, ltest).
-define(DEPS, [compile]).
-define(DEFAULT_TEST_TYPE, unit).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Run LFE tests that have been created with the ltest project.",
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
                {opts, opts(State)}
            ])
    ),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, test(State)}.

-spec format_error(any()) -> iolist().
format_error({unknown_app, Unknown}) ->
    io_lib:format("Applications list for test contains an unrecognizable application definition: ~p", [Unknown]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

info(Description) ->
    io_lib:format(
            "~n~s~n"
            "~n"
            "Run the unit, system, and integration tests "
            "for the project.~n"
            "~n",
            [Description]).

test(State) ->
    rebar_api:debug("Running the test command ...", []),
    {Opts, _} = rebar_state:command_parsed_args(State),
    rebar_api:debug("Got opts: ~p", [Opts]),
    case proplists:get_value('test-type', Opts, all) of
        all -> 'ltest-runner':all();
        unit -> 'ltest-runner':unit();
        system -> 'ltest-runner':system();
        integration -> 'test-runner':integration();
        UnknownType ->
            rebar_api:error("Unknown test-type value: ~p", [UnknownType])
    end,
    {ok, State}.

% {ok, State} = rebar3_lfe_prv_ltest:init(rebar_state:new()).
% rebar3_lfe_prv_ltest:do(State).
% rebar_state:command_parsed_args(rebar_state:new()).
% rebar_prv_eunit:eunit_opts(rebar_state:new()).

opts(State) ->
    lists:append(
        rebar_prv_eunit:eunit_opts(State),
        [{'test-type', $t, "test-type", atom, "type of tests to run; "
            "legal valuues are unit, system, integration, or all"},
         {color, undefined, "color", boolean, "whether to display tests "
             "in ANSI-highlighted colors."}]).
