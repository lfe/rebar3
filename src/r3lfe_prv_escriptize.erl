-module(r3lfe_prv_escriptize).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% Exported for testing
-ifdef(TEST).
-export([info/1]).
-endif.

-include("r3lfe.hrl").

-define(PROVIDER, escriptize).
-define(DEPS, [{?NAMESPACE, compile}, {default, escriptize}]).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Build an LFE escript executable",

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe escriptize"},
        {opts, []},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE escriptize provider starting", []),

    %% Set up paths
    rebar_paths:set_paths([deps, plugins], State),

    %% The actual escriptize work is done by {default, escriptize} dep
    %% We just ensure LFE code is compiled first via our compile dep

    ?INFO("LFE escript built successfully", []),

    {ok, State}.

-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Builds an executable escript containing the project and its~n"
        "dependencies' BEAM files. The LFE code is compiled first via~n"
        "the lfe compile provider.~n"
        "~n"
        "Requirements in rebar.config:~n"
        "  {escript_main_app, myapp}.~n"
        "  {escript_name, \"myapp\"}.~n"
        "  {escript_emu_args, \"%%! +sbtu +A1\\n\"}.~n"
        "~n"
        "The main module must export main/1:~n"
        "  (defmodule myapp~n"
        "    (export (main 1)))~n"
        "~n"
        "  (defun main (args)~n"
        "    ;; Process args~n"
        "    ...)~n"
        "~n"
        "Example:~n"
        "  rebar3 lfe escriptize~n"
        "  ./_build/default/bin/myapp arg1 arg2~n",
        [Description]
    ).
