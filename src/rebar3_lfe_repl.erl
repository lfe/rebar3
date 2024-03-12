-module(rebar3_lfe_repl).

-export([default_opts/0]).

-export([start/0, start/1, start/3]).

-export([lfe_version/0, quit_message/0]).

-define(DEFAULT_OPTS,
  #{start_module => lfe_shell,
    nobanner => false,
    banner => "",
    banner_template => "",
    banner_args => [],    
    %% Legacy k/vs:
    version => lfe_version(),
    quit_message => quit_message()
}).

default_opts() ->
    ?DEFAULT_OPTS.

start() ->
    start(?DEFAULT_OPTS).

start(UserOpts) ->
    Opts = maps:merge(?DEFAULT_OPTS, UserOpts),
    rebar_api:debug("Got opts: ~p", [Opts]),
    #{start_module := ReplMod,
      nobanner := NoBnr,
      banner := Bnr1,
      banner_template := BnrTmpl,
      banner_args := BnrArgs,
      version := Vsn,
      quit_message := QuitMsg      
    } = Opts,
    Bnr2 = banner(Bnr1, BnrTmpl, BnrArgs, Vsn, QuitMsg),
    start(ReplMod, NoBnr, Bnr2).

start(ReplMod, NoBnr, Bnr) ->
    case NoBnr of
        true ->
            ok;
        _ ->
            ok = io:put_chars(erlang:whereis(user), Bnr)
    end,
    rebar_api:debug("Calling %s:start ...", [ReplMod]),
    ReplMod:start().

%% XXX When the banner functions are public, delete everything below this line:
%% * https://github.com/lfe/lfe/issues/476

%% Coloured strings for the LFE banner, red, green, yellow and blue.
-define(RED(Str), "\e[31m" ++ Str ++ "\e[0m").
-define(GRN(Str), "\e[1;32m" ++ Str ++ "\e[0m").
-define(YLW(Str), "\e[1;33m" ++ Str ++ "\e[0m").
-define(BLU(Str), "\e[1;34m" ++ Str ++ "\e[0m").
-define(BOLD(Str), "\e[1m" ++ Str ++ "\e[0m").

%%banner() ->
%%    banner(lfe_version()).

%%banner(Vsn) ->
%%    banner(Vsn, quit_message()).

banner([Vsn, QuitMsg]) ->
    ?GRN("   ..-~") ++ ?YLW(".~_") ++ ?GRN("~---..") ++ "\n" ++
       ?GRN("  (      ") ++ ?YLW("\\\\") ++ ?GRN("     )") ++ "    |   A Lisp-2+ on the Erlang VM\n" ++
       ?GRN("  |`-.._") ++ ?YLW("/") ++ ?GRN("_") ++ ?YLW("\\\\") ++ ?GRN("_.-':") ++ "    |   Type " ++ ?GRN("(help)") ++ " for usage info.\n" ++
       ?GRN("  |         ") ++ ?RED("g") ++ ?GRN(" |_ \\") ++  "   |\n" ++
       ?GRN("  |        ") ++ ?RED("n") ++ ?GRN("    | |") ++   "  |   Docs: " ++ ?BLU("http://docs.lfe.io/") ++ "\n" ++
       ?GRN("  |       ") ++ ?RED("a") ++ ?GRN("    / /") ++   "   |   Source: " ++ ?BLU("http://github.com/lfe/lfe") ++ "\n" ++
       ?GRN("   \\     ") ++ ?RED("l") ++ ?GRN("    |_/") ++  "    |\n" ++
       ?GRN("    \\   ") ++ ?RED("r") ++ ?GRN("     /") ++  "      |   LFE v" ++
        Vsn ++ " " ++  QuitMsg ++ "\n" ++
       ?GRN("     `-") ++ ?RED("E") ++ ?GRN("___.-'") ++ "\n\n".

banner(Tmpl, Args) ->
    [io_lib:format(Tmpl, Args)].

banner("", "", _, Vsn, QuitMsg) ->
    banner([Vsn, QuitMsg]);
banner("", BnrTmpl, BnrArgs, _, _) ->
    banner(BnrTmpl, BnrArgs);
banner(Bnr, _, _, _, _) ->
    Bnr.
    
quit_message() ->
    %% We can update this later to check for env variable settings for
    %% shells that require a different control character to abort, such
    %% as jlfe.
    "(abort with ^C; enter JCL with ^G)".

lfe_version() ->
    {ok, [App]} = file:consult(code:where_is_file("lfe.app")),
    proplists:get_value(vsn, element(3, App)).
