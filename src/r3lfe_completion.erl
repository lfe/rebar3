-module(r3lfe_completion).

-export([
    generate_erlang_completions/1,
    generate_lfe_completions/1,
    ensure_files/0
]).

%% Generate completion file for Erlang standard library
-spec generate_erlang_completions(string()) -> ok | {error, term()}.
generate_erlang_completions(OutputPath) ->
    %% Get all loaded modules
    Modules = [atom_to_list(M) || M <- erlang:loaded()],

    %% Write to file, one per line
    Content = string:join(lists:sort(Modules), "\n") ++ "\n",

    filelib:ensure_dir(OutputPath),
    file:write_file(OutputPath, Content).

%% Generate completion file for LFE
-spec generate_lfe_completions(string()) -> ok | {error, term()}.
generate_lfe_completions(OutputPath) ->
    %% Common LFE forms and functions
    LfeForms = [
        "defun", "defmacro", "defmodule", "defrecord",
        "lambda", "match-lambda", "let", "let*", "letrec-function",
        "case", "if", "cond", "when", "receive", "try", "catch",
        "cons", "car", "cdr", "list", "tuple", "map",
        "+", "-", "*", "/", "div", "rem", "mod",
        "=:=", "=/=", "<", ">", "=<", ">=",
        "andalso", "orelse", "not",
        "progn", "eval-when-compile", "include-file",
        "binary", "bitstring",
        "map-get", "map-set", "map-update",
        "lists:map", "lists:filter", "lists:foldl", "lists:foldr",
        "io:format", "io:get_line"
    ],

    Content = string:join(lists:sort(LfeForms), "\n") ++ "\n",

    filelib:ensure_dir(OutputPath),
    file:write_file(OutputPath, Content).

%% Ensure default completion files exist
-spec ensure_files() -> ok.
ensure_files() ->
    Home = os:getenv("HOME", "/tmp"),
    CompletionDir = filename:join([Home, ".lfe", "completions"]),

    filelib:ensure_dir(filename:join(CompletionDir, "dummy")),

    ErlangFile = filename:join(CompletionDir, "erlang.txt"),
    LfeFile = filename:join(CompletionDir, "lfe.txt"),

    %% Generate if they don't exist
    case filelib:is_file(ErlangFile) of
        false -> generate_erlang_completions(ErlangFile);
        true -> ok
    end,

    case filelib:is_file(LfeFile) of
        false -> generate_lfe_completions(LfeFile);
        true -> ok
    end,

    ok.
