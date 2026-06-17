-module(r3lfe_format_lexer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Round-trip corpus
-export([
    round_trip_empty/1,
    round_trip_whitespace_only/1,
    round_trip_comment_only/1,
    round_trip_no_trailing_newline/1,
    round_trip_consecutive_blank_lines/1,
    round_trip_all_kinds/1,
    round_trip_tq_corpus/1,
    round_trip_integration_files/1
]).

%% Classification
-export([
    classify_char_semicolon/1,
    classify_char_lparen/1,
    classify_char_hex/1,
    classify_number_decimal/1,
    classify_symbol_123foo/1,
    classify_symbol_float_extra/1,
    classify_number_radix_b/1,
    classify_binary_open/1,
    classify_bstring/1,
    classify_number_radix_Nr/1,
    classify_number_radix_36/1,
    classify_qsymbol_with_semicolon/1,
    classify_qsymbol_escaped_bar/1,
    classify_tqstring_multiline/1,
    classify_tqbstring_multiline/1,
    classify_line_comment_newline/1,
    classify_unquote_splicing/1,
    classify_unquote/1,
    classify_fun_ref/1,
    classify_dot_standalone/1,
    classify_dot_in_symbol_run/1,
    classify_dot_ellipsis/1,
    classify_dot_in_list/1
]).

%% Error cases
-export([
    error_unterminated_block_comment/1,
    error_unterminated_string/1,
    error_unterminated_qsymbol/1,
    error_bad_tq_string/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        {group, round_trip_corpus},
        {group, classification},
        {group, errors}
    ].

groups() ->
    [
        {round_trip_corpus, [], [
            round_trip_empty,
            round_trip_whitespace_only,
            round_trip_comment_only,
            round_trip_no_trailing_newline,
            round_trip_consecutive_blank_lines,
            round_trip_all_kinds,
            round_trip_tq_corpus,
            round_trip_integration_files
        ]},
        {classification, [], [
            classify_char_semicolon,
            classify_char_lparen,
            classify_char_hex,
            classify_number_decimal,
            classify_symbol_123foo,
            classify_symbol_float_extra,
            classify_number_radix_b,
            classify_binary_open,
            classify_bstring,
            classify_number_radix_Nr,
            classify_number_radix_36,
            classify_qsymbol_with_semicolon,
            classify_qsymbol_escaped_bar,
            classify_tqstring_multiline,
            classify_tqbstring_multiline,
            classify_line_comment_newline,
            classify_unquote_splicing,
            classify_unquote,
            classify_fun_ref,
            classify_dot_standalone,
            classify_dot_in_symbol_run,
            classify_dot_ellipsis,
            classify_dot_in_list
        ]},
        {errors, [], [
            error_unterminated_block_comment,
            error_unterminated_string,
            error_unterminated_qsymbol,
            error_bad_tq_string
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Round-trip corpus (§7 group 1)
%%====================================================================

round_trip_empty(_Config) ->
    assert_round_trip(<<>>).

round_trip_whitespace_only(_Config) ->
    assert_round_trip(<<"   \t  ">>),
    assert_round_trip(<<"  \n  \n">>).

round_trip_comment_only(_Config) ->
    assert_round_trip(<<"; just a comment">>),
    assert_round_trip(<<"#| block comment |#">>).

round_trip_no_trailing_newline(_Config) ->
    assert_round_trip(<<"(foo bar)">>),
    assert_round_trip(<<"42">>).

round_trip_consecutive_blank_lines(_Config) ->
    assert_round_trip(<<"\n\n\n">>),
    assert_round_trip(<<"(a)\n\n\n(b)">>).

round_trip_all_kinds(_Config) ->
    %% tqstring and tqbstring require the opening line to end immediately with \n.
    Inputs = [
        <<"()">>,                          %% lparen, rparen
        <<"[]">>,                          %% lbracket, rbracket
        <<"#(1 2)">>,                      %% tuple_open
        <<"#m(a 1)">>,                     %% map_open lowercase
        <<"#M(a 1)">>,                     %% map_open uppercase
        <<"#b(1 2)">>,                     %% binary_open lowercase
        <<"#B(1 2)">>,                     %% binary_open uppercase
        <<"#.(foo)">>,                     %% eval_open
        <<"'x">>,                          %% quote
        <<"`x">>,                          %% quasiquote
        <<",x">>,                          %% unquote
        <<",@x">>,                         %% unquote_splicing
        <<"#'foo/2">>,                     %% fun_ref + symbol
        <<"hello">>,                       %% symbol
        <<"|hello world|">>,              %% qsymbol
        <<"42">>,                          %% number (decimal)
        <<"3.14">>,                        %% number (float)
        <<"#b1010">>,                      %% number (binary)
        <<"#o777">>,                       %% number (octal)
        <<"#xDEAD">>,                      %% number (hex)
        <<"#2r1010">>,                     %% number (Nr form)
        <<"#\\a">>,                        %% char (single codepoint)
        <<"#\\x1f600;">>,                  %% char (hex unicode)
        <<"#\\(">>,                        %% char (open paren as char)
        <<"#\\;">>,                        %% char (semicolon as char)
        <<"\"hello\"">>,                   %% string
        <<"#\"bytes\"">>,                  %% bstring
        <<"\"line\\nbreak\"">>,            %% string with escape
        <<"\"\"">>,                        %% empty string
        <<"\"\"\"\ncontent\n\"\"\"">>,     %% tqstring (valid LFE format)
        <<"#\"\"\"\ncontent\n\"\"\"">>,    %% tqbstring (valid LFE format)
        <<"; line comment\n">>,            %% line_comment + newline
        <<"#| block comment |#">>,         %% block_comment
        <<"   \t ">>,                      %% whitespace
        <<"\n">>                           %% newline
    ],
    lists:foreach(fun assert_round_trip/1, Inputs).

round_trip_tq_corpus(_Config) ->
    %% Fixture exercises both a block comment (#|...|#) and a multi-line
    %% triple-quoted string (with embedded """ and #| on content lines).
    File = filename:join([data_dir(), "tq_corpus.lfe"]),
    {ok, Bin} = file:read_file(File),
    assert_round_trip(Bin, File).

round_trip_integration_files(_Config) ->
    Files = integration_lfe_files(),
    ?assert(length(Files) > 0, "expected at least one _integration .lfe file"),
    ct:log("Round-tripping ~p integration .lfe files", [length(Files)]),
    lists:foreach(
        fun(File) ->
            {ok, Bin} = file:read_file(File),
            assert_round_trip(Bin, File)
        end,
        Files
    ).

%%====================================================================
%% Classification tests (§7 group 2)
%%====================================================================

classify_char_semicolon(_Config) ->
    %% #\; is a char, NOT a line comment
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"#\\;">>),
    ?assertEqual(char, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#\\;", r3lfe_format_lexer:text(T)).

classify_char_lparen(_Config) ->
    %% #\( is a char, NOT tuple_open
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"#\\(">>),
    ?assertEqual(char, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#\\(", r3lfe_format_lexer:text(T)).

classify_char_hex(_Config) ->
    %% #\x1f42d; is a single char token
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"#\\x1f42d;">>),
    ?assertEqual(char, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#\\x1f42d;", r3lfe_format_lexer:text(T)).

classify_number_decimal(_Config) ->
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"123">>),
    ?assertEqual(number, r3lfe_format_lexer:kind(T)),
    ?assertEqual("123", r3lfe_format_lexer:text(T)).

classify_symbol_123foo(_Config) ->
    %% 123foo is one symbol, not a number
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"123foo">>),
    ?assertEqual(symbol, r3lfe_format_lexer:kind(T)),
    ?assertEqual("123foo", r3lfe_format_lexer:text(T)).

classify_symbol_float_extra(_Config) ->
    %% 1.23e4extra is a symbol, not a number
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"1.23e4extra">>),
    ?assertEqual(symbol, r3lfe_format_lexer:kind(T)).

classify_number_radix_b(_Config) ->
    %% #b101 is a number
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"#b101">>),
    ?assertEqual(number, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#b101", r3lfe_format_lexer:text(T)).

classify_binary_open(_Config) ->
    %% #b( is binary_open, NOT a number
    {ok, [Open, Close]} = r3lfe_format_lexer:tokens(<<"#b()">>),
    ?assertEqual(binary_open, r3lfe_format_lexer:kind(Open)),
    ?assertEqual("#b(", r3lfe_format_lexer:text(Open)),
    ?assertEqual(rparen, r3lfe_format_lexer:kind(Close)).

classify_bstring(_Config) ->
    %% #"x" is a bstring
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"#\"x\"">>),
    ?assertEqual(bstring, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#\"x\"", r3lfe_format_lexer:text(T)).

classify_number_radix_Nr(_Config) ->
    %% #2r1010 is a number
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"#2r1010">>),
    ?assertEqual(number, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#2r1010", r3lfe_format_lexer:text(T)).

classify_number_radix_36(_Config) ->
    %% #36rHELLO — all of H,E,L,O are valid base-36 digits (A-Z covers up to base 36)
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"#36rHELLO">>),
    ?assertEqual(number, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#36rHELLO", r3lfe_format_lexer:text(T)).

classify_qsymbol_with_semicolon(_Config) ->
    %% |a;b| is one qsymbol; the ; is NOT a comment inside
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"|a;b|">>),
    ?assertEqual(qsymbol, r3lfe_format_lexer:kind(T)),
    ?assertEqual("|a;b|", r3lfe_format_lexer:text(T)).

classify_qsymbol_escaped_bar(_Config) ->
    %% |a\|b| is one qsymbol with an escaped bar inside
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"|a\\|b|">>),
    ?assertEqual(qsymbol, r3lfe_format_lexer:kind(T)),
    ?assertEqual("|a\\|b|", r3lfe_format_lexer:text(T)).

classify_tqstring_multiline(_Config) ->
    %% A valid multi-line tqstring:
    %%   - opening """ alone on its line
    %%   - content line containing """ and #| (both treated as literal content)
    %%   - closing """ alone on its line (blank line before it)
    Input = <<"\"\"\"\nhas \"\"\" and #| inside\n\"\"\"">>,
    {ok, Tokens} = r3lfe_format_lexer:tokens(Input),
    ?assertMatch([_], Tokens),
    [T] = Tokens,
    ?assertEqual(tqstring, r3lfe_format_lexer:kind(T)),
    ?assertEqual("\"\"\"\nhas \"\"\" and #| inside\n\"\"\"",
                 r3lfe_format_lexer:text(T)),
    assert_round_trip(Input).

classify_tqbstring_multiline(_Config) ->
    %% Same shape for #"""...""" => tqbstring
    Input = <<"#\"\"\"\nhas \"\"\" and #| inside\n\"\"\"">>,
    {ok, Tokens} = r3lfe_format_lexer:tokens(Input),
    ?assertMatch([_], Tokens),
    [T] = Tokens,
    ?assertEqual(tqbstring, r3lfe_format_lexer:kind(T)),
    ?assertEqual("#\"\"\"\nhas \"\"\" and #| inside\n\"\"\"",
                 r3lfe_format_lexer:text(T)),
    assert_round_trip(Input).

classify_line_comment_newline(_Config) ->
    %% ";; comment\n" => line_comment then newline; comment text excludes \n
    {ok, [Comment, Nl]} = r3lfe_format_lexer:tokens(<<";; comment\n">>),
    ?assertEqual(line_comment, r3lfe_format_lexer:kind(Comment)),
    ?assertEqual(";; comment", r3lfe_format_lexer:text(Comment)),
    ?assertEqual(newline, r3lfe_format_lexer:kind(Nl)),
    ?assertEqual("\n", r3lfe_format_lexer:text(Nl)).

classify_unquote_splicing(_Config) ->
    %% ,@x => unquote_splicing + symbol
    {ok, [Splice, Sym]} = r3lfe_format_lexer:tokens(<<",@x">>),
    ?assertEqual(unquote_splicing, r3lfe_format_lexer:kind(Splice)),
    ?assertEqual(",@", r3lfe_format_lexer:text(Splice)),
    ?assertEqual(symbol, r3lfe_format_lexer:kind(Sym)),
    ?assertEqual("x", r3lfe_format_lexer:text(Sym)).

classify_unquote(_Config) ->
    %% ,x => unquote + symbol
    {ok, [Uq, Sym]} = r3lfe_format_lexer:tokens(<<",x">>),
    ?assertEqual(unquote, r3lfe_format_lexer:kind(Uq)),
    ?assertEqual(",", r3lfe_format_lexer:text(Uq)),
    ?assertEqual(symbol, r3lfe_format_lexer:kind(Sym)).

classify_fun_ref(_Config) ->
    %% #'foo/2 => fun_ref (text "#'") + symbol (text "foo/2")
    {ok, [Ref, Sym]} = r3lfe_format_lexer:tokens(<<"#'foo/2">>),
    ?assertEqual(fun_ref, r3lfe_format_lexer:kind(Ref)),
    ?assertEqual("#'", r3lfe_format_lexer:text(Ref)),
    ?assertEqual(symbol, r3lfe_format_lexer:kind(Sym)),
    ?assertEqual("foo/2", r3lfe_format_lexer:text(Sym)).

classify_dot_standalone(_Config) ->
    %% A run of exactly "." is the cons-dot operator; emits distinct kind `dot`.
    {ok, [T]} = r3lfe_format_lexer:tokens(<<".">>),
    ?assertEqual(dot, r3lfe_format_lexer:kind(T)),
    ?assertEqual(".", r3lfe_format_lexer:text(T)),
    assert_round_trip(<<".">>).

classify_dot_in_symbol_run(_Config) ->
    %% a.b.c — dot is a valid symbol char inside a longer run; one symbol token.
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"a.b.c">>),
    ?assertEqual(symbol, r3lfe_format_lexer:kind(T)),
    ?assertEqual("a.b.c", r3lfe_format_lexer:text(T)).

classify_dot_ellipsis(_Config) ->
    %% ... is three dots in a row — still one symbol token.
    {ok, [T]} = r3lfe_format_lexer:tokens(<<"...">>),
    ?assertEqual(symbol, r3lfe_format_lexer:kind(T)),
    ?assertEqual("...", r3lfe_format_lexer:text(T)).

classify_dot_in_list(_Config) ->
    %% (a . b) => lparen, symbol, ws, dot, ws, symbol, rparen
    {ok, Tokens} = r3lfe_format_lexer:tokens(<<"(a . b)">>),
    Kinds = [r3lfe_format_lexer:kind(T) || T <- Tokens],
    ?assertEqual([lparen, symbol, whitespace, dot, whitespace, symbol, rparen], Kinds),
    assert_round_trip(<<"(a . b)">>).

%%====================================================================
%% Error cases (§7 group 3)
%%====================================================================

error_unterminated_block_comment(_Config) ->
    Result = r3lfe_format_lexer:tokens(<<"#| no closing">>),
    ?assertMatch({error, {unterminated_block_comment, _}}, Result).

error_unterminated_string(_Config) ->
    Result = r3lfe_format_lexer:tokens(<<"\"no closing">>),
    ?assertMatch({error, {unterminated_string, _}}, Result).

error_unterminated_qsymbol(_Config) ->
    Result = r3lfe_format_lexer:tokens(<<"|no closing">>),
    ?assertMatch({error, {unterminated_qsymbol, _}}, Result).

error_bad_tq_string(_Config) ->
    %% Non-space content after the opening """ => bad_tq_string
    ?assertMatch({error, {bad_tq_string, _}},
                 r3lfe_format_lexer:tokens(<<"\"\"\"content-on-open-line\n\"\"\"">>)),
    %% Spaces then non-space before \n => also bad_tq_string
    ?assertMatch({error, {bad_tq_string, _}},
                 r3lfe_format_lexer:tokens(<<"\"\"\"   bad\n\"\"\"">>)).

%%====================================================================
%% Helpers
%%====================================================================

assert_round_trip(Bin) ->
    assert_round_trip(Bin, <<"(inline)">>).

assert_round_trip(Bin, Label) ->
    {ok, Ts} = r3lfe_format_lexer:tokens(Bin),
    Reconstructed = unicode:characters_to_binary(
        r3lfe_format_lexer:to_iolist(Ts), utf8
    ),
    ?assertEqual(Bin, Reconstructed,
                 io_lib:format("round-trip failed for ~s", [Label])).

data_dir() ->
    TestDir = filename:dirname(filename:absname(?FILE)),
    filename:join([TestDir, "r3lfe_format_lexer_SUITE_data"]).

integration_lfe_files() ->
    TestDir = filename:dirname(filename:absname(?FILE)),
    IntDir = filename:join([TestDir, "..", "_integration"]),
    filelib:wildcard(filename:join([IntDir, "**", "*.lfe"])).
