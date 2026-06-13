%%%% Lossless, comment-preserving tokenizer for the LFE source formatter.
%%%% Unlike lfe_scan, this keeps every comment and every whitespace character so
%%%% the formatter can preserve them. See docs/design/022-arc1-lexer.md.
-module(r3lfe_format_lexer).

-export([tokens/1, to_iolist/1, kind/1, text/1, line/1, col/1]).

-export_type([token/0, kind/0]).

-record(tok, {kind :: kind(),
              text :: string(),   %% verbatim codepoints this token spans
              line :: pos_integer(),
              col  :: pos_integer()}).

-opaque token() :: #tok{}.

-type kind() :: lparen | rparen | lbracket | rbracket
              | tuple_open | map_open | binary_open | eval_open
              | quote | quasiquote | unquote | unquote_splicing | fun_ref
              | symbol | qsymbol | number | char
              | string | bstring | tqstring | tqbstring
              | line_comment | block_comment
              | whitespace | newline.

%%====================================================================
%% Exported API
%%====================================================================

-spec tokens(binary() | string()) -> {ok, [token()]} | {error, term()}.
tokens(Bin) when is_binary(Bin) ->
    case unicode:characters_to_list(Bin, utf8) of
        Cs when is_list(Cs) ->
            scan(Cs, 1, 1, []);
        {error, _, _} ->
            {error, {invalid_encoding, Bin}};
        {incomplete, _, _} ->
            {error, {invalid_encoding, Bin}}
    end;
tokens(Cs) when is_list(Cs) ->
    scan(Cs, 1, 1, []).

-spec to_iolist([token()]) -> iolist().
to_iolist(Tokens) ->
    [T#tok.text || T <- Tokens].

-spec kind(token()) -> kind().
kind(#tok{kind = K}) -> K.

-spec text(token()) -> string().
text(#tok{text = T}) -> T.

-spec line(token()) -> pos_integer().
line(#tok{line = L}) -> L.

-spec col(token()) -> pos_integer().
col(#tok{col = C}) -> C.

%%====================================================================
%% Internal: main scan loop — §5 priority order
%%====================================================================

%% Done.
scan([], _L, _C, Acc) ->
    {ok, lists:reverse(Acc)};

%% §5.1 Newline: one token per \n.
scan([$\n | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = newline, text = "\n", line = Line, col = Col},
    scan(Rest, Line + 1, 1, [Tok | Acc]);

%% §5.1 Whitespace run: maximal run of non-newline whitespace.
scan([C | _] = Cs, Line, Col, Acc) when C >= 0, C =< $\s, C =/= $\n ->
    {Text, Rest, Len} = collect_ws(Cs, [], 0),
    Tok = #tok{kind = whitespace, text = Text, line = Line, col = Col},
    scan(Rest, Line, Col + Len, [Tok | Acc]);

%% §5.2 Line comment: ; through end of line (excluding \n).
scan([$; | _] = Cs, Line, Col, Acc) ->
    {Text, Rest, Len} = collect_line_comment(Cs, [], 0),
    Tok = #tok{kind = line_comment, text = Text, line = Line, col = Col},
    scan(Rest, Line, Col + Len, [Tok | Acc]);

%% §5.3 / §5.2 Block comment: #|...|#
scan([$#, $| | Rest], Line, Col, Acc) ->
    case scan_block_comment(Rest, [$|, $#], Line, Col + 2, Line) of
        {ok, Body, Rest2, NewLine, NewCol} ->
            Tok = #tok{kind = block_comment,
                       text = lists:reverse(Body),
                       line = Line, col = Col},
            scan(Rest2, NewLine, NewCol, [Tok | Acc]);
        {error, _} = Err ->
            Err
    end;

%% §5.3 fun_ref: #' (emit the two chars; name/arity falls out as symbol).
scan([$#, $' | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = fun_ref, text = "#'", line = Line, col = Col},
    scan(Rest, Line, Col + 2, [Tok | Acc]);

%% §5.5 Char literal: #\ followed by hex form or a single codepoint.
scan([$#, $\\ | Rest], Line, Col, Acc) ->
    case scan_char_body(Rest) of
        {ok, Body, Rest2, BodyLen} ->
            Tok = #tok{kind = char,
                       text = [$#, $\\ | Body],
                       line = Line, col = Col},
            scan(Rest2, Line, Col + 2 + BodyLen, [Tok | Acc]);
        {error, _} = Err ->
            Err
    end;

%% §5.4 Triple-quoted binary string: #"""..."""
scan([$#, $", $", $" | Rest], Line, Col, Acc) ->
    case scan_tq_string(Rest, [$", $", $", $#], Line, Col + 4, Line) of
        {ok, Body, Rest2, NewLine, NewCol} ->
            Tok = #tok{kind = tqbstring,
                       text = lists:reverse(Body),
                       line = Line, col = Col},
            scan(Rest2, NewLine, NewCol, [Tok | Acc]);
        {error, _} = Err ->
            Err
    end;

%% §5.4 Binary string: #"..."
scan([$#, $" | Rest], Line, Col, Acc) ->
    case scan_sq_string(Rest, [$", $#], Line, Col + 2, Line) of
        {ok, Body, Rest2, NewLine, NewCol} ->
            Tok = #tok{kind = bstring,
                       text = lists:reverse(Body),
                       line = Line, col = Col},
            scan(Rest2, NewLine, NewCol, [Tok | Acc]);
        {error, _} = Err ->
            Err
    end;

%% §5.3 Tuple open: #(
scan([$#, $( | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = tuple_open, text = "#(", line = Line, col = Col},
    scan(Rest, Line, Col + 2, [Tok | Acc]);

%% §5.3 Map open: #m( or #M(
scan([$#, M, $( | Rest], Line, Col, Acc) when M =:= $m; M =:= $M ->
    Tok = #tok{kind = map_open, text = [$#, M, $(], line = Line, col = Col},
    scan(Rest, Line, Col + 3, [Tok | Acc]);

%% §5.3 Binary open: #b( or #B(
scan([$#, B, $( | Rest], Line, Col, Acc) when B =:= $b; B =:= $B ->
    Tok = #tok{kind = binary_open, text = [$#, B, $(], line = Line, col = Col},
    scan(Rest, Line, Col + 3, [Tok | Acc]);

%% §5.3 Eval open: #.(
scan([$#, $., $( | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = eval_open, text = "#.(", line = Line, col = Col},
    scan(Rest, Line, Col + 3, [Tok | Acc]);

%% §5.3 # fallthrough: radix number or other run starting with #.
scan([$# | _] = Cs, Line, Col, Acc) ->
    {Text, Rest, Len} = collect_run(Cs, [], 0),
    Tok = #tok{kind = classify_run(Text), text = Text, line = Line, col = Col},
    scan(Rest, Line, Col + Len, [Tok | Acc]);

%% §5.4 Triple-quoted string: """...""" — check before single-quote.
scan([$", $", $" | Rest], Line, Col, Acc) ->
    case scan_tq_string(Rest, [$", $", $"], Line, Col + 3, Line) of
        {ok, Body, Rest2, NewLine, NewCol} ->
            Tok = #tok{kind = tqstring,
                       text = lists:reverse(Body),
                       line = Line, col = Col},
            scan(Rest2, NewLine, NewCol, [Tok | Acc]);
        {error, _} = Err ->
            Err
    end;

%% §5.4 Regular string: "..."
scan([$" | Rest], Line, Col, Acc) ->
    case scan_sq_string(Rest, [$"], Line, Col + 1, Line) of
        {ok, Body, Rest2, NewLine, NewCol} ->
            Tok = #tok{kind = string,
                       text = lists:reverse(Body),
                       line = Line, col = Col},
            scan(Rest2, NewLine, NewCol, [Tok | Acc]);
        {error, _} = Err ->
            Err
    end;

%% §5.7 Bar-quoted symbol: |...|
scan([$| | Rest], Line, Col, Acc) ->
    case scan_qsymbol(Rest, [$|], Line, Col + 1, Line) of
        {ok, Body, Rest2, NewLine, NewCol} ->
            Tok = #tok{kind = qsymbol,
                       text = lists:reverse(Body),
                       line = Line, col = Col},
            scan(Rest2, NewLine, NewCol, [Tok | Acc]);
        {error, _} = Err ->
            Err
    end;

%% §5.8 Unquote-splicing: ,@ — must precede unquote.
scan([$,, $@ | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = unquote_splicing, text = ",@", line = Line, col = Col},
    scan(Rest, Line, Col + 2, [Tok | Acc]);

%% §5.8 Unquote: ,
scan([$, | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = unquote, text = ",", line = Line, col = Col},
    scan(Rest, Line, Col + 1, [Tok | Acc]);

%% §5.8 Quote: '
scan([$' | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = quote, text = "'", line = Line, col = Col},
    scan(Rest, Line, Col + 1, [Tok | Acc]);

%% §5.8 Quasiquote: `
scan([$` | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = quasiquote, text = "`", line = Line, col = Col},
    scan(Rest, Line, Col + 1, [Tok | Acc]);

%% §5.8 Parens and brackets.
scan([$( | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = lparen, text = "(", line = Line, col = Col},
    scan(Rest, Line, Col + 1, [Tok | Acc]);

scan([$) | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = rparen, text = ")", line = Line, col = Col},
    scan(Rest, Line, Col + 1, [Tok | Acc]);

scan([$[ | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = lbracket, text = "[", line = Line, col = Col},
    scan(Rest, Line, Col + 1, [Tok | Acc]);

scan([$] | Rest], Line, Col, Acc) ->
    Tok = #tok{kind = rbracket, text = "]", line = Line, col = Col},
    scan(Rest, Line, Col + 1, [Tok | Acc]);

%% §5.6 Symbol / number run: any remaining symbol-constituent char.
scan([C | _] = Cs, Line, Col, Acc) ->
    case symbol_char(C) of
        true ->
            {Text, Rest, Len} = collect_run(Cs, [], 0),
            Tok = #tok{kind = classify_run(Text), text = Text,
                       line = Line, col = Col},
            scan(Rest, Line, Col + Len, [Tok | Acc]);
        false ->
            {error, {unexpected_char, C, Line, Col}}
    end.

%%====================================================================
%% Internal: trivia collectors
%%====================================================================

%% collect_ws: maximal run of non-newline whitespace chars.
collect_ws([C | Rest], Acc, N) when C >= 0, C =< $\s, C =/= $\n ->
    collect_ws(Rest, [C | Acc], N + 1);
collect_ws(Rest, Acc, N) ->
    {lists:reverse(Acc), Rest, N}.

%% collect_line_comment: from ; through end of line (excludes \n).
collect_line_comment([$\n | _] = Rest, Acc, N) ->
    {lists:reverse(Acc), Rest, N};
collect_line_comment([], Acc, N) ->
    {lists:reverse(Acc), [], N};
collect_line_comment([C | Rest], Acc, N) ->
    collect_line_comment(Rest, [C | Acc], N + 1).

%%====================================================================
%% Internal: delimited-token scanners (accumulate reversed; caller reverses)
%%====================================================================

%% scan_block_comment: Acc starts as reversed "#|"; text ends with "|#".
scan_block_comment([$|, $# | Rest], Acc, Line, Col, _SLine) ->
    {ok, [$#, $| | Acc], Rest, Line, Col + 2};
scan_block_comment([$\n | Rest], Acc, Line, _Col, SLine) ->
    scan_block_comment(Rest, [$\n | Acc], Line + 1, 1, SLine);
scan_block_comment([C | Rest], Acc, Line, Col, SLine) ->
    scan_block_comment(Rest, [C | Acc], Line, Col + 1, SLine);
scan_block_comment([], _Acc, _Line, _Col, SLine) ->
    {error, {unterminated_block_comment, SLine}}.

%% scan_sq_string: Acc starts as reversed opening delimiter; keeps escapes verbatim.
scan_sq_string([$\\, C | Rest], Acc, Line, Col, SLine) ->
    scan_sq_string(Rest, [C, $\\ | Acc], Line, Col + 2, SLine);
scan_sq_string([$" | Rest], Acc, Line, Col, _SLine) ->
    {ok, [$" | Acc], Rest, Line, Col + 1};
scan_sq_string([$\n | Rest], Acc, Line, _Col, SLine) ->
    scan_sq_string(Rest, [$\n | Acc], Line + 1, 1, SLine);
scan_sq_string([C | Rest], Acc, Line, Col, SLine) ->
    scan_sq_string(Rest, [C | Acc], Line, Col + 1, SLine);
scan_sq_string([], _Acc, _Line, _Col, SLine) ->
    {error, {unterminated_string, SLine}}.

%% scan_tq_string: opening-line phase — only spaces valid before the required \n.
%% Mirrors lfe_scan's scan_tq_string_1. Spaces are kept verbatim in Acc.
scan_tq_string([$\s | Rest], Acc, Line, Col, SLine) ->
    scan_tq_string(Rest, [$\s | Acc], Line, Col + 1, SLine);
scan_tq_string([$\n | Rest], Acc, Line, _Col, SLine) ->
    scan_tq_string_body(Rest, [$\n | Acc], true, Line + 1, 1, SLine);
scan_tq_string([], _Acc, _Line, _Col, SLine) ->
    {error, {unterminated_string, SLine}};
scan_tq_string(_Cs, _Acc, _Line, _Col, SLine) ->
    {error, {bad_tq_string, SLine}}.

%% scan_tq_string_body: content phase.
%% AllSpaces tracks whether every char on the current line so far is a space;
%% """ is the closer iff AllSpaces is true (mirrors lfe_scan's blank_line/1 check).
%% 4+ consecutive quotes on a blank line: the first """ closes; the rest are
%% returned as the next input — matching lfe_scan's behaviour.
scan_tq_string_body([$", $", $" | Rest], Acc, AllSpaces, Line, Col, SLine) ->
    case AllSpaces of
        true ->
            {ok, [$", $", $" | Acc], Rest, Line, Col + 3};
        false ->
            scan_tq_string_body(Rest, [$", $", $" | Acc],
                                 false, Line, Col + 3, SLine)
    end;
scan_tq_string_body([$\n | Rest], Acc, _AllSpaces, Line, _Col, SLine) ->
    scan_tq_string_body(Rest, [$\n | Acc], true, Line + 1, 1, SLine);
scan_tq_string_body([$\s | Rest], Acc, AllSpaces, Line, Col, SLine) ->
    scan_tq_string_body(Rest, [$\s | Acc], AllSpaces, Line, Col + 1, SLine);
scan_tq_string_body([C | Rest], Acc, _AllSpaces, Line, Col, SLine) ->
    scan_tq_string_body(Rest, [C | Acc], false, Line, Col + 1, SLine);
scan_tq_string_body([], _Acc, _AllSpaces, _Line, _Col, SLine) ->
    {error, {unterminated_string, SLine}}.

%% scan_qsymbol: Acc starts as reversed "|"; \| and \\ escape sequences kept verbatim.
scan_qsymbol([$\\, C | Rest], Acc, Line, Col, SLine) ->
    scan_qsymbol(Rest, [C, $\\ | Acc], Line, Col + 2, SLine);
scan_qsymbol([$| | Rest], Acc, Line, Col, _SLine) ->
    {ok, [$| | Acc], Rest, Line, Col + 1};
scan_qsymbol([$\n | Rest], Acc, Line, _Col, SLine) ->
    scan_qsymbol(Rest, [$\n | Acc], Line + 1, 1, SLine);
scan_qsymbol([C | Rest], Acc, Line, Col, SLine) ->
    scan_qsymbol(Rest, [C | Acc], Line, Col + 1, SLine);
scan_qsymbol([], _Acc, _Line, _Col, SLine) ->
    {error, {unterminated_qsymbol, SLine}}.

%%====================================================================
%% Internal: char literal body (after #\)
%%====================================================================

%% scan_char_body: returns {ok, Body, Rest, BodyLen}.
scan_char_body([$x | Rest]) ->
    case collect_hex_for_char(Rest, [], 0) of
        {[_ | _] = Hex, [$; | Rest2], HexLen} ->
            Body = [$x | Hex] ++ [$;],
            {ok, Body, Rest2, 1 + HexLen + 1};
        _ ->
            {ok, [$x], Rest, 1}
    end;
scan_char_body([C | Rest]) ->
    {ok, [C], Rest, 1};
scan_char_body([]) ->
    {error, unterminated_char}.

collect_hex_for_char([C | Rest], Acc, N) when
      (C >= $0 andalso C =< $9) orelse
      (C >= $a andalso C =< $f) orelse
      (C >= $A andalso C =< $F) ->
    collect_hex_for_char(Rest, [C | Acc], N + 1);
collect_hex_for_char(Rest, Acc, N) ->
    {lists:reverse(Acc), Rest, N}.

%%====================================================================
%% Internal: run collection and classification — §5.6
%%====================================================================

collect_run([C | Rest], Acc, N) ->
    case symbol_char(C) of
        true  -> collect_run(Rest, [C | Acc], N + 1);
        false -> {lists:reverse(Acc), [C | Rest], N}
    end;
collect_run([], Acc, N) ->
    {lists:reverse(Acc), [], N}.

%% classify_run: decide number or symbol for a maximal run of symbol chars.
classify_run([$# | _] = Text) ->
    case is_radix_number(Text) of
        true  -> number;
        false -> symbol
    end;
classify_run(Text) ->
    try list_to_integer(Text) of
        _ -> number
    catch error:badarg ->
        try list_to_float(Text) of
            _ -> number
        catch error:badarg -> symbol
        end
    end.

%%====================================================================
%% Internal: number classification predicates
%%====================================================================

is_radix_number([$#, C | Rest]) when C =:= $b; C =:= $B ->
    valid_based_digits(Rest, 2);
is_radix_number([$#, C | Rest]) when C =:= $o; C =:= $O ->
    valid_based_digits(Rest, 8);
is_radix_number([$#, C | Rest]) when C =:= $d; C =:= $D ->
    valid_based_digits(Rest, 10);
is_radix_number([$#, C | Rest]) when C =:= $x; C =:= $X ->
    valid_based_digits(Rest, 16);
is_radix_number([$#, $* | Rest]) ->
    valid_based_digits(Rest, 2);
is_radix_number([$# | Rest]) ->
    case split_at_r(Rest, []) of
        {[_ | _] = Digits, More} ->
            try list_to_integer(Digits) of
                Base when Base >= 2, Base =< 36 ->
                    valid_based_digits(More, Base);
                _ ->
                    false
            catch error:badarg -> false
            end;
        _ ->
            false
    end;
is_radix_number(_) ->
    false.

%% valid_based_digits: optional sign then at least one valid digit.
valid_based_digits([$+ | Rest], Base) -> valid_digits(Rest, Base);
valid_based_digits([$- | Rest], Base) -> valid_digits(Rest, Base);
valid_based_digits(Rest, Base)         -> valid_digits(Rest, Base).

valid_digits([_ | _] = Cs, Base) ->
    lists:all(fun(C) -> is_base_digit(C, Base) end, Cs);
valid_digits([], _Base) ->
    false.

is_base_digit(C, Base) when Base >= 2, Base =< 10 ->
    C >= $0 andalso C =< $0 + Base - 1;
is_base_digit(C, Base) when Base > 10, Base =< 36 ->
    (C >= $0 andalso C =< $9) orelse
    (C >= $a andalso C =< $a + Base - 11) orelse
    (C >= $A andalso C =< $A + Base - 11).

%% split_at_r: split a list at the first 'r' or 'R' that follows digits.
split_at_r([$r | Rest], Acc) when Acc =/= [] -> {lists:reverse(Acc), Rest};
split_at_r([$R | Rest], Acc) when Acc =/= [] -> {lists:reverse(Acc), Rest};
split_at_r([C | Rest], Acc) when C >= $0, C =< $9 -> split_at_r(Rest, [C | Acc]);
split_at_r(_, _) -> {[], []}.

%%====================================================================
%% Internal: symbol_char — mirrors lfe_scan's definition
%%====================================================================

symbol_char($()  -> false;
symbol_char($))  -> false;
symbol_char($[)  -> false;
symbol_char($])  -> false;
symbol_char(${)  -> false;
symbol_char($})  -> false;
symbol_char($")  -> false;
symbol_char($;)  -> false;
symbol_char(C)   -> (C > $\s andalso C =< $~) orelse (C > 16#A0).
