# 022 · Arc A1 — Lossless LFE tokenizer (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines` skill. This is an **implementation
> spec**, not a design brief — the design decisions are already made below.
> Implement exactly what is specified; if something here is wrong or impossible,
> **stop and report** rather than improvise.

## 0. Orientation (read first, in order)

1. `CLAUDE.md` — project conventions. `warnings_as_errors` is ON.
2. Load the `erlang-guidelines` skill; read `11-anti-patterns.md` first.
3. `docs/design/022-lfe-format-provider.md` §2 and §4.1 — why this exists.
4. `lfe/src/lfe_scan.erl` (in the `lfe` dep) — **reference only**, for the LFE
   literal grammar (what is legal). **Do NOT fork it.** It is a
   continuation-passing streaming scanner; we are writing a simpler single-pass
   scanner. Use it to answer "is this literal form legal and what are its
   bounds", nothing more.

## 1. What you are building

One module, `src/r3lfe_format_lexer.erl`, that turns LFE source into a **flat,
lossless token list**. Nothing else — no tree, no printing, no provider. This is
the foundation the rest of the formatter stands on, so its one guarantee
(§6) is sacred.

## 2. Fixed design decisions (do not deviate)

1. **Whole-file, single pass.** Signature takes the entire source; no streaming,
   no `{more, …}` continuations.
2. **Work on a codepoint list.** Decode the input once with
   `unicode:characters_to_list(Bin, utf8)` and scan that list of integers. A
   token's `text` is the sublist of codepoints it spans. This makes the
   round-trip guarantee hold for any valid UTF-8 source.
3. **LF line endings; valid UTF-8.** Assume `\n` newlines and valid UTF-8. If
   `unicode:characters_to_list/2` does not return a flat list (i.e. malformed
   input), return `{error, {invalid_encoding, …}}`. Do not attempt CRLF
   normalization in A1 (document it as a known limitation).
4. **Trivia are first-class tokens.** Whitespace runs, newlines, and comments are
   emitted as ordinary tokens in the stream. Comment *attachment* to AST nodes is
   Arc A2's job — A1 just emits them in order.
5. **"Lex a maximal run, then classify."** For anything that could be a number or
   a symbol, scan the maximal run of symbol-constituent codepoints first, then
   classify the run as a number or a symbol (§5.6). Do **not** write an
   incremental number state machine.
6. **No semantic interpretation.** Do not evaluate `#.(…)`, do not parse number
   values, do not unescape strings. Keep the verbatim text. The lexer classifies
   and delimits; it never transforms.

## 3. Module skeleton

```erlang
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

-spec tokens(binary() | string()) -> {ok, [token()]} | {error, term()}.
-spec to_iolist([token()]) -> iolist().
-spec kind(token()) -> kind().
-spec text(token()) -> string().
-spec line(token()) -> pos_integer().
-spec col(token()) -> pos_integer().
```

Accessors are trivial. `to_iolist/1` is `[T#tok.text || T <- Tokens]`.

## 4. Token kinds (the complete set)

| kind | source | notes |
|------|--------|-------|
| `lparen` `rparen` | `(` `)` | |
| `lbracket` `rbracket` | `[` `]` | LFE treats `[]` as list too |
| `tuple_open` | `#(` | closed by `rparen` |
| `map_open` | `#m(` or `#M(` | closed by `rparen` |
| `binary_open` | `#b(` or `#B(` | closed by `rparen` |
| `eval_open` | `#.(` | read-eval; closed by `rparen` |
| `quote` | `'` | |
| `quasiquote` | `` ` `` | |
| `unquote` | `,` | |
| `unquote_splicing` | `,@` | match `,@` **before** `,` |
| `fun_ref` | `#'` | deprecated `#'name/arity`; emit `#'` then the `name/arity` falls out as a `symbol` |
| `symbol` | bare atom/var | incl. names containing `/ . : ? ! + - * = < >` etc. |
| `qsymbol` | `\|…\|` | bar-quoted; `\\\|` escapes a bar inside |
| `number` | see §5.6 | all bases, floats, char-as-number not included here |
| `char` | `#\a`, `#\x1f42d;`, `#\(` | the `#\` char literal |
| `string` | `"…"` | with escapes (kept verbatim) |
| `bstring` | `#"…"` | binary string |
| `tqstring` | `"""…"""` | triple-quoted, verbatim, multi-line |
| `tqbstring` | `#"""…"""` | triple-quoted binary string |
| `line_comment` | `;…` | up to but **excluding** the `\n` |
| `block_comment` | `#\| … \|#` | not nestable; EOF before `\|#` ⇒ error |
| `whitespace` | run of spaces/tabs | one token per maximal run, no `\n` |
| `newline` | a single `\n` | one token each (so blank lines = consecutive `newline`s) |

## 5. Scanning rules (decision tables — follow exactly)

Scan left to right. At each position, dispatch on the leading codepoint(s). First
match wins; the order below is the priority order.

### 5.1 Whitespace and newlines
- `\n` ⇒ one `newline` token (text `"\n"`).
- maximal run of space/tab (and other non-newline whitespace) ⇒ one `whitespace`.

### 5.2 Comments
- `;` ⇒ `line_comment`: consume through the char before the next `\n` (or EOF).
  The `\n` is a separate `newline` token.
- `#|` ⇒ `block_comment`: consume through the matching `|#` (inclusive). Not
  nestable. If EOF arrives first ⇒ `{error, {unterminated_block_comment, Line}}`.

### 5.3 The `#` dispatch (look at the char(s) after `#`)
Decide in this order:
- `#|` → block comment (5.2)
- `#'` → `fun_ref` (consume the two chars `#'`)
- `#\` → `char` (5.5)
- `#"""` → `tqbstring` (5.4); else `#"` → `bstring` (5.4)
- `#(` → `tuple_open`
- `#m(` / `#M(` → `map_open`
- `#b(` / `#B(` → `binary_open`  ← note: `(` follows
- `#.(` → `eval_open`
- otherwise `#` begins a **radix number run** (e.g. `#b101`, `#x-1F`, `#o377`,
  `#d99`, `#*101`, `#2r1010`, `#36rZZ`): hand off to the run scanner (5.6) which
  will classify it as `number`. (So `#b101` is a number but `#b(` is
  `binary_open` — the discriminator is whether `(` immediately follows the
  letter.)

### 5.4 Strings
- `"""` ⇒ `tqstring` (and `#"""` ⇒ `tqbstring`): triple-quoted, multi-line. The
  delimiting rule **must match `lfe_scan`** (`src/lfe_scan.erl` `scan_tq_string*`,
  ~L717–823), not "find the first `"""`":
  1. **Opening line:** after the opening `"""`/`#"""`, only spaces may follow up
     to the newline. If any other char appears before `\n`, return
     `{error, {bad_tq_string, Line}}` (the reader rejects it, so we must too —
     we never accept what the compiler won't parse).
  2. **Content lines:** accumulate verbatim. A `"""` that appears on a line with
     non-space content before it is **content, not a closer** (the LFE guide's
     own example puts `"""` inside a content line).
  3. **Closing line:** a `"""` is the closer only when the current line up to it
     is all spaces (`lfe_scan`'s `blank_line/1`). Consume it; the token spans
     from the opening delimiter through this closing `"""`, verbatim.
  4. EOF before a valid close ⇒ `{error, {unterminated_string, Line}}`.
  (4-quote / `""""` corner cases follow whatever `lfe_scan` does; match it or, if
  ambiguous, document and test the chosen behavior.)
- otherwise `"` ⇒ `string`: through the closing unescaped `"`. Backslash escapes
  the next char (kept verbatim — do not unescape). `#"` ⇒ `bstring`, same rule.
- Unterminated ⇒ `{error, {unterminated_string, Line}}`.
- Disambiguation: when you see `"`, check for `"""` first; `""` (empty string) is
  a `string`, not the start of a triple-quote.

### 5.5 Char literals
- `#\x` followed by hex digits and `;` ⇒ `char` of the hex form (`#\x1f42d;`).
- otherwise `#\` + exactly one codepoint ⇒ `char` (that codepoint may be `(`,
  `;`, space, etc. — it is data, not structure). So `#\;` is a char, not a
  comment.

### 5.6 Atoms / numbers — "run, then classify"
For any other start (a symbol-constituent codepoint, a digit, `+ - . :`, etc., or
a `#`-radix start from 5.3): consume the **maximal run** of symbol-constituent
codepoints. Per the LFE guide, a symbol may contain almost any latin-1+ char
except whitespace, the brackets `()[]`, `"`, `;`, and the leading-only chars
`| ' \` , #` (these may appear *after* the first char). Stop the run at any of
those terminators.

Then classify the run text:
- If it parses as an LFE number (decimal int, signed; float `1.0`, `1.0e10`,
  `1.111e-10`; or a radix form `#b…` `#o… ` `#d…` `#x…` `#*…` `#Nr…`) ⇒ `number`.
- Otherwise ⇒ `symbol`. (This is what makes `123foo` and `1.23e4extra` symbols,
  exactly as the guide requires.)

Implement the number test as a pure predicate over the run string. You may use a
regex or hand-rolled checks; keep it total and well-tested. Do not compute the
numeric value.

### 5.7 Bar-quoted symbols
- `|` ⇒ `qsymbol`: through the closing unescaped `|`. `\|` and `\\` are escapes
  inside; keep verbatim. Unterminated ⇒ `{error, {unterminated_qsymbol, Line}}`.

### 5.8 Prefixes and structure
- `,@` ⇒ `unquote_splicing` (check before `,`).
- `,` ⇒ `unquote`. `'` ⇒ `quote`. `` ` `` ⇒ `quasiquote`.
- `(` `)` `[` `]` ⇒ the obvious paren/bracket tokens.

### 5.9 Position tracking
Track `line` (start at 1) and `col` (start at 1) for each token's first
codepoint. Increment line on `\n` and reset col to 1. Tabs advance col by 1 (do
not expand). Positions are advisory for A2's diagnostics; the round-trip test
does not depend on them, but they must be sane.

## 6. The guarantee that defines "done"

For every input `Bin` of valid UTF-8 LFE source:

```erlang
{ok, Ts} = r3lfe_format_lexer:tokens(Bin),
unicode:characters_to_binary(r3lfe_format_lexer:to_iolist(Ts), utf8) =:= Bin
```

must hold **byte-for-byte**. No token's text may be normalized, trimmed, or
re-encoded. This is the primary test and the reason the module exists.

## 7. Tests — `test/r3lfe_format_lexer_SUITE.erl` (Common Test)

Match the repo's CT style (see existing `*_SUITE.erl`). Required groups:

1. **Round-trip corpus** — assert §6 for: every kind in §4; an empty file; a file
   that is only whitespace; a file that is only a comment; a file with no trailing
   newline; consecutive blank lines; and **every `.lfe` file under the repo's
   `_integration/` tree** (discover them at runtime and loop). This last one is
   the real-world proof.
2. **Classification** — assert the `kind` (and `text`) of targeted inputs:
   - `#\;` ⇒ one `char`, not a comment.
   - `#\(` ⇒ one `char`, not `tuple_open`.
   - `123foo` ⇒ one `symbol`; `123` ⇒ one `number`; `1.23e4extra` ⇒ `symbol`.
   - `#b101` ⇒ `number`; `#b(` ⇒ `binary_open`; `#b"x"` is not valid — `#"` is
     `bstring`, so test `#"x"` ⇒ `bstring`.
   - `#2r1010` ⇒ `number`; `#36rHELLO` ⇒ `number`.
   - `|a;b|` ⇒ one `qsymbol` whose text contains `;`.
   - `|a\|b|` ⇒ one `qsymbol` (escaped bar inside).
   - A real multi-line `tqstring`: opening `"""` alone on its line, a content
     line that itself contains `"""` and `#|`, and a closing `"""` alone on its
     (whitespace-only) line ⇒ exactly one `tqstring` spanning all of it.
     (NB: `"""a …` with content right after the opening delimiter is invalid LFE
     ⇒ `{error,{bad_tq_string,_}}`, not a token. The earlier single-line example
     here was wrong; do not test for it.)
   - `;; comment` then newline ⇒ `line_comment` + `newline` (comment text
     excludes the `\n`).
   - `,@x` ⇒ `unquote_splicing` + `symbol`; `,x` ⇒ `unquote` + `symbol`.
   - `#'foo/2` ⇒ `fun_ref` + `symbol` (text `"foo/2"`).
3. **Errors** — unterminated block comment / string / qsymbol each return the
   specified `{error, …}` tuple.

## 8. Constraints recap

- `-spec` every exported function; pass `rebar3 xref` and `rebar3 dialyzer`
  clean; compile under `warnings_as_errors`.
- Pure module: no process, no ETS, no file I/O (the provider reads files later).
- No dependency on `lfe_scan` at runtime.
- snake_case atoms/functions, `%%` for function comments, tagged returns
  (`{ok,_}`/`{error,_}`), let-it-crash on genuinely impossible internal states.

## 9. Report against this ledger

```
Arc A1 — r3lfe_format_lexer
[ ] Module compiles clean under warnings_as_errors
[ ] r3lfe_format_lexer_SUITE added; all groups pass (paste `rebar3 ct` summary)
[ ] Round-trip oracle green incl. every _integration/*.lfe file (state the count)
[ ] Classification + error tests green (list them)
[ ] xref clean; dialyzer clean (paste summaries)
[ ] No existing tests regressed (full suite)
[ ] Files changed, each with one-line rationale
[ ] Deviations from this spec (if any) named explicitly — or "none"
```

Stop at a proven-lossless token stream. Do not start Arc A2.
