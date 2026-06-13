# 022 · Arc A2 — CST with comment attachment (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. This is an **implementation spec**,
> not a design brief — the decisions are made below. Implement exactly this; if
> something here is wrong or impossible, **stop and report** rather than improvise.

## 0. Orientation (read first, in order)

1. `CLAUDE.md` — conventions; `warnings_as_errors` is ON.
2. Load the `erlang-guidelines` skill; read `11-anti-patterns.md` first.
3. `docs/design/022-lfe-format/rebar3-lfe-provider.md` §4.2 — the trivia model.
4. `src/r3lfe_format_lexer.erl` — Arc A1, **already shipped and closed**. This is
   your input. Its public API:
   - `tokens/1 :: binary()|string() -> {ok, [token()]} | {error, term()}`
   - `to_iolist/1`, and accessors `kind/1`, `text/1`, `line/1`, `col/1`
   - `token()` is opaque; `kind()` is the enumerated set in §1 below.
   - The stream is **flat** and includes `whitespace`, `newline`, `line_comment`,
     `block_comment` as first-class tokens.

Do **not** modify the lexer. Do **not** start the printer (Arc A3).

## 1. What you are building

One module, `src/r3lfe_format_cst.erl`, that turns the flat token list into a
**concrete syntax tree (CST)**: a tree of *significant* nodes, with *trivia*
(comments and blank-line markers) attached to nodes as leading / trailing /
dangling. Whitespace and newlines are **not** stored — the printer regenerates
them — but they are read to compute blank lines and to classify comments.

Token kinds, split into the two classes you must distinguish:

- **trivia tokens:** `whitespace`, `newline`, `line_comment`, `block_comment`.
- **significant tokens:** everything else — `lparen` `rparen` `lbracket`
  `rbracket` `tuple_open` `map_open` `binary_open` `eval_open` `quote`
  `quasiquote` `unquote` `unquote_splicing` `fun_ref` `symbol` `qsymbol`
  `number` `char` `string` `bstring` `tqstring` `tqbstring`.

## 2. Data model (use this shape)

```erlang
-type trivia() :: {comment, token()}   %% a line_comment or block_comment token (verbatim)
                | blank.                %% one-or-more consecutive blank lines, collapsed

-record(node, {
    type     :: node_type(),
    open     :: token() | undefined,  %% opener tok (containers) / the tok (leaves)
    close    :: token() | undefined,  %% closer tok (containers only)
    prefix   :: token() | undefined,  %% prefix tok (prefixed nodes only)
    children :: [#node{}],            %% containers: child nodes; prefixed: [InnerNode]
    leading  :: [trivia()],           %% trivia before this node, in source order
    trailing :: [trivia()],           %% a same-line comment after this node (0 or 1)
    dangling :: [trivia()]            %% containers: trivia after last child, before close
}).

-record(document, {
    children :: [#node{}],
    dangling :: [trivia()]            %% trivia after the last top-level node (EOF)
}).

-type node_type() :: list | tuple | map | binary | eval   %% containers
                   | symbol | number | string | char      %% leaves
                   | prefixed.                              %% prefix applied to one node
```

Field usage by type:
- **containers** (`list`/`tuple`/`map`/`binary`/`eval`): `open` = the opener token
  (this also records bracket style: `lparen` vs `lbracket` for `list`), `close` =
  the closer token, `children` = child nodes, `dangling` used.
- **leaves** (`symbol`/`number`/`string`/`char`): `open` = the single token.
  (Group the string-ish kinds `string`/`bstring`/`tqstring`/`tqbstring` under
  `string`; `qsymbol` under `symbol`; keep the original token so kind/text are
  recoverable.)
- **prefixed** (`'` `` ` `` `,` `,@` `#'`): `prefix` = the prefix token,
  `children` = `[InnerNode]` (exactly one).

Export `token()`-style opaque `node/0`, `document/0`, and accessors the printer
will need: `type/1`, `open/1`, `close/1`, `prefix/1`, `children/1`, `leading/1`,
`trailing/1`, `dangling/1`, plus `document_children/1`, `document_dangling/1`.

## 3. The attachment algorithm (follow exactly)

Parse the flat token list left to right with one helper that parses a **sequence
of nodes** until a closer (or EOF):

`parse_seq(Tokens) -> {Nodes, Dangling, RestTokens}`

Maintain `Pending :: [trivia()]` — leading trivia accumulated for the *next*
node, in source order. Loop over `Tokens`:

1. **`whitespace`** → skip (no state change).
2. **layout run with blank line** → while consuming consecutive `newline`s (with
   optional interleaved `whitespace`), if **two or more** `newline`s occur in the
   run, append a single `blank` to `Pending` (once per run, regardless of count).
   A single `newline` contributes nothing to `Pending`.
3. **`line_comment` / `block_comment` that is *trailing*** → see §4. If it
   attaches as trailing to the just-emitted node, consume it and continue.
   Otherwise treat as an own-line comment: append `{comment, Tok}` to `Pending`.
4. **opener** (`lparen` `lbracket` `tuple_open` `map_open` `binary_open`
   `eval_open`) → recurse: `parse_seq` the body until the matching closer (§5);
   build the container node with `open`=opener, `close`=closer,
   `children`=body nodes, `dangling`=body's leftover trivia, `leading`=`Pending`
   (then reset `Pending`=[]); run the trailing-peek (§4) on the new node; append
   to `Nodes`.
5. **closer** (`rparen` `rbracket`) → this sequence ends. Return
   `{Nodes, Pending, [closer | …]}` — the leftover `Pending` becomes the parent
   container's `dangling`; the caller consumes the closer.
6. **prefix** (`quote` `quasiquote` `unquote` `unquote_splicing` `fun_ref`) →
   parse exactly **one** following node (recursively; it may itself be a prefix,
   container, or leaf). Build `prefixed` with `prefix`=prefix token,
   `children`=[InnerNode], `leading`=`Pending` (reset). Any trivia between the
   prefix and its target attaches to the **inner** node's `leading` (rare; keep
   it simple). Trailing-peek applies to the outer `prefixed` node.
7. **leaf** (`symbol` `qsymbol` `number` `char` `string` `bstring` `tqstring`
   `tqbstring`) → build a leaf node, `open`=token, `leading`=`Pending` (reset),
   trailing-peek, append.
8. **end of input** → return `{Nodes, Pending, []}` (leftover `Pending` is the
   document's `dangling`).

Top level: `parse/1` calls `parse_seq`, and if `RestTokens` is non-empty (a stray
closer) returns `{error, {unbalanced, …}}`; otherwise wraps the result in
`#document{children=Nodes, dangling=Dangling}`.

## 4. Trailing-comment rule (precise)

After emitting a node `N`, **peek**: skip only `whitespace` tokens (not
`newline`). If the very next token is a `line_comment` or `block_comment` — i.e.
it sits on the **same line** as `N` with no `newline` between — attach it as `N`'s
single `trailing` trivia and consume it. At most one trailing comment per node;
anything after a `newline` is leading trivia for whatever follows.

Examples: `(foo) ; bar` → `; bar` is trailing on the `list` node. `;; sec`\n
`(defun …)` → `;; sec` is leading on the `defun` list. A comment alone before a
closing `)` (on its own line) → it stays in `Pending` at the closer and becomes
the container's `dangling`.

## 5. Opener/closer matching

`rparen` closes `lparen`, `tuple_open`, `map_open`, `binary_open`, `eval_open`.
`rbracket` closes `lbracket`. A mismatch (e.g. `[` … `)`) or EOF before a closer
⇒ `{error, {unbalanced, ExpectedCloser, Line}}`. Record the actual `open`/`close`
tokens so the printer can preserve bracket style and `#m(`/`#b(`/`#.(`/`#(`
opener spelling.

## 6. Oracles (the three tests that define "done")

> Note: byte-for-byte round-trip is **A1's** guarantee, not A2's — A2 drops
> whitespace. A2's guarantees are structural:

Expose for testing:
- `significant_tokens/1 :: document() -> [token()]` — an in-order left-to-right
  walk yielding every significant token (openers, closers, prefixes, leaves), and
  **no** trivia.
- `comments/1 :: document() -> [token()]` — every comment token, in source order.

Then assert, over a corpus (the repo's `_integration/*.lfe`, the A1 `tq_corpus`
fixture, plus targeted snippets):

1. **Token-preservation:** `significant_tokens(parse(Ts))` equals
   `[T || T <- Ts, kind(T) ∉ trivia]`, in order. (No significant token dropped,
   added, or reordered.)
2. **Comment-preservation:** `comments(parse(Ts))` equals
   `[T || T <- Ts, kind(T) ∈ {line_comment, block_comment}]`, in order.
3. **AST-equivalence:** build text from `significant_tokens` (concatenate their
   `text/1` with single spaces between — spacing is irrelevant to the reader),
   read it with `lfe_scan`/`lfe_parse`, and compare the resulting sexpr terms to
   reading the original source the same way. They must be equal. (Exclude inputs
   containing `#.(…)` read-eval from this oracle; still cover them in 1 & 2.)

## 7. Attachment unit tests (assert exact placement)

- `(foo) ; bar` → one top-level `list` node; its `trailing` = `[{comment, …}]`
  with text `"; bar"`; the inner `symbol foo` has empty trailing.
- `;;; section`\n`(defun f () 'ok)` → the `list` node's `leading` =
  `[{comment, …}]` (text `";;; section"`).
- a blank line between two top-level forms → the second node's `leading` contains
  exactly one `blank`.
- `(a\n  ;; trailing-in-list\n  )` → the comment is the `list` node's `dangling`,
  not attached to `a`.
- `'foo` → a `prefixed` node, `prefix` kind `quote`, one `symbol` child; ` `,@x`
  → `prefixed` with prefix kind `unquote_splicing`.
- `#m(k v)` → `map` node; `#(a b)` → `tuple`; `#b(1 2)` → `binary`; `[a b]` →
  `list` whose `open` kind is `lbracket`.

## 8. Constraints

- Pure module; no process/ETS/file I/O; no new deps.
- `-spec` every exported function; `xref` + `dialyzer` clean; compile under
  `warnings_as_errors`.
- snake_case, `%%` function comments, tagged `{ok,_}`/`{error,_}` returns,
  let-it-crash on genuinely impossible internal states.
- Do not modify `r3lfe_format_lexer`. If you find a lexer bug, **report it** —
  don't patch it inside A2.

## 9. Report against this ledger

```
Arc A2 — r3lfe_format_cst
[ ] Module compiles clean under warnings_as_errors
[ ] r3lfe_format_cst_SUITE added; all groups pass (paste `rebar3 ct` summary)
[ ] Oracle 1 (token-preservation) green over corpus (state file count)
[ ] Oracle 2 (comment-preservation) green over corpus
[ ] Oracle 3 (AST-equivalence vs lfe_scan/lfe_parse) green over corpus
[ ] Attachment unit tests green (list them: trailing / leading / blank / dangling
    / prefixed / container-kinds)
[ ] Error cases: unbalanced closer, EOF-before-closer, bracket mismatch
[ ] xref clean; dialyzer clean
[ ] No existing tests regressed (full suite)
[ ] Files changed, each with one-line rationale
[ ] Deviations from this spec named explicitly — or "none"
```

Stop at a correct CST. Do not start Arc A3.
