# 022 · Arc A3 — Printer core, generic (shared reference)

> ⚠️ **DO NOT hand CC this whole file.** A one-shot attempt at all of A3 blew the
> 32k output cap. A3 is split into three bounded sub-slices — hand CC **one at a
> time**:
> - `cc-prompt-s1-flat.md` — pipeline + flat rendering (comment-free, no breaking)
> - `cc-prompt-s2-breaking.md` — the +2 break algebra (comment-free, width-driven)
> - `cc-prompt-s3-comments.md` — comments, blank lines, edges; full-corpus oracles
>
> This file is the **shared reference** the three sub-slices point back to for the
> fixed decisions (§2) and the oracle definitions (§6). Read it, then work the
> sub-slice you were handed.

> Target: Sonnet 4.6 + `erlang-guidelines`. **Implementation spec**, not a design
> brief. Implement exactly this; if something is wrong or impossible, **stop and
> report** rather than improvise.

## 0. Orientation (read first, in order)

1. `CLAUDE.md` — conventions; `warnings_as_errors` is ON.
2. Load `erlang-guidelines`; read `11-anti-patterns.md` first.
3. `docs/design/022-lfe-format/rebar3-lfe-provider.md` §3, §5 — formatting policy
   and the correctness oracles.
4. `src/r3lfe_format_lexer.erl` (A1, closed) and `src/r3lfe_format_cst.erl`
   (A2, closed). These are your inputs — do **not** modify them. A2 API recap:
   - `r3lfe_format_cst:parse/1 :: [token()] -> {ok, cst_document()} | {error,_}`
   - accessors: `type/1` `open/1` `close/1` `prefix/1` `children/1` `leading/1`
     `trailing/1` `dangling/1`; `document_children/1` `document_dangling/1`;
     `significant_tokens/1`; `comments/1`.
   - node types: `list` `tuple` `map` `binary` `eval` (containers), `symbol`
     `number` `string` `char` (leaves), `prefixed`. `trivia()` =
     `{comment, token()} | blank`.

## 1. What you are building

One module, `src/r3lfe_formatter.erl`:

```erlang
-spec format(binary() | string()) -> {ok, iolist()} | {error, term()}.
```

Pipeline: `r3lfe_format_lexer:tokens/1` → `r3lfe_format_cst:parse/1` → **print the
document to an iolist**. On any lexer/parser `{error,_}`, return it unchanged.

This arc is the **generic** printer: a single uniform break rule for every form.
**No special-form indent table** (`case`/`let`/`defun` alignment) — that is Arc A4.
The goal here is a *correct, comment-preserving, idempotent* baseline.

## 2. Fixed decisions (do not deviate)

1. **Width = 80 columns.** Define as a single module constant
   (`-define(WIDTH, 80).`) so A6 can make it configurable later.
2. **Indent unit = 2 spaces.** No tabs ever in output.
3. **Output is LF-only.** `\n` line endings. CRLF input is normalized to LF
   (A1 already lexed `\r` into whitespace, which the CST drops). Document this in
   the module header as intended behavior.
4. **Prefixes glue to their target.** A `prefixed` node prints
   `prefix.text ++ print(inner)` with **no** space between. (`'foo`, `` `(a b)``,
   `,@xs`, `#'f/2`.) This is mandatory — `#' f/2` is rejected by the reader.
5. **Generic break rule** (§3) — uniform for all heads. No per-symbol rules.
6. **File ends with exactly one** `\n`. No leading blank line. No trailing blanks.
7. The printer changes **only** whitespace / newlines / indentation. It never
   reorders, inserts, or drops a significant token or a comment.

## 3. The break algebra

Two-mode printing per node: **flat** (single line) or **broken** (multi-line).

### 3.1 Flat eligibility

A node may print flat only if **all** hold:
- it carries no `leading`, `trailing`, or `dangling` comments, and none of its
  descendants do (a comment anywhere in the subtree forces breaking);
- it contains no multi-line token (`tqstring`, `tqbstring`, `block_comment`);
- its flat rendering fits: `current_column + flat_width(node) =< ?WIDTH`.

Implement `flat_width/1` (or `flat_render/1` returning text whose length you
measure). For a container the flat form is
`open.text ++ space-joined flat children ++ close.text`, e.g. `(foo a b)`. For a
leaf it is the token text. For `prefixed` it is `prefix.text ++ flat(inner)`
(no space). `blank` trivia and any comment ⇒ not flat-eligible.

### 3.2 Broken form (generic rule)

When a **container** must break, render:

```
<OPEN><child0>
<IND>  <child1>
<IND>  <child2>
... <childN><CLOSE>
```

- `<OPEN>` is the opener text (`(`, `[`, `#(`, `#m(`, `#b(`, `#.(`) at the
  container's current column `C`.
- **child0 (the head)** stays on the opener line, printed immediately after
  `<OPEN>`.
- **children 1..N** each start on their own line at indent column `C + 2`.
- `<CLOSE>` (`)` or `]`) hugs the last child — same line as the last child's last
  line, no space, no own line.
- An **empty** container (no children) prints `<OPEN><CLOSE>` (e.g. `()`) unless
  it has dangling comments (then break and emit them per §4).
- Each child is printed **recursively** with its own flat/broken decision,
  starting at column `C + 2`.

(This uniform "+2 hanging" is deliberately simpler than LFE's per-form
conventions. A4 replaces it with the `lfe-indent.el` table and the alignment
rules. Do not anticipate A4 here.)

A `prefixed` node that must break: print `prefix.text` then the broken `inner`
starting at the column just after the prefix (still glued — the break happens
inside `inner`).

A **leaf** never "breaks"; multi-line tokens (`tqstring`/`tqbstring`) are emitted
verbatim — when you do, update the current column to the length of the text after
its last `\n`.

## 4. Comment & trivia emission

- **leading** trivia of a node: emit before the node, each on its own line at the
  node's current indent. `{comment, Tok}` → the comment text then a newline.
  `blank` → one empty line (see §5).
- **trailing** trivia (a single comment): emit on the **same line** after the
  node — `…node ; comment` — with exactly one space before the `;`/`#|`, then a
  newline. A trailing comment therefore forces the surrounding container to be in
  broken mode.
- **dangling** trivia of a container: emit after the last child, before
  `<CLOSE>`, each on its own line at `C + 2`. `<CLOSE>` then goes on its own line
  at column `C`.
- Block comments (`#| … |#`) are emitted verbatim (they may span lines); track
  the column after the last `\n` of the block.

## 5. Blank-line & final-newline policy

- Between sibling nodes, if the next sibling's `leading` contains a `blank`, emit
  exactly **one** empty line before it. Never more than one consecutive blank
  line in output.
- No blank line immediately after a container opener or at the very start of the
  document (drop a leading `blank`).
- The document ends with exactly one `\n` (emit top-level forms separated by
  newlines; collapse/trim trailing blanks; ensure a single final newline).

## 6. Oracles (define "done")

Over a corpus (the repo's `_integration/*.lfe`, the A1 `tq_corpus` fixture, and
targeted snippets), assert:

1. **Idempotency:** `format(X2) == X2` where `{ok, X2io} = format(X)`,
   `X2 = iolist_to_binary(X2io)`. (A non-fixed-point formatter is buggy.)
2. **Token-preservation:** significant tokens of `format(X)` (re-lex + re-parse)
   equal those of `X`, in order.
3. **Comment-preservation:** `comments/1` of `format(X)` equals that of `X` —
   same texts, same order.
4. **AST-equivalence:** reading `format(X)` and `X` through `lfe_io:read_string`
   (or `lfe_scan`/`lfe_parse`) yields equal sexpr terms. Exclude only inputs
   containing `#.(` read-eval. (Unlike A2's oracle, you **may include** `#'`
   fun_ref inputs here, because the printer glues prefixes — verify it does.)

Also a small number of **golden** tests for the generic shape (a flat form that
fits; one that breaks to +2 hanging; a form with leading/trailing/dangling
comments; a blank-line case). Mark these provisional — A4 will rewrite expected
output when the indent table lands.

## 7. Tests — `test/r3lfe_formatter_SUITE.erl` (Common Test)

Groups: `oracles` (the four properties over the corpus), `golden` (the small
shape set above), `edge` (empty file ⇒ `""` or single newline — pick and test;
file that is only a comment; a top-level form longer than 80 cols that must
break; a `tqstring`-containing form; CRLF input normalized to LF). State your
empty-file choice explicitly.

## 8. Constraints

- Pure module; no process/ETS/file I/O; no new deps.
- `-spec` every exported function; `xref` + `dialyzer` clean; compile under
  `warnings_as_errors`.
- Do not modify `r3lfe_format_lexer` or `r3lfe_format_cst`. Found a bug there?
  **Report it**, don't patch across module boundaries.
- snake_case, `%%` function comments, tagged `{ok,_}`/`{error,_}` returns.

## 9. Report against this ledger

```
Arc A3 — r3lfe_formatter (generic)
[ ] Module compiles clean under warnings_as_errors
[ ] r3lfe_formatter_SUITE added; all groups pass (paste `rebar3 ct` summary)
[ ] Oracle 1 idempotency green over corpus (state file count)
[ ] Oracle 2 token-preservation green over corpus
[ ] Oracle 3 comment-preservation green over corpus
[ ] Oracle 4 AST-equivalence green over corpus (incl. fun_ref; excl. read-eval)
[ ] Golden shape tests present (flat / +2-broken / comments / blank-line)
[ ] Edge tests: empty file, comment-only, >80 break, tqstring, CRLF→LF
[ ] Prefix-gluing verified (no space; fun_ref round-trips)
[ ] xref clean; dialyzer clean
[ ] No existing tests regressed (full suite)
[ ] Files changed, each with one-line rationale
[ ] Deviations named explicitly — or "none"
```

Stop at a correct generic printer. Do not start Arc A4 (special-form indentation).
