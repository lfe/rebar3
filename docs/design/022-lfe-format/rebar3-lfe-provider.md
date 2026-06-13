# 022 — `rebar3 lfe format`: an LFE source formatter

> Status: **Design / planning** (pre-implementation)
> Target release: **rebar3_lfe 0.5.5**
> Author: Duncan + pairing assistant (PM/planner)
> Companion: `022-lfe-format-cc-prompts.md` (ready-to-hand CC prompts)

## 1. Goal

Add a `format` provider to the `lfe` namespace so users can:

```sh
rebar3 lfe format                 # reformat, in place, every .lfe file in the
                                  # configured source dirs (or the defaults)
rebar3 lfe format --check         # no writes; exit non-zero if any file would
                                  # change (CI gate)
rebar3 lfe format --path PATH     # operate only on PATH (a file or a dir),
                                  # ignoring configured/default source dirs
rebar3 lfe format --check --path PATH   # the two compose
```

The formatter is a **full reflow pretty-printer** (rustfmt / erlfmt class): it
re-decides line breaks against an 80-column budget, normalizes indentation and
inter-token spacing, and — critically — **preserves every comment**.

## 2. The central constraint (read this first)

LFE's own scanner (`lfe_scan.erl`) **discards comments and whitespace**. Both
line comments (`;`) and the whitespace/newline scanners return `{none, …}` —
they emit *no token at all* (see `lfe_scan.erl` `scan_line_comment/4` ≈ L282,
and the whitespace clauses ≈ L230–233). Block comments (`#| … |#`) are handled
the same way.

**Consequence:** the tempting shortcut — read with `lfe_scan`/`lfe_parse`, then
reprint with `lfe_io_pretty` — would *silently delete every comment in every
file*. That is disqualifying for a source formatter. (For reference, the sibling
`lykn fmt` does take this shortcut: it reflows from a parsed `SExpr` tree with no
comment node. That is acceptable for a young language with no large installed
codebase; it is **not** acceptable for LFE.)

Therefore the formatter must carry its own **comment-preserving,
whitespace-aware tokenizer** and a **lossless CST**. The good news: the hard part
of tokenizing LFE — its rich literal syntax (`#\a` chars, `#"…"` binary strings,
triple-quoted strings, `#xFF` / `#2r1010` numerics, `|quoted symbols|`,
`#'f/2`, `#(…)` tuples, `#m(…)` maps, `#b(…)` binaries, `#.(…)` read-eval) — is
already solved in `lfe_scan.erl`. We **adapt** that battle-tested logic rather
than reinvent it.

## 3. The authoritative formatting spec, and what it does *not* cover

`lfe/emacs/lfe-indent.el` (209 lines) is the canonical source of LFE
indentation. Two facts matter:

1. **It is a re-indenter, not a re-flow engine.** Emacs only ever computes the
   *leading whitespace* of each line; it never moves a line break. So
   `lfe-indent.el` tells us **how deep to nest**, but says **nothing about where
   to break lines**. A reflow printer has to supply that policy itself.

2. **The model is the standard Lisp engine + a per-symbol indent table.**
   `lfe-body-indent = 2`. The table (L161–206) classifies head symbols:
   - `def*` / `defun` → **defform**: body lines indent at `column + 2`.
   - integer `N` (e.g. `case 1`, `let 1`, `if 1`, `lambda 1`, `receive 0`,
     `progn 0`, `try 1`, `catch 0`, `call 2`, `: 2`, `cond` via default) →
     **specform**: the first `N` "distinguished" args nest deeper
     (`column + 2*2`) when placed on their own line; remaining body args nest at
     `column + 2`.
   - no rule, head is a symbol → **default/funcall**: continuation args align
     under the *first argument*.
   - head is not a symbol (a list, e.g. a `let` binding list or a
     `match-lambda` clause) → align elements vertically under the head list.

We reproduce this table verbatim as the nesting authority. **Line-break policy**
(the gap `lfe-indent.el` leaves open) is derived from the LFE style guide
(`lfe-manual/.../style-guide.md`) and standard Lisp/erlfmt convention:

- 80-column hard maximum; 2-space indentation; no tabs.
- A form is printed on **one line if it fits**; otherwise it **breaks**, putting
  distinguished args on the head line where they fit and body args one-per-line
  at the form's body indent.
- `cond`/`case` clauses align (one clause per line). `defun` pattern-match
  clauses are compact, one clause per line. `let`/`let*` binding lists print one
  binding per line, vertically aligned under the first binding.
- Maps, tuples, proplists: **no** key/value column alignment (style guide §6).
- Docstrings sit on their own line(s) after the arg list / function name.
- One blank line between top-level forms; runs of ≥2 blank lines collapse to 1;
  blank lines inside forms are preserved (collapsed to ≤1).
- Bracket style (`[]` vs `()`) is **preserved as written** in v1 — we do not
  convert. (Both read identically, so this is a pure-style choice we defer.)

## 4. Architecture

Four new Erlang modules in `rebar3_lfe`, all unit/CT-testable like the rest of
the repo. No new dependencies.

```
src/r3lfe_prv_format.erl     %% provider: CLI surface, file discovery, orchestration
src/r3lfe_format_lexer.erl   %% lossless tokenizer (adapted from lfe_scan)
src/r3lfe_format_cst.erl     %% token stream -> CST with comment/trivia attachment
src/r3lfe_formatter.erl      %% CST -> pretty-printed iolist (the print algebra)
```

(Names are a proposal; CC may consolidate `cst` + `formatter` if it keeps the
two concerns clearly separated. The provider and the engine must stay separable
so the engine can be unit-tested without rebar state.)

### 4.1 Lexer — lossless token stream

A typed token stream where **every byte of the source is accounted for**. Token
kinds: open/close (`(` `)` `[` `]`), the literal openers (`#(` `#m(`/`#M(`
`#b(`/`#B(` `#.(`), prefixes (`'` `` ` `` `,` `,@` `#'`), atoms/symbols
(incl. `|…|`), numbers (all bases), strings (normal, binary `#"`, triple-quoted),
chars (`#\…`), line comments, block comments, and trivia (spaces, newlines,
blank-line runs). Each token records its source text and position.

**Foundational guarantee (the lexer's whole job):** concatenating the verbatim
text of all tokens reproduces the input **byte-for-byte**. This is the property
that makes comment preservation possible and is the lexer's primary test.

### 4.2 CST — tree with attached trivia

Parse the token stream into nodes (list / tuple / map / binary / atom / number /
string / symbol / prefixed-form), carrying bracket style and attaching each
comment + blank-line as one of:

- **leading** — own-line comment(s) immediately above a form;
- **trailing** — a comment on the same line *after* a form (`(foo) ; note`);
- **dangling** — comment with no sibling to attach to (e.g. before a closing
  paren in an otherwise empty/last position).

This attachment model is exactly how erlfmt / prettier / rustfmt preserve
comments; it is the crux of the whole feature and deserves its own slice and its
own tests.

### 4.3 Printer — the break/nest algebra

A recursive printer with the classic "try flat, else break" decision (à la
Wadler/Oppen, or the simpler measured-width recursion `lykn`'s formatter uses),
extended with: the `lfe-indent.el` indent table for nesting amounts; the
special-form alignment rules (§3); trailing-comment-forces-break; leading/dangling
comment emission; and the blank-line policy. **Invariant: the printer only ever
changes whitespace, newlines, and indentation — it never reorders, inserts, or
drops a token.**

## 5. Correctness oracles (how we know it's right)

Three layered checks, used in tests across every arc:

1. **Lossless lexing** — `concat(tokens(src)) == src`, byte-for-byte.
2. **AST equivalence** — `lfe_parse(lfe_scan(src))` ≡
   `lfe_parse(lfe_scan(format(src)))`. Read both the input and the formatted
   output through LFE's *real* reader and compare the resulting sexp terms. Since
   comments and bracket style don't affect the parsed term, this oracle proves
   the formatter **never changed meaning** — the single most important safety
   property. (Caveat: pick a corpus the stock reader accepts; `#.(…)` read-eval
   forms are excluded from the equivalence corpus or compared post-eval.)
3. **Idempotency** — `format(format(src)) == format(src)`. A formatter that
   isn't a fixed point is buggy by definition; this is cheap and catches a large
   class of errors.

Plus **golden-file** tests: the style-guide examples must format to themselves
(or to an agreed canonical form), checked as committed fixtures.

## 6. Provider surface

Mirror `r3lfe_prv_clean` / `r3lfe_prv_eval`. Declared `opts`:

```erlang
Opts = [
    {check, $c, "check", boolean,
     "Do not write; exit non-zero if any file is not already formatted"},
    {path,  $p, "path",  string,
     "Format only this file or directory, ignoring configured source dirs"}
].
```

`do/1` logic:

1. Read `{Opts, _} = rebar_state:command_parsed_args(State)`.
2. Build the file list:
   - if `--path` given: that file, or all `*.lfe` under that dir (recursive);
   - else: for each app, `r3lfe_config:get_src_dirs/1` →
     `r3lfe_package:discover_files/1` to collect `.lfe` files. (Default resolves
     to `src/` plus any configured `src_dirs`. `include/` and `test/` are reached
     via `--path` or by configuring `src_dirs`; we honor the user's spec of
     "configured source directories or the defaults".)
3. For each file: read, `format/1`, then:
   - normal mode: write back only if changed; report changed files;
   - `--check`: never write; collect files that *would* change.
4. Exit: normal mode returns `{ok, State}`; `--check` returns `{ok, State}` if
   clean, `{error, …}` (non-zero) if any file would change, listing them.

Register `r3lfe_prv_format` in `rebar3_lfe.erl`'s provider list.

**CLI safety note (per project CLAUDE.md):** the formatter writes files the user
can recover with `git checkout`; it must never touch anything outside the
resolved file set, and `--check` must never write. No safety-gate flags are
involved.

## 7. Arc breakdown (handed to CC one arc at a time)

Each arc is independently acceptable, with its own oracle-backed tests. We work
the arcs in order; later arcs depend on earlier ones.

| Arc | Deliverable | Primary acceptance |
|-----|-------------|--------------------|
| **A1** | `r3lfe_format_lexer` — lossless, comment/trivia-preserving tokenizer adapted from `lfe_scan` | Round-trip: `concat(tokens(src)) == src` byte-for-byte across an LFE-literal corpus (chars, binary/triple strings, all numeric bases, quoted symbols, tuples/maps/binaries, both comment kinds) |
| **A2** | `r3lfe_format_cst` — token stream → CST with leading/trailing/dangling comment attachment + blank-line model | Token-preservation (CST's significant tokens == lexer's, in order) + comment-preservation (ordered comment texts unchanged) + AST-equivalence vs `lfe_parse` on the corpus. (Byte-for-byte is A1's guarantee; A2 drops whitespace.) |
| **A3** | `r3lfe_formatter` core — generic print algebra (lists/tuples/maps/atoms/strings), 80-col break, +2 nest, comment emission, blank-line policy; **no** special-form table yet | Idempotency + AST-equivalence on corpus; sane output on generic forms |
| **A4** | Special-form indentation + alignment — port `lfe-indent.el` table; `let`/`cond`/`case`/`defun`/`try`/… alignment; docstrings | Golden-file tests: style-guide examples format to themselves; idempotency holds |
| **A5** | `r3lfe_prv_format` provider + wiring — `--check`, `--path`, src_dir resolution, in-place write, exit codes, reporting; register in `rebar3_lfe.erl` | CT suite like other providers; `rebar3 lfe format --check` runs clean on the repo's `_integration` project |
| **A6** | Hardening + docs + release — edge cases (empty files, `#.(…)`, unicode, no trailing newline), property/fuzz idempotency, README/docs, CHANGELOG, bump to **0.5.5** | Property tests green; docs published; version bumped |

Granularity: **arc-level** prompts (not finer slices) — each arc is one coherent
CC handoff. A1, A2, A4 are the high-risk arcs and should expect more review
iterations.

## 8. Ledger skeleton (per the collaboration framework)

For each arc, CC reports against these rows; CDC (the planner) verifies each row
independently against the actual artifacts before closing.

```
Arc Ax — <name>
[ ] Modules compile clean (warnings_as_errors honored, per rebar.config)
[ ] New code has CT/eunit tests; tests pass
[ ] Oracle(s) for this arc green (lossless / AST-equiv / idempotency as applicable)
[ ] No existing tests regressed (full `rebar3 ct` + `rebar3 eunit`)
[ ] xref + dialyzer clean (repo runs both)
[ ] Scope-as-delivered diffed vs scope-as-specified; deferrals named, no silent drops
[ ] Files changed listed with one-line rationale each
```

Five-iteration cap per arc before we stop and rethink the slice.

## 9. Open questions (decide as we reach them, not now)

- **Bracket normalization** (`[]`↔`()`): deferred — preserve as written in v1.
- **`include/` and `test/` by default**: v1 honors `src_dirs` only; revisit if
  users expect test/include formatting without `--path`.
- **`#.(…)` read-eval forms** in the AST-equivalence corpus: excluded from the
  equivalence oracle (they evaluate at read time); still exercised for
  lossless-lexing and idempotency.
- **Configurable width / a `.lfe_format` config**: out of scope for 0.5.5; the
  80-col default is hard-coded but isolated so it can become an option later.
```
