# 022 · Arc A3 · S1 — pipeline + flat rendering (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First of three bounded sub-slices of
> Arc A3 (kept small to stay well under the output cap). Implement exactly this.
> If wrong or impossible, **stop and report**.
>
> **Output discipline (important):** write code straight to files with Write/Edit;
> do **not** paste files back into your reply; keep prose to a few lines; commit
> the module and a small suite, run tests, report tersely. This slice is
> deliberately small — do not exceed it.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); load `erlang-guidelines`, read
   `11-anti-patterns.md`.
2. `arc3-printer/cc-prompt.md` — the **shared reference**. Read §2 (fixed
   decisions: width 80, indent 2, LF-only, prefixes glue, generic +2 rule) and §6
   (oracle definitions). You implement only part of it here.
3. Closed inputs you build on (do not modify): `src/r3lfe_format_lexer.erl`,
   `src/r3lfe_format_cst.erl`. CST API: `parse/1`, accessors `type/1` `open/1`
   `close/1` `prefix/1` `children/1`; `document_children/1`; `significant_tokens/1`.

## 1. Scope of THIS slice

**In:** create `src/r3lfe_formatter.erl` with the pipeline and **flat rendering
only**.

```erlang
-spec format(binary() | string()) -> {ok, iolist()} | {error, term()}.
```

Pipeline: `r3lfe_format_lexer:tokens/1` → `r3lfe_format_cst:parse/1` →
flat-render the document. Propagate `{error,_}` unchanged.

`flat_render/1` for every node type:
- leaf (`symbol`/`number`/`string`/`char`) → its token text verbatim.
- container (`list`/`tuple`/`map`/`binary`/`eval`) → `open.text` ++
  space-joined `flat_render` of children ++ `close.text` (e.g. `(foo a b)`,
  `#m(k v)`, `[a b]`). Empty container → `open.text ++ close.text` (`()`).
- `prefixed` → `prefix.text ++ flat_render(inner)` with **no space** (`'foo`,
  `,@xs`, `#'f/2`).

Top-level layout: render each top-level form via `flat_render`, one per line,
separated by `\n`; if a form's `leading` contains a `blank`, emit exactly one
empty line before it (never at the very start); end the file with exactly one
`\n`.

**Out (deferred — do NOT implement here):** width checks / breaking (S2);
comment emission and per-node blank handling inside forms (S3). For this slice,
**assume inputs are comment-free and each top-level form fits on one line.** It is
fine that a >80-col form stays on one long line here — S2 fixes that.

## 2. Tests — `test/r3lfe_formatter_SUITE.erl`, group `flat`

Use small, comment-free inputs that fit in 80 cols. Assert (definitions in shared
ref §6): idempotency, token-preservation, and AST-equivalence (via
`lfe_io:read_string`), plus 2–3 golden flat outputs (e.g.
`(defun f (x) (+ x 1))` → itself; nested `(a (b c) d)` → itself; `'foo`,
`#m(k v)`, `[a b]` round-trip). Exclude `#.(` from AST-equivalence. Keep the
suite small.

## 3. Constraints

Pure module; no new deps; `-spec` all exports; `xref`+`dialyzer` clean; compile
under `warnings_as_errors`; do not modify lexer/CST (report bugs, don't patch
across modules).

## 4. Ledger

```
Arc A3·S1 — flat printer
[ ] r3lfe_formatter:format/1 wired (lex→parse→flat-render); errors propagated
[ ] flat_render covers all node types; prefixes glue; empty containers
[ ] top-level layout: one form/line, single blank on leading-blank, final \n
[ ] suite group `flat`: idempotency + token + AST-equiv + small goldens, green
[ ] xref + dialyzer clean; warnings_as_errors clean
[ ] no regressions (full suite); files changed + one-line rationale
[ ] deviations named — or "none"
```

Stop here. Do not start S2.
