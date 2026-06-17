# 022 · Arc A7 · S2b — printer regimes: canonical vs break-preserving (SHARED REF)

> ⚠️ **DO NOT hand CC this whole file.** Doing all of S2b in one turn blew the 32k
> output cap. It's split into two bounded sub-slices — hand one at a time:
> - `cc-prompt-s2b-1-classify.md` — regime classification + `InData` threading,
>   **behavior-neutral** (break-preserving nodes still use the current renderer).
> - `cc-prompt-s2b-2-renderer.md` — the break-preserving renderer + regime tests
>   (this is where output changes).
>
> This file is the **shared reference** (the model, §1–§4) the two sub-slices point
> back to. Read it, then work the sub-slice you were handed.

> Target: Sonnet 4.6 + `erlang-guidelines`. The architectural heart of A7.
> **Implementation spec.** **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns`).
2. `docs/design/022-lfe-format/formatting-rules.md` §1 (the model). **This slice
   implements §1.**
3. `src/r3lfe_format_cst.erl` — has `nl_before/1` (S2a) and `multiline/1`.
4. `src/r3lfe_formatter.erl` — the printer you extend.

## 1. The two regimes

Decide per list/container node, threading a **data-context** flag `InData`:

- **Break-preserving** (keep the author's breaks): a node is break-preserving if
  ANY of —
  - `InData` is true (inside a quote — §2), or
  - it's a `tuple` or `binary` (data containers), or
  - it's a `list` whose head is **not** a known special form — i.e. a plain call
    or unknown macro (`classify_head` would return `funcall`, or the head isn't a
    symbol).
- **Canonical** (formatter owns layout — keep current behavior): a `list` whose
  head is in the specform table or is a def-form (`classify_head` → `{specform,_}`
  / `defform`), and `map` (k/v pairs, S3a). These keep their **existing** rendering
  from A4. *(Clauses get their canonical rule in S3; in this slice a clause —
  non-symbol head — is break-preserving, which S3 will override. That's fine.)*

## 2. Data-context tracking

Thread `InData :: boolean()` through the printer (default false at top level):
- entering a `quote` or `quasiquote` prefix → inner is printed with `InData=true`;
- entering an `unquote` or `unquote_splicing` prefix → inner with `InData=false`
  (unquote re-enters code inside a quasiquote);
- everything else passes `InData` through unchanged.

So `'(if x y)` is **data** (break-preserving, never treated as an `if`), while
`` `(foo ,(if x y)) `` treats the `(if …)` under the unquote as **code**
(canonical).

## 3. Break-preserving renderer

For a break-preserving node:

- **Flat** iff: `not multiline(node)` (the author did **not** break it) AND it
  fits in 80 AND it has no `must_break` descendant. → render flat (current flat
  path).
- **Otherwise broken, preserving the author's break positions:**
  - head/opener + the first child go on the opener line — **unless** the first
    child has `nl_before=true`, in which case it starts a new line too.
  - for each subsequent child: start a **new line** iff `nl_before(child)` is true
    (the author broke before it) **or** it would overflow 80 on the current line;
    otherwise keep it on the current line separated by a single space (this
    preserves author grouping like `make-op`'s k/v pairs).
  - **continuation column** = the column where the **first argument** is rendered
    (align-under-first-arg). If the first argument itself broke to its own line,
    use the form's open column + 2 (hanging) and align subsequent args there.
  - the close hugs the last child (comment-before-close per §3.4a is **out of
    scope here** — leave current behavior; S4 handles it).

Result (make-op, author-grouped):
```
(make-op action 'drive-son-to-school
         preconds '(son-at-home car-works)
         add-list '(son-at-school)
         del-list '(son-at-home))
```
Plain call the author broke (kept broken) and one the author left flat (stays
flat if it fits):
```
(merge-options defaults          (process x y z)
               user-opts
               overrides)
```

## 4. Keep canonical forms unchanged

Do **not** change the rendering of canonical forms in this slice (specform/def/map
keep their A4 output). This slice only: (a) routes nodes to the right regime, (b)
implements the break-preserving renderer, (c) threads `InData`. The always-break
additions, clause rule, and lambda rule are **S3**.

## 5. Tests — extend `r3lfe_formatter_SUITE`, group `regimes`

- make-op-style call: author groups k/v pairs across lines → grouping preserved.
- plain call the author broke (fits flat) → stays broken (per your decision).
- plain call the author wrote flat (fits) → stays flat.
- plain call too wide → wraps, aligned under first arg.
- quoted data list `'(a\n b\n c)` → breaks preserved; `'(a b c)` flat → stays flat.
- quasiquote with an unquoted `if`: `` `(x ,(if a b c)) `` → the `if` is treated as
  code (canonical) while the surrounding list is data.
- tuple/binary the author broke → preserved.
- canonical forms (a `case`, a `defun`) → output **unchanged** vs before this slice
  (regression guard).
- Full oracles: idempotency, token-, comment-preservation, AST-equivalence over
  the corpus. Add representatives to the fixture.

## 6. Constraints

Pure engine; `xref`/`dialyzer` standing; `warnings_as_errors`. Idempotent;
token-/comment-preserving; AST-equivalent. Don't modify lexer/CST (S2a is done).

## 7. Ledger

```
Arc A7·S2b — regimes
[ ] regime split: break-preserving (data ctx / tuple / binary / unknown-head list)
    vs canonical (specform/def list, map); canonical output unchanged
[ ] InData threaded (quote/quasiquote → data; unquote/-splicing → code)
[ ] break-preserving renderer: flat iff author-flat+fits; else preserve nl_before
    positions, align-under-first-arg, wrap >80; grouping (make-op) preserved
[ ] group `regimes` tests + canonical-unchanged regression; oracles green over corpus
[ ] xref + dialyzer standing; warnings_as_errors; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Always-break additions / clause / lambda rules are S3.
