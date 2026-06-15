# 022 · Arc A4 · S3b — style-guide conformance (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Final A4 sub-slice — **closes Arc A4**.
> This is mostly **validation**: confirm the formatter matches the LFE style guide,
> and surface (do not silently "fix") any divergence. **Stop and report** if
> blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. The style guide: `/Users/oubiwann/lab/lfe/lfe-manual/src/part7/ai-resources/style-guide.md`
   (in the VM: `/sessions/charming-tender-cerf/mnt/lfe-manual/src/part7/ai-resources/style-guide.md`)
   and `lfe/doc/src/lfe_guide.7.md`. These define the target formatting.
3. Your `src/r3lfe_formatter.erl` (A4·S1+S2+S3a closed) and
   `test/r3lfe_formatter_SUITE.erl`.

## 1. Scope of THIS slice — conformance, not new features

Build a conformance suite from the style guide's own examples and assert the
formatter is a **fixed point** on each: `format(canonical) == canonical`. Most
should already pass from the existing machinery (cond falls out of funcall-align;
let/case from specform; def-forms from S2; maps/tuples from S3a). Your job is to
prove it and adjudicate the gaps.

**Do NOT silently bend the formatter to match a guide example** — some guide
formatting is human judgment a mechanical formatter can't (and shouldn't) chase.
For each example that is NOT a fixed point, classify it:
- **(bug)** the formatter's output is clearly wrong vs the rules we defined →
  fix it, minimally, and note the fix; or
- **(divergence)** the formatter is internally consistent but differs from the
  guide's hand-formatting → **do not change code**; record it in the report for
  Duncan/planner to adjudicate.

## 2. The conformance corpus (from the style guide)

Add each as a golden fixed-point test (`format(X) == X`) — transcribe the
canonical form from the guide:
- `defun` simple (`factorial`), `defun` match-clause (`ackermann`), `defun`
  constants (`+my-pi+`, `+my-e+` grouped with no blank line between).
- `defun`/`defmacro` with a docstring (the `small-prime-number?` style example).
- `cond` with aligned clauses.
- `let` and `let*` with aligned bindings (the `low`/`high`/`sum` example —
  including the "do NOT vertically align the values" rule).
- `case`.
- a `#m(…)` map (pairs, no value alignment) and a "bad vs good" alignment case.
- tuples / proplists (no alignment).
- `defrecord`.
- multi-line function-call args (`do-something` aligned under first arg).
- `defmodule` with `export` on separate lines.
- file-header comment levels (`;;;;`/`;;;`/`;;`/`;`) round-tripping.

## 3. Broad idempotency + equivalence sweep

Beyond fixed points, run the full oracle set over a **wider** sample to build
confidence: every `.lfe` under the repo (`_integration/**`, any `test/.../*.lfe`
data, plus the accumulated corpus). For each: idempotency, token-preservation,
comment-preservation, AST-equivalence (excl. `#.(` read-eval). State the file
count.

## 4. Output — a short conformance report

Produce a brief summary (in the ledger / a comment block in the suite) listing,
for each style-guide construct: **fixed point ✅**, **fixed after (bug) fix**, or
**divergence (described)**. This is the artifact the planner will review to
declare A4 done.

## 5. Constraints

Pure module; keep `format/1`'s abstract spec + both `no_underspecs` suppressions;
`xref`+`dialyzer` clean; `warnings_as_errors`; don't modify lexer/CST. Any code
fix must keep idempotency + the comment matrix + all prior oracles green.

## 6. Ledger (closes Arc A4)

```
Arc A4·S3b — conformance
[ ] conformance suite: each style-guide construct as a fixed-point golden
[ ] every example classified: fixed point / fixed-after-bug / divergence
[ ] (bug) fixes are minimal, listed, and keep all oracles green
[ ] (divergence) items recorded for adjudication — NOT silently code-changed
[ ] wide idempotency + equivalence sweep over all .lfe (state count), green
[ ] conformance report produced (per-construct status)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Arc A4 is complete when this slice's ledger is green and the divergence list (if
any) has been adjudicated with the planner.
