# 022 · Arc A3 · S3 — comments, blank lines, edges (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Final A3 sub-slice — completes Arc A3.
> Prereq: S1 + S2 merged (flat + breaking work on comment-free input). Implement
> exactly this; **stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md`; `erlang-guidelines` (`11-anti-patterns.md` first).
2. `arc3-printer/cc-prompt.md` shared ref — §3.1 (comment clause of flat
   eligibility), §4 (comment/trivia emission), §5 (blank/final-newline), §6
   (oracles). This slice implements those.
3. Your S1+S2 `src/r3lfe_formatter.erl` (extend); closed lexer/CST. CST trivia API:
   `leading/1` `trailing/1` `dangling/1`; `document_dangling/1`; `comments/1`.
   `trivia()` = `{comment, token()} | blank`.

## 1. Scope of THIS slice — lift the comment-free restriction

- **Flat eligibility gains its comment clause** (shared ref §3.1): a node with any
  `leading`/`trailing`/`dangling` comment in its subtree is **not** flat-eligible
  → it breaks. (Multi-line tokens already force breaks from S2.)
- **Comment emission** (shared ref §4):
  - `leading` → each on its own line at the node's indent, before the node;
  - `trailing` (single) → same line after the node: `…node ; comment`, one space
    before the delimiter, then newline (forces the container broken);
  - `dangling` → own lines at `C+2` after the last child; then `<CLOSE>` on its
    own line at `C`;
  - block comments `#| … |#` emitted verbatim; update column after last `\n`.
- **Blank-line policy** (shared ref §5): a `blank` in `leading` → exactly one
  empty line; never >1 consecutive; none right after an opener or at doc start.
- **Edges:** empty input (decide `""` vs single `\n` — state which and test it);
  comment-only file; CRLF input normalized to LF (confirm `\r` is dropped).

## 2. Tests — extend the suite, groups `comments`, `edge`, and full-corpus `oracles`

- `comments`: leading own-line comment before a `defun`; trailing `; bar` after a
  form; a `dangling` comment before a closing paren; a blank line between two top
  forms preserved as one; a block comment inside a form.
- `edge`: empty file; comment-only file; >80 form with an embedded comment; a
  `tqstring`-containing form; CRLF→LF.
- `oracles` (the real A3 acceptance): run **all four** — idempotency,
  token-preservation, **comment-preservation**, AST-equivalence — over the
  **full corpus**: every `_integration/*.lfe`, the A1 `tq_corpus` fixture, and
  the targeted snippets. Include `#'` fun_ref in AST-equivalence (prefixes glue);
  exclude only `#.(` read-eval.

## 3. Constraints

Pure module; `-spec`; `xref`+`dialyzer` clean; `warnings_as_errors`; don't modify
lexer/CST (report bugs). Comment-preservation must now hold — no comment dropped,
duplicated, or reordered.

## 4. Ledger (closes Arc A3)

```
Arc A3·S3 — comments + blanks + edges
[ ] flat-eligibility comment clause; comment subtree forces break
[ ] leading / trailing / dangling emission per §4; block comments verbatim
[ ] blank-line policy per §5; single final newline; empty-file behavior stated
[ ] CRLF→LF confirmed
[ ] group `comments` + group `edge` green
[ ] full-corpus `oracles`: idempotency + token + comment + AST-equiv, green
    (state file count; note read-eval exclusion)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

Arc A3 is complete when this slice's ledger is green.
