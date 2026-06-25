# 022 · Arc A7 · S6 — gallery regen + full sweep (CC spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. A7 capstone (verification, not new
> behavior). **Stop and report** if a formatter bug surfaces — do **not** edit the
> formatter from this task.
>
> **Output discipline (critical — large):** work **one § per turn** for the gallery;
> write straight into the docs with Edit; don't paste files back; terse prose.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines`.
2. `cc-prompt-gallery.md` — the **fill workflow + the 60 inputs** (compile-verify,
   format, write exact output, idempotency + token check per entry). Use it as the
   mechanism.
3. `formatting-rules.md` — the **final v2 behavior** the gallery must reflect.
4. `formatting-gallery.md` — the doc to regenerate (its fences + descriptions are
   **stale**, pre-A7).
5. `test/e2e/format_e2e.sh`, `SMOKE.md` — the real-CLI checks.

## 1. Part A — regenerate the gallery against FINAL behavior (one § per turn)

For every entry, run the `cc-prompt-gallery.md` workflow against the **current**
formatter and write the **exact** output into the fence. The prior fence contents
are stale — replace them. **Also fix any entry description text that asserts
now-wrong behavior.** Known-stale descriptions to correct (not exhaustive — check
each):

- **#23 try** — now **full symmetry** (§3.7): `try` alone, body + each section at
  +2, section keyword alone, contents at +4, case/catch clauses via `render_clause`.
- **#31 flet** — flat-if-fits when it fits (unchanged), but the description must note
  that **when an flet breaks** its locals render **defun-like** (§3.6). The #31
  input fits, so its fence stays flat; do not claim it breaks.
- **#38 defmodule export** — now **always one-per-line at +1** (under the keyword,
  `C+OpenLen`), **alphabetically sorted** by name then arity. Drop the old "+2" and
  "short export stays flat" claims.
- **#39 import** — now **nested**: `import` alone, `(from M …)`/`(rename M …)`
  keyword+module on the head line, entries one-per-line at +1, sorted within each
  clause; clause order preserved.
- Any entry whose blurb cites a specific indent/always-break/align rule that A7
  changed — reconcile against `formatting-rules.md`.

Per-entry invariants still hold: each fence is a `format/1` **fixed point**
(idempotent) and **token-preserving** (multiset, per the S5b carve-out). If any
entry is not idempotent or drops a token → **stop and report** (formatter bug;
don't paper over, don't fix here).

## 2. Part B — header + status

- Update the gallery **header conventions** to v2: the **knowledge-gated model**
  (canonical for known forms; break-preserving for unknown/data); the always-break
  set (`if`/`let`/`let*`/`case`/`cond`/`progn`/`receive`/`try`/`maybe`/maps/def-bodies);
  **try full symmetry**; **export/import one-per-line at +1, sorted**; data-list head
  comment on the opener line; cons-dot kept glued.
- Update the **status line** from "empty pending CC / paused" to: generated &
  verified against the 0.5.5 formatter (state the date).

## 3. Part C — append a small §10 for A7 behaviors not already shown (optional, ≤3)

Keep stable IDs 1–60 unchanged; **append** new entries (61+) so nothing renumbers.
Add only if not already represented:

- **wide flet → defun-like break** (the §3.6 case #31 can't show because it fits).
- **cons-dot / improper list** `(a . b)` kept glued (§3.8), if no existing entry
  shows it.
- **commented export preserved** (sort suppressed when an entry has a comment, §3.5).

Each new entry follows the same fill workflow (verified output, idempotent,
token-preserving).

## 4. Part D — full sweep + invariant confirmation

Run and confirm green (state the numbers):

- **Full CT suite** (`r3lfe_formatter_SUITE` + the lexer/cst/provider suites).
- **PropEr property suite** (`r3lfe_properties`) — all four oracles:
  idempotency, token-multiset, comment-preservation, normalized-AST.
- **Corpus sweep** (the `sweep_oracles` over the corpus files).
- **e2e** (`test/e2e/format_e2e.sh`) and the `SMOKE.md` manual checks — the real
  `rebar3 lfe format` CLI path (default in-place, `--dry-run`, `--check`, `--path`).
- `xref` + `dialyzer` clean; `warnings_as_errors`.

State the **final total test count**. Confirm **no `src/` changes** in this task
(gallery + docs only; the formatter is frozen for S6). Any surfaced bug → stop and
report as a separate finding.

## 5. Ledger

```
Arc A7·S6 — gallery regen + full sweep
[ ] all 60 fences regenerated from the CURRENT formatter (verified output, one § per turn)
[ ] stale entry descriptions corrected (#23 try, #31 flet, #38 export, #39 import, others)
[ ] header conventions + status line updated to v2 (gallery unpaused)
[ ] §10 appended for uncovered A7 behaviors (≤3; IDs 1–60 unchanged) — or "none needed"
[ ] every entry idempotent + token-preserving; any non-fixed-point reported (not papered over)
[ ] full CT + PropEr (4 oracles) + corpus sweep + e2e + SMOKE green; final count stated
[ ] xref + dialyzer clean; warnings_as_errors; NO src/ changes (formatter frozen)
[ ] files changed + one-line rationale; bugs surfaced reported separately; deviations or "none"
```

After S6, A7 is verification-complete and the gallery is a true, idempotent showcase
of 0.5.5 behavior. Next: A6 release prep → cut 0.5.5.
