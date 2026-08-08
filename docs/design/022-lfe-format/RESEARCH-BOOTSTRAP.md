# Research-instance bootstrap — LFE formatter (`rebar3 lfe format`)

> **Purpose.** You are a fresh instance of the assistant who has been pairing with
> Duncan (PM/planner) on the LFE source formatter for the `rebar3_lfe` plugin. This
> document hands you the full context so you can dig into **research questions**
> (Duncan will give them) without re-deriving the project. It also defines a
> **reconciliation anchor** so that, if your research yields changes worth
> implementing, you can cleanly merge them against work that continues in the main
> thread.
>
> **THE ANCHOR — memorize this.**
> - **Code checkpoint:** git commit **`528beb3`** ("A7·S4b — closing delimiters
>   never de-indent"), in repo `rebar3_lfe`.
> - **Plan checkpoint:** Arc **A7 through sub-slice S4b complete**; S4c onward not
>   started. (Plan lives in `docs/design/022-lfe-format/`.)
> - Your reasoning starts here. When you propose changes, reconcile against:
>   (1) `git diff 528beb3..HEAD` (what the main thread did after the anchor), and
>   (2) `formatting-rules.md` (the authoritative spec) + the Open Questions below
>   (where your research may diverge). Latest explicit Duncan decision wins;
>   surface conflicts rather than silently overriding.

---

## 1. The project in one paragraph

`rebar3 lfe format` is a new subcommand for the `rebar3_lfe` plugin (shipping in
release **0.5.5**) that reformats LFE source. It is a **comment-preserving**
formatter: it lexes to a lossless token stream, builds a CST that retains comments
and author line breaks, then prints. Target: 80-col width, 2-space indent,
indentation derived from Emacs `lfe-indent.el` (so it is intentionally CL/Lisp
idiomatic). It is **idempotent**, **token-preserving** (except a deliberate
export-sort, planned), **comment-preserving**, and **AST-equivalent** (formatted
output reads back to the same s-expressions). NB: Robert Virding created LFE;
Duncan is a core contributor, not the creator.

## 2. How we work (process — this matters)

- **Roles:** Duncan + this assistant are PM/planner (peer frame). A separate
  **CC** (Claude Code, Sonnet 4.6) does the implementation. The planner writes
  per-slice **prompts** with **ledgers**; CC implements and self-reports; the
  planner **independently audits** CC's work by reading the actual source (no
  Erlang toolchain in the planner's sandbox — audits are by careful reading +
  reasoning, and verifying invariants like "column threaded == spaces emitted").
- **Sub-slice everything.** CC has a 32k output cap per turn. Twice a big slice
  blew it (A3 whole; A7·S2b). **Default: split any non-trivial slice into bounded
  sub-slices**, each with its own prompt + ledger, handed one at a time.
- **Audits earn their keep.** Independent audits caught real things CC's
  self-reports missed: a self-referential oracle, a fill-vs-one-per-line
  deviation, an over-reaching fix, a release-blocking provider bug. Keep auditing.
- **Output discipline for CC:** write to files, don't reprint them, terse prose,
  build incrementally.

## 3. The journey (good + the course-corrections — read this to understand "why")

1. **Research + design.** Studied `lfe-indent.el` (the indentation authority — it
   is a *re-indenter*, defines depth not line-breaks) and the LFE style guide.
   Decided a **full-reflow pretty-printer** (chosen over reindent-only) with a
   comment-preserving lexer + CST, because LFE's `lfe_scan` discards comments.
2. **Built A1–A6** (full-reflow era): A1 lexer (lossless, comment-preserving),
   A2 CST (comment attachment: leading/trailing/dangling), A3 printer (generic
   break algebra), A4 special-form indentation (the `lfe-indent` table; decided
   **align-under-first-arg** for plain calls; always-break let/case/cond/maps;
   def-forms always break body except the no-arg constant idiom), A5 the provider
   (`rebar3 lfe format` with **default in-place / `--dry-run` (stdout) / `--check`
   (CI) / `--path`**), A6 hardening/docs/release.
3. **Bugs found along the way (the valuable part):**
   - **Triple-quote delimiting** (A1) — fixed to match `lfe_scan`'s blank-line rule.
   - **Comment-before-close swallow** (A3/A4) — a `)` after `; comment` ate the
     paren → enforced "a comment ends its line."
   - **Head-leading / head-trailing comment** explosions — narrowed.
   - **`?DEPS = []`** in the provider → the bare command ran before app discovery
     so default `rebar3 lfe format` formatted **0 files** (release-blocker, found
     only by a **real end-to-end CLI test**, not by `do/1` unit tests). Same
     latent bug fixed in `r3lfe_prv_clean`. Lesson: **always e2e-test a rebar3
     plugin through the real CLI**, via `_checkouts` (rebar3 3.27 `{path,…}`
     plugins are broken).
   - **Self-referential oracle** — token-preservation derived tokens from the CST
     on *both* sides, so a parse-level drop was invisible. Re-anchored all three
     sites to **raw lexer tokens**. (A reported "dropped code" turned out to be a
     false alarm — compaction, not loss — but the investigation produced this real
     hardening.)
4. **The big course-correction → A7 (knowledge-gated model).** Duncan ran the
   formatter on real LFE (`lfe/examples`) and found full-reflow **collapsed code he
   had deliberately broken** and mishandled forms it couldn't understand. We
   redesigned (see `formatting-rules.md`): **canonical where we know, break-
   preserving where we don't.** This *revises* parts of A3/A4. A7 is the current
   arc.
5. **CL cross-check.** Ran the formatter's output through `yasi` (CL dialect): for
   forms LFE shares with Common Lisp it is **whitespace-identical** (unsurprising —
   shared `lisp-mode` lineage). Noted in the README.
6. **Codex interlude.** During an Anthropic API outage Duncan used Codex (OpenAI)
   to land S3b-2 (clause routing) and Dialyzer fixes; Codex's own CDC verified
   them. The main-thread planner has **not** independently audited those (commits
   `b14fc78`, `34bbfed`, `b44c586`) — flagged for reconciliation.

## 4. The authoritative model (read `formatting-rules.md` in full)

**Knowledge-gated**, decided per form:
- **Known forms** (special-form table, def-forms, maps, clauses) → **canonical**:
  the formatter owns the layout, *adds/fixes* breaks.
- **Unknown forms** (plain calls, record-gen macros, user macros, all quoted/data
  content) → **break-preserving**: keep the author's break positions, only
  reindent (align-under-first-arg) and wrap >80. (Because grouping like `make-op`'s
  k/v pairs encodes intent we can't recover without macro expansion / `$ENV`.)
- Consequence: canonical output for known forms, author-layout-dependent for
  unknown forms; idempotent throughout. (This is **not** the original "full
  reflow"; it's closer to reindent + required-breaks.)

Always-break (even if it would fit): `if`, `let`/`let*`, `case`, `cond`, `maybe`,
`receive`, `try`, `progn`, maps, def-forms with a body, non-trivial clauses,
`export`/`import`. Flat-if-fits: `lambda` (flat only if single non-structural
body), `when`, comprehensions, `(: …)`, plain calls. Clauses break their body
below the pattern unless it's a single trivial datum. Cons-dot `(a . b)` is its own
token, kept glued. Exports: one-per-line, **alphabetically sorted** (the one
deliberate token-reorder — planned in S5).

## 5. Architecture & where things live

- `src/r3lfe_format_lexer.erl` (457 lines) — lossless tokenizer; `tokens/1`,
  `to_iolist/1`, accessors; `dot` token for cons-dot.
- `src/r3lfe_format_cst.erl` (366) — CST; comment attachment; `nl_before/1`
  (author break positions, for break-preserving), `multiline/1`, `dot_token/1`.
- `src/r3lfe_formatter.erl` (1387) — the printer: `regime/2` (canonical vs
  break-preserving, threading `InData` for quote/data context), `must_break/1`,
  `is_always_break_head/1`, `render_clause`/`trivial_clause`, the break-preserving
  renderer, `print_classified` per head class.
- `src/r3lfe_prv_format.erl` (260) — the provider; modes; file resolution.
- Tests: `test/r3lfe_formatter_SUITE.erl` (CT, the bulk), `r3lfe_format_lexer_SUITE`,
  `r3lfe_format_cst_SUITE`, `r3lfe_prv_format_SUITE`, `r3lfe_properties.erl`
  (PropEr), `test/e2e/format_e2e.sh` (real-CLI e2e via `_checkouts`).
- **Oracles** (used everywhere): idempotency `format(format(x))==format(x)`;
  token-preservation (raw lexer, non-trivia, order-sensitive); comment-preservation
  (ordered comment texts); AST-equivalence (`lfe_io:read_string`, excl. `#.(`).
- Docs: `formatting-rules.md` (spec), `rebar3-lfe-provider.md` (design),
  `cc-prompts.md` (arc index/status), `arc7-rules-v2/` (current arc prompts),
  `formatting-gallery.md` (showcase — **PAUSED**, regenerated in S6),
  `SMOKE.md` (manual CLI test).

## 6. Current state @ `528beb3`

Done in A7: **S1** cons-dot; **oracle-fix** (all 3 token oracles raw-lexer);
**S2a** CST `nl_before`; **S2b** regimes + break-preserving renderer (incl. fix1:
flat-overflow wraps one-per-line, grouping preserved); **S3a** always-break
additions; **S3b** clause rule (all clause forms); **S3c** lambda rule; **S4a**
signature-never-alone (narrowed fix2); **S4b** closing-delimiters-never-de-indent
(committed at the anchor; **planner-audited ✅** — one-line `close_section` change,
both triggers, hug branch unchanged, 4 exact+idempotent regression tests; 680
total tests). `vsn` is already `0.5.5`. `make check` was clean as of the last report. (A1–A6 from the full-reflow era are
in history but partly *superseded* by A7 per `formatting-rules.md`.)

**Post-anchor progress (main thread, beyond `528beb3`):** **S4c** flet/fletrec
locals render defun-like when broken (flat-if-fits preserved) — `f693706`,
planner-audited ✅. **S4c·fix1** match-clause local fns now route through
`print_clause_loop`/`render_clause` (wide clauses get pattern-on-line, body-below;
progn/let regression-guarded) — `5a9e388`, planner-audited ✅ (688 tests). The S4c
clause gap is **closed**. **S4d** try full-symmetry (§3.7, Duncan's decision):
`try` alone, body + sections at +2, section keywords alone with contents at +4,
case/catch clauses via `render_clause` (also closes the S3b-2 clause-routing
deferral) — `df7d6a1`, planner-audited ✅ (693 tests). A research instance should
reconcile against these deltas (HEAD now `df7d6a1`). **S4e** data-list head
comment inline (§3.9): in a quoted/quasiquoted list, a leading comment on the
first element stays on the opener line, elements at `C+len(Open)`; code lists keep
opener-alone — `941442e`, planner-audited ✅ (engine correct; one regression test
`dhc_code_list_unchanged` is mis-targeted — its comment precedes `arg1` not the
head, so it doesn't exercise the `InData=false` head-comment branch; test-only fix
pending in S5b). **S5a** export/import one-per-line + **+1 indent** (under the
keyword, `C+OpenLen`; via `EffIndent` so non-export specforms stay byte-identical);
force-break with entries; import `from`/`rename` internals deferred to S5c — 702
tests, planner-audited ✅. **S5b** export alphabetical sort (stable, by
`{name, arity}`; `(export all)`/mixed/**any-commented** export → order preserved)
+ the **oracle carve-out** (token→multiset, AST→`normalize_module_decls`, comment
oracle left strict; load-bearing self-tests) + the bundled `dhc_code_list_unchanged`
fix — 710 tests, planner-audited ✅. Note: the "commented exports preserve order"
rule is a sound S5b deviation from the original spec (avoids a third carve-out);
ratified into §3.5. **S5c** import full: nested `from`/`rename` keyword+module on
head line, entries one-per-line at +1, sorted within each clause (rename by old
name/arity), clause order preserved; `normalize_module_decls` import case populated
— 717 tests, planner-audited ✅. **Latent finding (S5c·fix1 pending, also fixes
S5b):** the sort-suppression checks only *leading* comments; an entry with a
*trailing* comment (and no leading) is still sorted, moving its comment and
reordering the comment stream (violates comment-preservation). **S5c·fix1** closed
it: `entry_has_comment` (leading OR trailing) used in all three suppression sites
(export + import from/rename) — 720 tests, planner-audited ✅. **S6** gallery
regenerated against final 0.5.5 behavior (all 60 fences verified idempotent; stale
descriptions fixed; header→v2; §10 added #61 wide-flet/#62 cons-dot/#63
commented-export) + full sweep green (CT + PropEr 4-oracle + corpus + e2e, xref/
dialyzer clean, formatter frozen) — planner-audited ✅. **A7 is verification-
complete.** Only **A6 (release prep → cut 0.5.5)** remains.

## 7. What's left (the forward plan)

- **A7·S4c** — flet/fletrec locals format like defuns (rules §3.6). **DONE
  (`f693706` + fix1 `5a9e388`)** — wide-match-clause gap closed.
- **A7·S4d** — try section symmetry (#10) + route try `case`/`catch` clauses
  through `render_clause` (deferred from S3b-2). **DONE (`df7d6a1`)** — full
  symmetry; §3.7 finalized.
- **A7·S4e** — data-list head comment inline with `(` (§3.9; reconcile with the A3
  head-leading-comment / opener-alone handling).
- **A7·S5** — exports: one-per-line + **alphabetical sort** (name then arity) +
  a **token-preservation oracle carve-out** for export/import reordering.
- **A7·S6** — **regenerate the gallery** from current output; full property +
  corpus + e2e sweep; confirm all invariants.
- **A6 (release), resumed after A7** — `docs/commands.md` `format` entry,
  CHANGELOG `0.5.5`, confirm `vsn`, run/confirm e2e + SMOKE, release-readiness
  checklist (CC does **not** publish — per `CLAUDE.md`, no `--allow-dirty`, no push).

## 8. Open questions / research seeds (where your research may land)

- **Export indentation +1 vs +2** (§3.5) — mnesia example suggests +1; current
  build +2. To settle in S5.
- **Proplist force-break** (§3.4) — force-break a detected all-2-tuple list even
  when it fits flat? Default: no.
- **Flat-overflow data tuples** put 2 elements on the opener line (uniform
  head+first-arg). Acceptable; revisit if a per-data-container rule is wanted.
- **CRLF** — currently normalized to LF (documented); no preservation option.
- **Configurability** — width/indent are hard-coded (80/2); no `.lfe_format`
  config yet. Out of scope for 0.5.5.
- Deeper **CL-alignment** beyond the shared subset; **performance** on large files;
  a **standalone `lfefmt`**/editor integration; **macro-aware** k/v detection
  (would need expansion / `$ENV`).

## 9. Reconciliation protocol (for you, the research instance)

1. Your baseline is `528beb3`. Before proposing code, run `git diff 528beb3..HEAD`
   to see what the main thread changed after the anchor (S4c+).
2. Express your research conclusions against **`formatting-rules.md`** (the spec)
   and the Open Questions — say which section/rule each idea touches.
3. For each idea: is it (a) a new rule, (b) a change to an existing rule, or (c) a
   bug? Changes to existing rules are **Duncan's call** (he owns formatting
   aesthetics); surface them as decisions, don't assume.
4. Where your idea conflicts with a post-anchor change, the **latest explicit
   Duncan decision wins**; present the divergence and a merge path.
5. If implementation is warranted, follow the house discipline: arc/slice prompts
   with ledgers, sub-sliced to the cap, oracle-backed, independently audited.

## 10. First things to read

`formatting-rules.md` → `cc-prompts.md` (status) → `arc7-rules-v2/cc-prompt.md`
(arc map) → `rebar3-lfe-provider.md` (design) → skim `src/r3lfe_formatter.erl`.
The planner's cross-session memory also summarizes the locked decisions and
lessons.
