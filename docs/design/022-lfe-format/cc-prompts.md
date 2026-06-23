# 022 — CC prompts for `rebar3 lfe format`

Companion to `rebar3-lfe-provider.md`. Hand these to CC **one arc at a time**.
Each arc lives in its own subdir: `arcN-<slug>/cc-prompt.md` (the spec) and
`arcN-<slug>/cc-prompt-fixM-<slug>.md` (scoped fixes). A2 is ready now; A3–A6 are
drafts to be tightened after the prior arc's report is graded.

Status: **A1–A5 ✅** · **A7 ACTIVE** (formatting model v2 — knowledge-gated; spec =
`formatting-rules.md`; arc map = `arc7-rules-v2/cc-prompt.md`; gallery PAUSED).
Done in A7: S1 cons-dot ✅, oracle-fix ✅ (all 3 token oracles raw-lexer), S2a ✅,
S2b ✅ (regimes + break-preserving renderer; flat-overflow wraps one-per-line,
grouping preserved), S3a ✅ (always-break: if/progn/receive/try/maybe).
S3 ✅ (S3a always-break, S3b clauses [S3b-2 via Codex], S3c lambda; 670 green).
dialyzer fixes ✅ (Codex). **S4 pre-split** (5 refinements): S4a sig-never-alone ✅ (fix2 narrowed correctly).
**Next: S4b close-no-de-indent READY** (`cc-prompt-s4b-close-deindent.md` — lone
close aligns with content indent, not form column). Then S4c flet-as-defun, S4d
try symmetry+clauses, S4e head-comment; then
S4 (sig/flet/try/head-comment/close-de-indent), S5 (exports sort), S6 (gallery
regen + full sweep). #7 dropped-code was a FALSE ALARM. Minor logged: flat-overflow
data tuples put 2 elems on the opener line (uniform head+first-arg) — revisit if
wanted. **A6 (release) deferred until A7 lands.** — older A6 detail below —

Status (history): **A6 NEXT** (release;
`arc6-release/`: **S0 e2e-CLI ✅** (real `rebar3 lfe format` via _checkouts; found+
fixed a RELEASE-BLOCKING bug — `?DEPS=[]` ran the bare provider before app
discovery → default format found 0 files; fix `{default, app_discovery}`; 22
asserts), S1 hardening ✅ + **S1·fix1 (dangling-blank, test-first) pending**,
S2 docs+release pending).

Sidecar fix (Duncan: do it now, bundle into 0.5.5): `r3lfe_prv_clean` had the same
`?DEPS=[]` app-discovery bug → fix prompt `arc6-release/cc-prompt-clean-app-discovery-fix.md`
(one-line DEPS fix + e2e assertion: compile→.beam, standalone clean removes them).
CHANGELOG note for it lands in A6·S2.
⚠️ Gap found: provider is only tested via `do/1` directly — NO test runs the real
`rebar3 lfe format` through rebar3's plugin machinery (the `Command lfe not found`
class). S0 closes that with a local-path-plugin fixture + shell-out asserts.
Engine + provider complete;
`vsn` already `0.5.5`. A6 = property/fuzz/edge hardening, docs/commands.md,
CHANGELOG, release-readiness checklist (no publish). — older detail below —

Status (history): **A4 IN PROGRESS** (S1 ✅ incl. fix1+fix2;
comment-on-head-line matrix exhaustively handled & in the corpus) · S2 ✅
(def-forms: constant-idiom flat, with-args break, dynamic N) · **S3 split into
S3a ✅ + S3b ✅ (conformance) + S3c ✅ (always-break let/case/cond/map). 6
divergences adjudicated → **S3d READY** = `arc4-indent/cc-prompt-s3d-export-guards.md`
(export/import keyword-alone + clause guards; **closes A4**) · **A5 ✅**
(provider: in-place / --dry-run / --check / --path; dry-run+check provably never
write — single write site reachable only from in-place; 601/601) · **A6 NEXT**
(hardening, docs/commands.md, CHANGELOG, bump to 0.5.5).

CL cross-check (yasi, Lisp dialect): formatter output is whitespace-identical to
a CL indenter on the CL-shared forms; always-break rules are deliberate house
style. Noted in README "📐 Consistent Formatting".

A4 divergence rulings (Duncan): always-break let/let* (+ binding lists
one-per-line), case/cond, maps [S3c ✅]; **export/import keyword-alone** with items
indented [S3d]; match-clause **pattern+guard on one line** [S3d]; trailing-comment
alignment → accept divergence (single space, no change). **S3d = last A4 slice.**

**A5 CLI surface (final, adjudicated with Duncan):** `rebar3 lfe format` edits in
place by default; `--dry-run`/`-n` prints formatted output to stdout (multi-file:
each under a `;; ==> <path>` header); `--check`/`-c` is the CI gate (no writes,
exit non-zero + list unformatted files); `--path`/`-p` (file or dir) composes with
all three. Documented in README "📐 Consistent Formatting" and design §6.

**README** updated (Quick Start, "📐 Consistent Formatting" feature section, and
the Core commands list) with the in-place / dry-run / check usage matrix.

**Formatting gallery** (`formatting-gallery.md` + `cc-prompt-gallery.md`): a
60-entry compile-verified showcase of the formatter's canonical output, filled by
CC (verify-compile → format → write fence), one § per turn. ⚠️ Best filled
**after A4 fully closes** (S3c+S3d), so the let/case/cond/map/export/guard entries
capture the final rules rather than needing a refill.

⚠️ Found in S2 audit (latent since S1, NOT a closed-arc escape — data-container
alignment was never in scope before S3): `print_broken_container` runs
`classify_head` for ALL container types, so a breaking `tuple`/`map`/`binary`
misreads its first element as a function head (`#(case x)` → case specform; maps
get funcall alignment, not key-value pairs). S3a fixes this.

A4·S2 decision (Duncan): def-form body breaking is **nuanced** — a `defun`/
`defmacro` with an **empty arglist** (constant idiom, `(defun +my-pi+ () 3.14)`)
stays flat-if-fits; one **with args** or match-clause **always breaks** its body;
other defforms (defmodule/defrecord/…) always break. Via `flat_width` → `infinity`
for defform-headed lists *except* empty-arglist defun/defmacro. `defun`/`defmacro`
use dynamic N (2 = signature form, 1 = match-clause) routed through specform.

⚠️ **Audit lesson (recurring):** "oracles green over the corpus" ≠ invariant
holds. The `_integration` corpus lacks adversarial comment placements, so two
real bugs (A3 head-leading idempotency; A4·S1 comment-swallows-close) passed all
oracles. Audits must *construct* comment-placement cases — trailing comment on the
last child, comments between `(` and head, comments on distinguished args — not
rely on the corpus. Each found bug gets added to the corpus fixture permanently.

A4 decision (Duncan): plain calls **align continuation args under the first arg**
(faithful to `lfe-indent.el`), not +2 hang. Shared ref `arc4-indent/cc-prompt.md`
carries the full indent table + reflow semantics. Sub-slices: S1 = table + 3
primitives (specform / funcall-align / list-head) + provisional defform; S2 =
def-forms + docstrings + clause layout; S3 = style-guide conformance.

A3·S2 added the +2 break algebra: 0-based column threaded through
`print_node/2` → `print_broken_container/2`; `flat_width/1` is exactly
`length(flat_render/1)` so the `Col + W =< 80` fit-check is sound; idempotency
holds. S3 (comments/blanks/edges, full-corpus oracles) closes the arc.

A3·S1 shipped `src/r3lfe_formatter.erl` with `format/1` (flat rendering) +
`r3lfe_formatter_SUITE` group `flat`. Public return spec is the abstract
`{ok, iolist()} | {error, term()}`, with a documented targeted
`-dialyzer({no_underspecs, format/1})` — **keep both in S2/S3; do not narrow.**

Every prompt assumes CC will, at the top of the session:
- read `CLAUDE.md` (project conventions; `warnings_as_errors` is ON),
- load the `erlang-guidelines` skill and read `11-anti-patterns.md` first,
- read `rebar3-lfe-provider.md` (the design),
- treat the existing providers as the house style.

Shared non-negotiables to restate in each handoff:
- The formatter **must never reorder, insert, or drop a token** — only
  whitespace/newlines/indentation may change.
- **Comments must be preserved** (line `;` and block `#| |#`).
- `-spec` every exported function; code must pass `rebar3 xref` and
  `rebar3 dialyzer` clean and compile under `warnings_as_errors`.
- Report against the arc's ledger rows at the end; name any deferral, no silent
  drops.

**Output-budget discipline (every prompt — CC is Sonnet 4.6 with a 32k output
cap per turn).** A one-shot attempt at all of A3 blew the cap and produced
nothing. So: keep each handoff's deliverable **small** (one module portion + a
small test group); write code straight to files with Write/Edit and **do not
paste files back** into the reply; keep prose to a few lines; build modules and
suites **incrementally**, running tests between steps. When an arc's deliverable
is large, it is **split into sub-slices** (`cc-prompt-sN-<slug>.md`) handed one at
a time — prefer this to raising `CLAUDE_CODE_MAX_OUTPUT_TOKENS` (which only lets a
failed attempt waste more). Apply the same sub-slicing to A4–A6.

---

## Arc A1 — Lossless, comment-preserving tokenizer  *(CLOSED ✅)*

> Spec: `arc1-lexer/cc-prompt.md`; fix iteration: `arc1-lexer/cc-prompt-fix1-tqstring.md`.
> Shipped `src/r3lfe_format_lexer.erl` with the API A2 builds on:
> `tokens/1 -> {ok,[token()]} | {error,term()}`, `to_iolist/1`, and accessors
> `kind/1` `text/1` `line/1` `col/1`. `token()` is opaque; `kind()` enumerates
> the token kinds. The stream is **flat** and includes `whitespace`, `newline`,
> `line_comment`, and `block_comment` as first-class tokens.

**Context.** You are adding an LFE source formatter to the `rebar3_lfe` plugin.
The foundation is a tokenizer that, unlike LFE's own `lfe_scan` (which throws
comments and whitespace away), keeps **everything**. Read
`docs/design/022-lfe-format-provider.md` sections 2 and 4.1, and study
`_build/default/lib/lfe/src/lfe_scan.erl` (or the `lfe` dep source) — you will
**adapt its literal-tokenizing logic**, not reinvent it. Do not call `lfe_scan`
at runtime; we need positions and trivia it discards.

**Task.** Create `src/r3lfe_format_lexer.erl` exposing:

```erlang
-spec tokens(binary() | string()) -> {ok, [token()]} | {error, term()}.
-spec to_iolist([token()]) -> iolist().   %% inverse: concat of verbatim texts
```

Design a `token()` type (suggested: a record `#tok{kind, text, line, col}` with
an exported opaque or transparent type, plus accessor funs). Produce a **flat**
stream that includes whitespace, newline, and blank-line tokens as first-class
items, so that reconstruction is trivial. Comment attachment to AST nodes is a
*later* arc (A2) — A1 just emits faithful tokens.

**Token kinds to cover** (enumerate from the LFE guide / `lfe_scan`):
- structural: `( ) [ ]`
- literal openers: `#(` (tuple), `#m(` / `#M(` (map), `#b(` / `#B(` (binary),
  `#.(` (read-eval)
- prefixes: `'` `` ` `` `,` `,@` and the deprecated `#'name/arity`
- atoms/symbols including bar-quoted `|…|` (with escaped `\|`)
- numbers in every base (`123`, `#b101`, `#*101`, `#o377`, `#d99`, `#xFF`,
  `#2r1010`, `#36rZ`), floats (`1.0e10`), and char literals `#\a`, `#\x1f42d;`
- strings: normal `"…"` (with escapes), binary `#"…"`, and **triple-quoted**
  `"""…"""` / `#"""…"""` (verbatim, multi-line)
- comments: line `;…\n` and block `#| … |#` (may span lines; not nestable)
- trivia: runs of spaces/tabs, single newlines, and blank-line runs

**The one guarantee that defines done:** for any input,
`iolist_to_binary(to_iolist(element(2, tokens(Src)))) =:= unicode_binary(Src)`,
**byte-for-byte**. This is the primary test.

**Tests.** Add `test/r3lfe_format_lexer_SUITE.erl` (Common Test, matching repo
convention; tight eunit blocks inside the module are fine too). Include:
- a round-trip property/corpus covering **every** kind above, plus real LFE files
  (point it at the repo's own `_integration` `.lfe` files and a handful of
  hand-written edge cases: empty file, file with no trailing newline, file that
  is only a comment, CRLF if you choose to support it — otherwise document
  LF-only);
- kind-classification assertions on tricky cases (`#\;` char vs `;` comment;
  `#"…"` binary string vs `#(…)`; `|a;b|` bar-symbol containing a semicolon;
  triple-quote containing `"` and `#|`).

**Ledger rows to report against:** see design §8 (compile clean, tests added &
passing, lossless oracle green, no regressions, xref+dialyzer clean, files+rationale).

**Out of scope for A1:** any tree building, any pretty-printing, any provider.
Stop at a proven-lossless token stream.

---

## Arc A2 — CST with comment attachment  *(CLOSED ✅)*

> Spec: `arc2-cst/cc-prompt.md`. Shipped `src/r3lfe_format_cst.erl`. API A3 builds
> on: `parse/1 -> {ok, cst_document()} | {error,term()}`; node accessors `type/1`
> `open/1` `close/1` `prefix/1` `children/1` `leading/1` `trailing/1`
> `dangling/1`; `document_children/1` `document_dangling/1`; plus
> `significant_tokens/1` and `comments/1` (oracle helpers). Types renamed
> `cst_node/0` / `cst_document/0` (avoid built-in `node/0` clash). `trivia()` =
> `{comment, token()} | blank`. **A3 constraint surfaced here:** prefix nodes
> (`'` `` ` `` `,` `,@` `#'`) must print *glued* to their inner node (no space) —
> `#' foo/2` is rejected by the reader.

**Task.** Create `src/r3lfe_format_cst.erl`: consume `r3lfe_format_lexer:tokens/1`
and build a tree of nodes (`list`/`tuple`/`map`/`binary`/`eval`/`atom`/`number`/
`string`/`char`/`symbol`/`prefixed`), recording bracket style (`(` vs `[`).
Attach each comment and blank-line run to a node as **leading**, **trailing**, or
**dangling** trivia.

**Acceptance (corrected from the old draft — byte-for-byte belongs to A1, not
A2).** (1) **Token-preservation**: the significant (non-trivia) tokens extracted
from the CST equal the lexer's significant token sequence — no reorder/drop/add.
(2) **Comment-preservation**: the ordered comment texts in the CST equal those in
the lexer stream. (3) **AST-equivalence**: a trivia-stripped serialization of the
CST, read back through the real `lfe_scan`/`lfe_parse`, equals the original
parse. Plus attachment unit tests: `(foo) ; bar` → trailing on `foo`; `;;; sec`
above a `defun` → leading; comment before a lone `)` → dangling.

---

## Arc A3 — Printer core (generic, no special forms)  *(DRAFT)*

**Task.** Create `src/r3lfe_formatter.erl` with
`-spec format(binary()|string()) -> {ok, iolist()} | {error, term()}` that
lexes → builds CST → prints. Implement the "try flat, else break" algebra:
print a form on one line if it fits in 80 cols, else break with +2 nesting,
one argument per line. Emit leading comments on their own line at current indent;
a trailing comment forces a line break after its node; block comments verbatim.
Blank-line policy: ≤1 blank between forms, collapse runs. **No** special-form
indent table yet — every head uses the generic rule.

**Acceptance.** Idempotency (`format(format(x)) == format(x)`) and
AST-equivalence on the corpus; visually reasonable output on generic forms;
comments never lost (round-trip the corpus and diff comment sets).

> **Split (after the 32k-cap incident).** `arc3-printer/cc-prompt.md` is now the
> **shared reference**; hand CC the sub-slices one at a time:
> `cc-prompt-s1-flat.md` (pipeline + flat render, comment-free) →
> `cc-prompt-s2-breaking.md` (the +2 break algebra, comment-free) →
> `cc-prompt-s3-comments.md` (comments/blanks/edges; full-corpus oracles — closes
> A3). Each is sized to stay well under the output cap.

---

## Arc A4 — Special-form indentation + alignment  *(DRAFT — highest craft)*

**Task.** Port the `lfe-indent.el` indent table (design §3) into the printer's
break/nest decisions: `defform` for `def*`/`defun`; `specform N` for
`case`/`let`/`if`/`lambda`/`receive`/`try`/`catch`/`call`/`:`/etc.; default
funcall alignment; head-is-a-list vertical alignment. Add the style-guide
alignment rules: `let`/`let*` one binding per line aligned under the first;
`cond`/`case` one clause per line; `defun` pattern-match clauses compact;
docstrings on their own line; maps/tuples/proplists **not** column-aligned.

**Acceptance.** Golden-file tests: the LFE style-guide examples (and a curated
fixture set) format to their canonical form; the formatter is a fixed point on
them; AST-equivalence and comment preservation still hold.

---

## Arc A5 — Provider + CLI wiring  *(DESIGNED — sub-sliced in `arc5-provider/`)*

> Hand CC the sub-slices one at a time: `arc5-provider/cc-prompt-s1-inplace.md`
> (provider skeleton + file resolution + in-place mode + registration) →
> `arc5-provider/cc-prompt-s2-dryrun-check.md` (`--dry-run` stdout + `--check` CI).
> Shared ref + concrete plumbing API in `arc5-provider/cc-prompt.md`. Final CLI:
> in-place default; `--dry-run`/`-n`; `--check`/`-c`; `--path`/`-p`.

Original outline (superseded by the arc5-provider prompts):

**Task.** Create `src/r3lfe_prv_format.erl` modeled on `r3lfe_prv_clean` /
`r3lfe_prv_eval` (design §6). Declare `--check`/`-c` (boolean) and `--path`/`-p`
(string). `do/1`: build the file list (`--path` file-or-dir, else
`r3lfe_config:get_src_dirs/1` → `r3lfe_package:discover_files/1`); for each file
read → `r3lfe_formatter:format/1` → write-if-changed (normal) or collect-changed
(check). Return `{ok,State}` normally; in `--check`, `{error,…}` (non-zero)
listing files that would change. Register `r3lfe_prv_format` in `rebar3_lfe.erl`.

**Acceptance.** `test/r3lfe_prv_format_SUITE.erl` (CT) covering: in-place format
of a temp project, `--check` clean vs dirty (exit code), `--path` to a single
file and to a dir, default src_dir resolution. `rebar3 lfe format --check` runs
clean against the repo's `_integration` project after a one-time format pass.

---

## Arc A6 — Hardening, docs, release  *(DRAFT)*

**Task.** Edge cases (empty files, `#.(…)`, unicode symbols/strings, missing
trailing newline, very long atoms/strings that can't fit 80 cols — must not
loop or crash). Add a PropEr idempotency property (generate or sample real LFE,
assert fixed point + AST-equivalence). Update README + plugin docs with the
`format` command. Add a CHANGELOG entry. Bump `src/rebar3_lfe.app.src` `vsn` to
`0.5.5`.

**Acceptance.** Property tests green; full `rebar3 ct`/`eunit`/`xref`/`dialyzer`
clean; docs and CHANGELOG updated; version bumped.

---

## Working rhythm

1. Hand CC the arc prompt. 2. CC implements + self-reviews + reports against the
ledger. 3. Duncan shares the report here; planner verifies each ledger row
against the actual artifacts (independent audit), grades, requests fixes.
4. Iterate to full acceptance (≤5 iterations), close the arc, tighten the next
draft prompt with what we learned. 5. After A6, cut **rebar3_lfe 0.5.5**.
