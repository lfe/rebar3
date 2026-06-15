# 022 · Arc A5 · S2 — `--dry-run` and `--check` modes (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. Second A5 sub-slice — completes the
> provider. Prereq: S1 merged (in-place mode + resolution + registration).
> **Implementation spec** — implement exactly. **Stop and report** if blocked.
>
> **Output discipline:** Edit in place; don't reprint files; terse prose; run
> tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. `arc5-provider/cc-prompt.md` — shared ref (CLI contract, safety).
3. Your `src/r3lfe_prv_format.erl` (S1) — replace the `--dry-run`/`--check` stubs
   with real behavior. Reuse the S1 file-resolution and per-file format logic.

## 1. Scope of THIS slice — the two no-write modes

File resolution and the format call are unchanged from S1. Only the per-file
**action** and the exit behavior change by mode. `--dry-run` and `--check` remain
mutually exclusive (error if both — keep S1's check). Neither mode **ever**
writes a file.

### `--dry-run` (`-n`) — formatted output to stdout

For each resolved file, format it and print the formatted result to **stdout**
(use `io:format("~ts", [Out])` / `rebar_io` as appropriate — plain stdout, not
the `?INFO` log stream, so it can be piped). When **more than one** file is in the
set, precede each file's output with a header line exactly:

```
;; ==> <relative-or-given-path>
```

and a trailing newline between files. For a **single** file (e.g.
`--path foo.lfe`), print just the formatted content with **no** header (clean for
piping). A parse-error file → `?ERROR` + skip (as in-place); run returns
`{error,…}` if any failed, else `{ok, State}`. Never writes.

### `--check` (`-c`) — CI gate

For each resolved file, format it and compare to the original. Collect the files
whose formatted output **differs** (would change). Never write, never print
file contents. Then:
- if the would-change list is **empty** → `?INFO("All N files are formatted")`,
  return `{ok, State}` (exit 0);
- else → `?ERROR` listing each unformatted file, one per line, and return
  `{error, …}` (non-zero exit) with a summary (`N file(s) need formatting`).
- a parse-error file is reported and also makes the result non-zero.

## 2. Tests — extend `r3lfe_prv_format_SUITE`

- `--dry-run` single file (`--path f.lfe`): stdout equals the formatter output,
  **no header**, and the file on disk is **unchanged**.
- `--dry-run` multiple files: stdout contains a `;; ==> <path>` header before each
  file's content; no files changed. (Capture stdout in the suite — e.g. via
  `ct:capture_start/ct:capture_get` or redirect.)
- `--check` clean (all files already formatted) → `{ok,State}`, no writes.
- `--check` dirty (one unformatted file) → `{error,…}`, the file is **listed**,
  nothing written, result is non-zero.
- `--check` with a syntax-error file → reported, result non-zero.
- `--dry-run --check` together → mutually-exclusive error (still holds).
- Confirm in-place mode (S1) still works (no regression).
- **README parity:** add a check (or a note in the report) that the documented
  examples in README "📐 Consistent Formatting" match the actual flag names,
  short forms, and behavior.

## 3. Constraints

`-spec` exported funcs; `xref` + `dialyzer` clean; `warnings_as_errors`. Neither
mode writes any file. No new deps. Don't modify the formatter/lexer/CST.

## 4. Ledger (closes Arc A5)

```
Arc A5·S2 — dry-run + check
[ ] --dry-run: formatted output to stdout; multi-file uses ';; ==> <path>' headers;
    single file = bare content (pipe-friendly); never writes
[ ] --check: lists would-change files, exit non-zero if any; clean → exit 0;
    never writes
[ ] parse-error file makes both modes non-zero; reported per file
[ ] --dry-run + --check still mutually exclusive
[ ] suite: dry-run single/multi (stdout captured), check clean/dirty, syntax-error,
    both-flags, in-place no-regression — all green
[ ] README examples verified against actual CLI (flags/short forms/behavior)
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions
[ ] files changed + one-line rationale; deviations named — or "none"
```

When green, **Arc A5 is complete** — `rebar3 lfe format` works as documented.
Remaining: A6 (hardening, `docs/commands.md` entry, CHANGELOG, bump to 0.5.5).
