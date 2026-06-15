# 022 · Arc A5 · S1 — provider + in-place mode (CC implementation spec)

> Target: Sonnet 4.6 + `erlang-guidelines`. First A5 sub-slice. **Implementation
> spec** — implement exactly. **Stop and report** if blocked.
>
> **Output discipline:** write to files with Write/Edit; don't reprint files;
> terse prose; run tests; report briefly.

## 0. Orientation

1. `CLAUDE.md` (`warnings_as_errors` ON); `erlang-guidelines` (`11-anti-patterns`).
2. `arc5-provider/cc-prompt.md` — the **shared reference** (CLI contract, the
   plumbing API, safety rules). Read it.
3. `src/r3lfe_prv_clean.erl` — copy its structure. `src/r3lfe_prv_eval.erl` — see
   how it declares `opts` and reads `command_parsed_args`.
4. `src/r3lfe_formatter.erl` — `format/1` (the closed engine you call).
5. `src/r3lfe_config.erl` (`get_src_dirs/1`), `src/r3lfe_package.erl`
   (`discover_files/1`), `include/r3lfe.hrl`, `src/rebar3_lfe.erl` (registration).

## 1. Scope of THIS slice

Create `src/r3lfe_prv_format.erl` (`-behaviour(provider)`) and register it. Wire
the full `opts` list (from the shared ref — declare all three flags now), but
implement **only the in-place (default) mode**. `--dry-run` and `--check` are S2 —
for this slice, if either is passed, return
`{error, "not yet implemented (S2)"}` (a clear stub), so the surface exists but
the behavior lands in S2.

`do/1` (in-place):
1. `{Opts, _} = rebar_state:command_parsed_args(State)`.
2. Reject unimplemented flags for now (stub `dry_run`/`check` per above);
   error if both `--dry-run` and `--check` given (mutually exclusive).
3. Build the file list:
   - `--path` given (`proplists:get_value(path, Opts)`): if it's a regular file,
     that file; if a directory, all `.lfe` under it recursively
     (`r3lfe_package:discover_files/1`); if it doesn't exist, `{error, …}`.
   - else: for each app (`current_app` or `project_apps`),
     `r3lfe_config:get_src_dirs/1` → `r3lfe_package:discover_files/1`, collect all
     `.lfe` files.
4. For each file: read as binary; `r3lfe_formatter:format/1`:
   - `{ok, IO}`: `Out = unicode:characters_to_binary(IO)`. If `Out =/= Original`,
     write it back and `?INFO("Formatted ~s", [File])`; else leave untouched.
   - `{error, Reason}`: `?ERROR` it with the file name; skip; mark the run failed.
5. Return `{ok, State}` if all files succeeded; `{error, …}` (non-zero) if any
   file failed to format. Report a one-line summary (n formatted, n unchanged,
   n failed).

`format_error/1`: `io_lib:format("~p", [Reason])` (as in the other providers).

Register `r3lfe_prv_format` in the `Providers` list in `src/rebar3_lfe.erl`.

## 2. Tests — `test/r3lfe_prv_format_SUITE.erl` (Common Test)

Model on existing `r3lfe_prv_*_SUITE`. Cover:
- in-place format of a temp app: write a deliberately-unformatted `.lfe` file,
  run `do/1`, assert the file on disk is now the formatter's output and compiles.
- unchanged file (already formatted) is **not** rewritten (assert mtime or
  content identity / a "0 changed" outcome).
- `--path` to a single file → only that file touched.
- `--path` to a directory → all `.lfe` under it (recursive); files elsewhere
  untouched.
- default (no `--path`) → resolves configured `src_dirs` (use a temp app with a
  couple of `.lfe` files under `src/`).
- a file with a syntax error → reported, skipped, run returns `{error, …}`, other
  files still formatted.
- `--path` to a nonexistent path → `{error, …}`.
- stub: `--dry-run` and `--check` each return the not-yet-implemented error;
  passing both returns the mutually-exclusive error.

## 3. Constraints

`-spec` exported funcs; `xref` + `dialyzer` clean; compile under
`warnings_as_errors`. Never write outside the resolved file set; write only when
content changed. No new deps. Don't modify the formatter/lexer/CST.

## 4. Ledger

```
Arc A5·S1 — provider + in-place
[ ] r3lfe_prv_format created (provider behaviour; init/1, do/1, format_error/1)
[ ] all three opts declared; dry_run/check stubbed; both-given → mutually-excl error
[ ] file resolution: --path file, --path dir (recursive), default src_dirs;
    nonexistent path → error
[ ] in-place: format, write-only-if-changed, per-file ?INFO; summary line
[ ] parse-error file → ?ERROR + skip + run returns {error,…}; others still done
[ ] registered in rebar3_lfe.erl
[ ] r3lfe_prv_format_SUITE: in-place, unchanged-not-rewritten, --path file/dir,
    default dirs, syntax-error, nonexistent path, flag stubs — all green
[ ] xref + dialyzer clean; warnings_as_errors clean; no regressions (full suite)
[ ] files changed + one-line rationale; deviations named — or "none"
```

Stop here. Do not implement `--dry-run`/`--check` behavior (S2).
