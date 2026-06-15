# 022 · Arc A5 — the `format` provider / CLI (shared reference)

> ⚠️ **DO NOT hand CC this whole file.** A5 is sub-sliced. Hand one at a time:
> - `cc-prompt-s1-inplace.md` — provider skeleton + file resolution + **in-place**
>   mode (the default) + registration + CT.
> - `cc-prompt-s2-dryrun-check.md` — **`--dry-run`** (stdout) and **`--check`**
>   (CI) modes + CT. *(After S1.)*
>
> This file is the shared reference: the CLI contract, the plumbing the plugin
> already provides, and the safety rules.

## Goal

Wire the (complete, closed) formatter engine `r3lfe_formatter:format/1` to a
rebar3 subcommand so users get what the README "📐 Consistent Formatting" section
promises. The engine is done (A1–A4); A5 is **only** orchestration — no formatting
logic lives here.

## CLI contract (final, adjudicated with Duncan)

```
rebar3 lfe format                      # in-place, all .lfe in configured src dirs
rebar3 lfe format --path src/sub       # in-place, that directory (recursive)
rebar3 lfe format --path src/foo.lfe   # in-place, that file
rebar3 lfe format --dry-run [...]      # no writes; formatted output to stdout
rebar3 lfe format --check  [...]       # no writes; exit non-zero if any unformatted
```

Three modes: **in-place** (default), **`--dry-run`** (stdout), **`--check`** (CI).
`--path`/`-p` (a file or a directory) composes with all three. `--dry-run`/`-n`
and `--check`/`-c` are **mutually exclusive** — if both are given, **error**
("--dry-run and --check are mutually exclusive"). Without `--path`, operate on the
configured source dirs (default `src/`).

## Plumbing the plugin already provides (use these — don't reinvent)

From the earlier survey of the repo:

- **Provider pattern** — mirror `src/r3lfe_prv_clean.erl` exactly:
  `-behaviour(provider).`, export `init/1`, `do/1`, `format_error/1`,
  `-include_lib("rebar3_lfe/include/r3lfe.hrl").`
- `r3lfe.hrl` gives `?NAMESPACE` (= `lfe`), `?LFE_SRC_EXTENSION` (= `".lfe"`), and
  logging macros `?DEBUG/2 ?INFO/2 ?WARN/2 ?ERROR/2` (wrap `rebar_api:*`).
- **`init/1`** builds the provider with `providers:create/1`:
  `{namespace, ?NAMESPACE}, {name, format}, {module, ?MODULE}, {bare, true},
  {deps, []}, {example, "rebar3 lfe format"}, {opts, Opts}, {short_desc, …},
  {desc, …}`, then `rebar_state:add_provider(State, Provider)`.
  `Opts` tuple shape: `{Key, ShortChar, "long", Type, "help text"}`.
- **`do/1`** reads flags via `{Opts, _} = rebar_state:command_parsed_args(State)`,
  then `proplists:get_value(check, Opts, false)` etc.
- **App selection:** `case rebar_state:current_app(State) of undefined ->
  rebar_state:project_apps(State); App -> [App] end` (as in `r3lfe_prv_clean`).
- **Source dirs:** `r3lfe_config:get_src_dirs(AppInfo)` → list of dirs (default
  `["src"]` + configured `src_dirs`).
- **File discovery:** `r3lfe_package:discover_files(SrcDir)` → recursive `.lfe`
  files.
- **Registration:** add `r3lfe_prv_format` to the `Providers` list in
  `src/rebar3_lfe.erl`.

## The `opts` list

```erlang
Opts = [
    {dry_run, $n, "dry-run", boolean,
     "Do not write; print the formatted result to stdout"},
    {check,   $c, "check",   boolean,
     "Do not write; exit non-zero if any file is not already formatted"},
    {path,    $p, "path",    string,
     "Format only this file or directory, ignoring configured source dirs"}
].
```

## Safety & error handling (per project CLAUDE.md)

- The formatter rewrites files the user can recover with `git checkout`; it must
  **never** touch anything outside the resolved file set, and **neither
  `--dry-run` nor `--check` may ever write**.
- Read each file as a binary; compare the formatter output to the original and
  **write only if changed** (don't churn mtimes).
- A file that fails to parse (`r3lfe_formatter:format/1` → `{error, _}`) is
  **reported per file** (`?ERROR`) and **skipped**; the run continues over the
  remaining files; the provider returns `{error, …}` (non-zero) at the end if any
  file failed. A broken file must never abort the whole run silently.
- No new dependencies. No safety-bypass flags passed to any underlying tool.

## Tests

CT suite `test/r3lfe_prv_format_SUITE.erl`, modeled on the other
`r3lfe_prv_*_SUITE` suites (set up a temp app dir with `.lfe` files, invoke the
provider's `do/1` with a constructed `rebar_state`, assert results). The
`_integration/myproj` project is the realistic target.
