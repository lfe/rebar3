# 022 · Arc A6 — hardening + release (shared reference)

> ⚠️ **DO NOT hand CC this whole file.** A6 is the final arc, sub-sliced:
> - `cc-prompt-s1-hardening.md` — property/fuzz tests, edge-case battery,
>   robustness; fix anything they surface.
> - `cc-prompt-s2-docs-release.md` — `docs/commands.md` entry, CHANGELOG,
>   version confirm, gallery completeness, release-readiness checklist.
>
> This file is the shared reference. **Prereq: A1–A5 closed** (they are; the
> formatter engine and the `format` provider are complete and on `0.5.5`).

## Goal

Take `rebar3 lfe format` from "works" to "ship-ready 0.5.5": prove robustness
under property/fuzz testing and edge cases, finish the docs, and prepare (not
perform) the release.

## Standing facts (don't relitigate)

- Engine (A1–A4) and provider (A5) are closed. The formatter is idempotent,
  comment-preserving, token-preserving, AST-equivalent, and CL-aligned on the
  shared subset (yasi-verified).
- `src/rebar3_lfe.app.src` `vsn` is already `"0.5.5"` — **confirm**, don't bump
  again.
- Provider modules carry the standard rebar3-plugin dialyzer noise (unknown
  `rebar_api`/`rebar_state`/`providers` functions) — this is the **standing state
  of the whole provider suite**, not a regression. Do **not** try to "fix" it by
  changing project-wide PLT config in A6 (that's a separate decision).
- PropEr is available (`{proper, "1.5.0"}` + `rebar3_proper`); `test/r3lfe_properties.erl`
  exists — add properties there.

## Release safety (per project CLAUDE.md)

A6 **prepares** the release; it does **not** publish. Do not run `make publish`,
`deno publish`, `cargo publish`, `git push`, or tag. Do not auto-pass
`--allow-dirty`/`--no-verify`/`--force` to anything. The deliverable is a
**release-readiness checklist** for Duncan to execute.

## Definition of done for A6

Property + fuzz + edge tests green and committed; `docs/commands.md` has a full
`format` entry; CHANGELOG has a `0.5.5` entry; the gallery is complete and matches
current output; full `rebar3 ct`/`eunit`/`xref` clean and `dialyzer` at its
standing provider-suite level; a release checklist exists. Then **0.5.5 is ready
to cut.**
