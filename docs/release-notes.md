# Release History

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.5.8] - 2026-08-08

**Upgrade urgency:** LOW — dependency and release-readiness refresh for the
current 0.5.x line. No command syntax changes; recommended for new projects and
CI template smoke checks.

### Changed

- **LFE dependency now tracks the latest compatible 2.x release.** The plugin
  config, generated project templates, docs, and test fixtures now use
  `{lfe, "~> 2.0"}` so consumers resolve the newest LFE 2.x release instead of
  pinning to `2.2.0`.
- **Quickstart examples now use the 0.5.x-compatible plugin requirement.**
  Current-facing install snippets use `{rebar3_lfe, "~> 0.5"}` to avoid stale
  patch-version pins.

### Fixed

- **Generated template smoke checks use normal project compilation.** Template
  checks now compile generated projects through `rebar3 compile`, allowing
  dependencies to compile before the LFE compile hook runs.
- **Provider compile dependencies now use the default compile provider.**
  Runtime providers depend on `{default, compile}` so dependency compilation
  happens before provider-specific commands run.
- **LFE compiler loading is more robust inside compiler worker processes.**
  The compiler path setup now survives rebar3 compiler worker process boundaries
  and explicitly loads `lfe_comp` from candidate ebin directories when needed.

---

## [0.5.7] - 2026-07-14

**Upgrade urgency:** MEDIUM — fixes a long-standing bug where `include-lib`
self-references failed to resolve before the app was staged in `_build`. No API
or command changes; a drop-in bugfix release.

### Fixed

- **`include-lib` self-references now resolve before `_build` staging.**
  A form such as `(include-lib "myapp/include/records.hrl")` — where an app
  includes one of its *own* headers by application name — previously failed on a
  clean build. The LFE compiler was being handed an empty include path
  (`rebar_compiler` does not thread the `context/1` map through to `compile/4`,
  so the `include_dirs` lookup in `Opts` always came back empty), leaving
  resolution to fall through to `code:lib_dir/1`. That fallback only succeeds
  once rebar3 has created the app-dir symlinks under `_build`, so the very first
  compile of a fresh checkout — or any app not yet staged — could not find its
  own includes.

  The include path is now resolved from `AppInfo` directly, and the **parent of
  the app directory** is added to the search path so self-references resolve via
  LFE's path-first search regardless of `_build` staging. This mirrors
  `epp`/rebar3 behaviour for `erlc` self-includes. When `AppInfo` is
  unavailable, the app directory is derived from the source path
  (`<appdir>/src/foo.lfe` → `<appdir>`) as a fallback.

  Fixed across three call sites: the compiler module (`r3lfe_compiler_mod`), the
  dependency scanner (`r3lfe_dependency_scanner`), and the compile provider
  (`r3lfe_prv_compile`).

---

## [0.5.6] - 2026-06-28

**Upgrade urgency:** MEDIUM — `confabulate` was renamed to `defabulate` (breaking
for any caller of the old command); all other changes are additive. See the
[migration guide](0.4-to-0.5-migration.md#confabulate--defabulate-breaking-rename-in-055)
for the one-line fix.

### New `rebar3 lfe format` command — an LFE source formatter

#### Added

- **`rebar3 lfe format`** — a clean-room LFE source formatter:
  - Reformats all `.lfe` files in the project source directories (default: `src/`)
    to LFE style conventions: 80-column width, 2-space indentation, the
    `lfe-indent.el`-derived special-form table.
  - **Comment-preserving**: all leading, trailing, and block comments are kept
    in place and never moved relative to their enclosing form.
  - **Idempotent**: formatting an already-formatted file is a no-op.
  - **Token-preserving**: no identifiers, literals, or punctuation added,
    removed, or mutated.
  - **AST-equivalent**: formatted output parses to the same Lisp structure as
    the input.
  - **Knowledge-gated layout**: known LFE special forms (`defun`, `let`, `case`,
    `cond`, `if`, `try`, `receive`, `progn`, `maybe`, maps, def-forms, etc.) are
    laid out canonically; unknown or data-oriented forms preserve the author's
    line breaks (reindented to the correct column).
  - **`export`/`import` sorted**: entries always one-per-line at `C+1` under the
    keyword, alphabetically sorted by name then arity; sort is suppressed when
    any entry carries a developer comment.
  - **`--dry-run`**: print formatted output to stdout; no files written.
  - **`--check`**: CI mode — exit non-zero if any file is not already formatted;
    no files written.
  - **`--path P`**: restrict formatting to a single `.lfe` file or a directory
    (recursive).
  - **Powered by the [`lfmt`](https://hex.pm/packages/lfmt) formatter library**
    (`{lfmt, "~> 0.4"}`): the formatter engine lives in a standalone,
    dependency-free hex package — shared across LFE tooling — and is resolved
    automatically as a plugin dependency. No new runtime dependencies are added to
    *your* application. The `r3lfe_prv_format` provider delegates to
    `lfmt:format/1`; its `{ok, binary()} | {error, term()}` contract and the
    `unicode:characters_to_binary/1` flatten are unchanged.

### Data conversion — new `confabulate`, renamed `defabulate`

#### Breaking

- **`rebar3 lfe confabulate` renamed to `rebar3 lfe defabulate`** — the
  command that converts LFE data files to Erlang format (`*.lfe` → `*.erl`) is
  now called `defabulate`. The behaviour is identical; only the invocation name
  changes. LFE keeps the fabulous name for its own command (see below).

  ```bash
  # ❌ 0.5.5 and earlier
  rebar3 lfe confabulate --input data.lfe

  # ✅ 0.5.6+
  rebar3 lfe defabulate --input data.lfe
  ```

#### Added

- **`rebar3 lfe confabulate`** — new command that converts Erlang data files to
  LFE syntax (`*.erl` → `*.lfe`). Reads Erlang terms (`.`-terminated, parsed via
  `file:consult/1`) and writes one LFE expression per line via `lfe_io:print1/1`.
  Accepts `--input`, `--output`, and `--force` options, mirroring `defabulate`.

  ```bash
  # Erlang → LFE
  rebar3 lfe confabulate --input data.erl

  # Full roundtrip
  rebar3 lfe confabulate --input data.erl --output data.lfe
  rebar3 lfe defabulate  --input data.lfe --output roundtrip.erl
  ```

  > **Note on string rendering**: `lfe_io:print1/1` renders Erlang char-list
  > strings (e.g. `"alice"`) as LFE integer-list syntax (`(97 108 105 99 101)`).
  > This is correct — char lists *are* integer lists in LFE/Erlang — and the
  > roundtrip is lossless.

### OTP compatibility

#### Changed

- **Supported OTP range is now 25–29** (was 24–28).
  - **Dropped OTP 24**: EOL since May 2023; the `-dialyzer({no_extra_return, …})`
    attribute used in `src/r3lfe_prv_run.erl` requires OTP 25+.
  - **Added OTP 29**: matrix now covers OTP 25, 26, 27, 28, and 29.
  - rebar3 pins: 3.27.0 for OTP 26–29; 3.24.0 for OTP 25.

### Fixed

- **`r3lfe_prv_clean` / provider `?DEPS`**: changed `[{default, compile}]` to
  `[{default, app_discovery}]` so bare `rebar3 lfe format` and `rebar3 lfe clean`
  run correctly without requiring a prior compile step. Discovered via real-CLI
  e2e testing.

- **Dialyzer**: fixed spurious Dialyzer warnings produced during rebar3 plugin
  builds by adjusting the `-dialyzer` attribute scope in `src/r3lfe_prv_run.erl`.

### Infrastructure

- **CI**: OTP 29 added to the test matrix; rebar3 bumped to 3.27.0 for OTP 26+;
  `codecov/codecov-action` updated to `@v5`.
- **CI**: `rebar3 lfe format --check` e2e test (`test/e2e/format_e2e.sh`) wired
  into the `template-checks` job so formatter regressions are caught in CI.

---

## [0.5.5] - 2025-11-13

### Housekeeping release

**Upgrade urgency:** LOW — internal header relocation only; no API or behaviour
changes.

#### Changed

- **Internal header location**: `include/r3lfe.hrl` moved to `src/r3lfe.hrl`.
  All 26 source modules updated their `-include` path accordingly. The header
  is not part of the public API, so no downstream impact is expected; if you
  happened to reference it directly, update the path.

- **README**: Updated badge versions to reflect the current OTP and LFE ranges.

---

## [0.5.4] - 2025-10-31

### Maintenance Release - Templates and Polish

This release fixes template packaging issues and removes debug output that was inadvertently left in 0.5.3.

**Upgrade urgency**: MEDIUM - Users of 0.5.3 should upgrade to remove debug output and get complete template support

#### Fixed

- **CRITICAL**: Added `priv` directory to hex package files list
  - Templates were missing from hex.pm releases in 0.5.2 and 0.5.3
  - Users installing from hex couldn't use `rebar3 new` commands
  - Fixed in `src/rebar3_lfe.app.src` by adding `"priv"` to files list
  - See commit `014d7f1` for details

- **Output**: Removed debug statements polluting stdout
  - All `io:format` debug statements left from 0.5.3 troubleshooting have been removed
  - These were causing unwanted output during normal rebar3 operations
  - Clean output restored for compilation and other commands
  - Fixed in `src/r3lfe_config.erl`
  - See commit `89649cc` for details

- **Templates**: Fixed gen_statem template issues (contributed by Dmitry Matveyev)
  - Fixed formatting and syntax issues in `statem-handle-event.lfe.tpl`
  - Fixed formatting and syntax issues in `statem-state-functions.lfe.tpl`
  - Templates now generate correct LFE code for state machine implementations
  - See commit `fabecdc` for details

#### Changed

- **Templates**: Improved project template quality (contributed by Joel Jucá)
  - Added `.env` and `.envrc` to `.gitignore` for better environment management
  - Fixed heading levels in README template (changed from # to ##)
  - Added instructions for using `make repl` with customized colored prompt
  - See commit `aeb5887` for details

- **Templates**: Updated rebar.config templates for better compatibility
  - Changed hex plugin package references from `rebar3_lfe` to `r3lfe`
  - Updated all rebar.config template variants (app, lib, escript, main, release)
  - Updated CI/CD template for consistency
  - Fixed app.src templates to use correct package name
  - See commit `dd4e6dc` for details

- **Documentation**: Added warnings about using bleeding-edge branch
  - README now clearly indicates when users are on the development branch
  - Helps prevent confusion about stability and features
  - See commit `9d9b3ba` for details

#### Contributors

Special thanks to:
- **Dmitry Matveyev** ([@greenfork](https://github.com/greenfork)) - Fixed gen_statem templates
- **Joel Jucá** ([@joeljuca](https://github.com/joeljuca)) - Improved project templates

---

## [0.5.3] - 2025-10-25

### Critical Bug Fix Release

This release addresses several critical compiler bugs discovered in 0.5.2 that could cause compilation failures for certain dependency configurations.

**Upgrade urgency**: HIGH - Users experiencing compilation failures with dependencies should upgrade immediately

#### Fixed

- **CRITICAL**: Fixed dependency erl_opts not being read from rebar.config
  - Dependencies' `erl_opts` (such as `no_auto_import`) were not being applied when compiling dependency LFE files
  - This caused compilation failures when dependencies defined functions that conflicted with Erlang BIFs
  - The fix reads each app's own `rebar.config` directly to get its specific `erl_opts` and `lfe_opts`
  - Particularly fixes compilation of libraries like yuri that define `get/1` function and require `{no_auto_import, [{get,1}]}`
  - See commit `4c29467` for details

- **CRITICAL**: Fixed compile/4 not using merged compiler options
  - The compiler's `compile/4` function was not properly merging compiler options from config
  - Options like `debug_info` and custom `erl_opts` were being ignored during compilation
  - Fixed in `src/r3lfe_compiler_mod.erl` to properly retrieve and merge options from AppInfo
  - See commit `a51cc23` for details

- **CRITICAL**: Fixed dependency include-lib resolution failing for unloaded apps
  - Dependency header file resolution via `include-lib` was failing for applications not yet loaded in the VM
  - The scanner now uses `code:lib_dir/1` which handles both loaded and unloaded applications
  - Also improved error messages when dependencies cannot be found
  - Fixed in `src/r3lfe_dependency_scanner.erl`
  - See commit `9d74a63` for details

- **Compiler**: Fixed crash on nested LFE compiler error format
  - LFE compiler occasionally returns nested error format: `{error, [{error, FileErrors, []}], [], []}`
  - The error handler now properly extracts and formats errors from this nested structure
  - Prevents cryptic crashes and provides clear error messages to users
  - Fixed in `src/r3lfe_compile_worker.erl`
  - See commit `6349f1b` for details

#### Changed

- **Build**: Set `debug_info` as default compiler option
  - All compiled modules now include debug information by default
  - Enables better debugging, analysis, and hot code reloading
  - Can still be disabled via project-specific `erl_opts` if needed
  - Updated in `include/r3lfe.hrl`

#### Internal

- Added comprehensive debug logging for troubleshooting compilation issues
  - Debug output shows configuration reading, option merging, and compilation steps
  - Helps diagnose issues with dependency compilation
  - Can be enabled via `DEBUG=1` or `DIAGNOSTIC=1` environment variables

---

## [0.5.2] - 2025-10-24

### Major Feature Release with Critical Bug Fix

This release adds automatic rlwrap integration for enhanced REPL experience, significantly improves the versions command, and fixes a critical data loss bug from 0.5.1.

**Upgrade urgency**: CRITICAL - All users of 0.5.1 should upgrade immediately to avoid potential data loss from the clean bug

#### Added

- **🎉 Automatic rlwrap Integration**: Enhanced REPL with line editing, history, and tab completion
  - Auto-detects and wraps REPL with rlwrap when available (no configuration needed)
  - Persistent command history across sessions stored in `~/.lfe/history`
  - Tab completion for Erlang modules and LFE special forms
  - Emacs-style line editing keybindings (Ctrl-A, Ctrl-E, Ctrl-K, etc.)
  - Configurable via `rebar.config` (history file, colors, completion files)
  - New `--no-rlwrap` flag to disable integration
  - New modules: `r3lfe_rlwrap.erl`, `r3lfe_completion.erl`
  - Graceful degradation with informative warning when rlwrap not available
  - Cross-platform support (Unix/Linux/macOS)
  - See `docs/design/021-rlwrap-integration.md` for details

- **Enhanced `rebar3 lfe versions` Command**: Complete project version information
  - New Dependencies section showing all project dependencies with versions
  - New Plugins section showing all plugins with versions
  - Profile information for non-default profile deps/plugins
  - Alphabetically sorted output for easier scanning
  - New utility module `r3lfe_util.erl` with helper functions
  - Smart plugin version lookup across multiple profile directories
  - Uniform heading formatting with centered text
  - Auto-hide sections when empty or redundant
  - See `docs/design/020-versions-cmd-update.md` for details

- **Documentation**: New screenshot showing enhanced REPL with rlwrap

#### Fixed

- **CRITICAL**: Fixed `clean/2` function that was deleting source files instead of compiled beam files
  - The compiler module's clean function was receiving source `.lfe` files from rebar3 but deleting them directly
  - According to the rebar3 compiler contract, clean should convert source files to their corresponding `.beam` targets before deletion
  - Running `rebar3 clean` would have resulted in **catastrophic data loss** by deleting user source code
  - Fixed in `src/r3lfe_compiler_mod.erl` to properly convert `.lfe` sources to `.beam` files in the output directory
  - Updated test suite to verify correct behavior (delete beam files, preserve source files)
  - See commit `48f68ae` for full details

- **Compiler**: Fixed "Bad directory" warnings for uncompiled dependencies
  - The compiler was trying to add dependency ebin directories before they were compiled
  - Now checks if ebin directory exists before adding to code path
  - Eliminates confusing warnings during normal compilation

- **REPL**: Fixed multiple rlwrap integration issues through iterative debugging
  - Fixed infinite trampolining loop by adding `--rlwrap-active` internal flag
  - Fixed argument filtering to exclude Erlang VM args
  - Fixed shell quoting issues by using `spawn_executable` instead of shell commands
  - Fixed TTY access for interactive terminal control
  - Fixed character-mode support with `--always-readline` flag
  - Fixed port handling to properly wait for REPL termination

- **Versions Command**: Fixed version detection and display issues
  - LFE version now loads application before querying
  - Plugin versions read from `.app` files instead of trying to load uncompiled apps
  - Dependencies now show actual versions from rebar3's resolved state
  - Plugin name corrected from `r3lfe` to `rebar3_lfe` in Build Tools
  - Plugin version messages changed from 'unknown' to 'not compiled' when appropriate

#### Changed

- **Templates**: Updated all project templates for better publishing workflow
  - Changed `plugins` to `project_plugins` for consistency with rebar3 best practices
  - Added `rebar3_hex` to default `project_plugins`
  - Added `publish` alias to all template `rebar.config` files
  - Updated `Makefile.tpl` to use simpler publish command

- **Documentation**: Reorganized design docs with numbered indices (001-021)
  - Easier navigation and reference
  - Clear chronological ordering of design decisions

- **Configuration**: Updated coverage requirements and options
  - Reduced minimum code coverage from 90% to 80% for more realistic targets
  - Simplified coverage configuration in `rebar.config`
  - Updated CI workflow to use coverage alias

- **Build**: Fixed Makefile test targets and removed obsolete tasks

#### Testing

- **New Test Suites**:
  - `r3lfe_rlwrap_SUITE.erl`: 13 comprehensive tests for rlwrap integration
  - `r3lfe_util_SUITE.erl`: 14 tests for utility functions (9 deps/plugins + 5 version lookup)

- **Enhanced Test Coverage**:
  - `r3lfe_prv_versions_SUITE.erl`: Added 12 new tests (total: 25 tests)
    - 4 heading formatter tests
    - 5 plugin version lookup tests
    - 3 new version extraction tests
  - `r3lfe_compiler_mod_SUITE.erl`: Updated clean test to verify correct behavior

- **Total**: All 434 tests passing with comprehensive coverage

---

## [0.5.1] - 2025-10-22

### Maintenance Release

Minor cleanup and maintenance updates following the 0.5.0 release.

#### Changed

- **README**: Simplified title to "rebar3_lfe" and removed coverage badge
- **Templates**: Changed app.src version from `git` to `"0.1.0"` for clearer versioning
  - Updated `app-app.src.tpl` template
  - Updated `app-lib.src.tpl` template
- **Makefile**: Streamlined test targets
  - Updated template test targets to use `rebar3 compile` instead of `rebar3 lfe compile` for consistency
  - Removed deprecated `test-clean-build-cmd` target

#### Fixed

- Test targets now use correct compilation commands for generated projects
- Removed obsolete clean-build command references

---

## [0.5.0] - 2025-10-22

### 🎉 Complete Rewrite

This is a ground-up rewrite of the rebar3_lfe plugin with modern architecture and best practices.

#### Added

- **Modern Compiler Integration**: Uses rebar3's Custom Compiler Modules interface (3.14+)
- **Header Dependency Tracking**: Changes to `.lfe` header files now trigger recompilation
- **Incremental Compilation**: Only recompiles files that actually changed
- **Compiler Option Tracking**: Detects when compiler options change and recompiles
- **Package System 2.0**: Redesigned nested module support with proper cleanup
- **Progress Reporting**: Clear feedback during compilation
- **Better Error Messages**: Professional, actionable error formatting
- **Dependency Caching**: Faster repeat compilations
- **Multi-OTP Support**: Tested on Erlang/OTP 24-28
- **Comprehensive Testing**: >90% code coverage with unit, integration, and property tests
- **Modern CI/CD**: GitHub Actions with multi-version testing
- **Expression Evaluation**: `rebar3 lfe eval` command for evaluating LFE expressions
  - Execute expressions without starting a REPL
  - Full LFE language support (lambdas, let, conditionals, etc.)
  - Access to compiled project modules and dependencies
  - Useful for quick calculations, testing, automation, and CI/CD scripts
- **Script Support**: `rebar3 lfe run` command for executing LFE scripts with main/1
  - Supports `--main` option for script path
  - Pass arguments after `--` separator
  - Configure default script in rebar.config
- **Escript Support**: Build and run standalone executables
  - `rebar3 lfe escriptize` - Build standalone executable escripts
  - `rebar3 lfe run-escript` - Execute built escripts
  - Bundles all dependencies into portable binaries
- **Release Management**: `rebar3 lfe run-release` for complete release lifecycle
  - Start/stop/restart release nodes
  - Console and remote shell access
  - Hot code upgrades/downgrades
  - Status checking and monitoring
- **Data Conversion**: bidirectional data conversion between LFE and Erlang
  - `rebar3 lfe defabulate` — LFE data to Erlang terms
  - `rebar3 lfe confabulate` — Erlang terms to LFE data
  - Config file conversion, test data generation, data migration tools

#### Changed

- **BREAKING**: Plugin package renamed from `rebar3_lfe` to `r3lfe`
- **BREAKING**: All modules renamed with `r3lfe_` prefix
- **BREAKING**: Internal APIs completely redesigned
- **IMPROVED**: Compilation is 10-30x faster for incremental builds
- **IMPROVED**: REPL works reliably on all OTP versions
- **IMPROVED**: Package system handles errors gracefully
- **IMPROVED**: All commands follow modern rebar3 patterns
- **IMPROVED**: Consistent error handling across all providers

#### Removed

- **BREAKING**: Removed undocumented internal functions
- **BREAKING**: Removed deprecated configuration options
- **CLEANUP**: Removed all legacy rebar3_lfe_* modules (22 files, 2,574 lines)
  - Complete removal of 0.4.x codebase after rewrite
  - Added verification test suite to ensure no legacy code remains
  - Preserved rebar3_lfe.app.src for application configuration
  - Safety scripts included for auditing and verification
- Removed workarounds for old rebar3 versions

#### Fixed

- **CRITICAL**: Header file changes now properly trigger recompilation
- **CRITICAL**: Temporary package files always cleaned up, even on errors
- **CRITICAL**: Path handling works correctly on Windows
- Race conditions in package preparation
- Memory leaks from unclosed file handles
- Atom table exhaustion in long-running builds
- Missing dependency errors now clearly reported
- REPL startup issues on OTP 26+
- Escript builds work correctly
- Release management commands execute properly

#### Security

- No longer executes shell commands (uses Erlang APIs)
- Validates all user input
- Sandboxed compilation with proper error handling

### Migration from 0.4.x

See [MIGRATION.md](../MIGRATION.md) for detailed upgrade instructions.

---

## [0.4.0] - 2020-XX-XX

### Major Updates for rebar3 Compatibility

#### Changed

- Updated to use `rebar_otp_app:compile` for proper app.src management
- Improved source directory ordering for better compilation
- API updates to maintain compatibility with newer rebar3 versions
- Better handling of escript types

#### Added

- Support for LFE 2.0+
- Updated templates for modern LFE projects

#### Fixed

- Compatibility issues with breaking changes in rebar3
- Template generation for current LFE versions

---

## [0.3.0] - 2019-XX-XX

### Package Support and Testing Infrastructure

#### Added

- **Package System**: Support for nested modules (e.g., `my.app.core`)
- **Testing Support**: Integration with ltest framework
- **Confabulate Provider**: Data conversion utilities
- Module documentation improvements

#### Changed

- Converted critical path code from LFE to Erlang for better performance
- Improved options handling across all providers
- Updated templates for better project scaffolding
- Enhanced error messages

#### Fixed

- Path handling issues across different platforms
- Code path management for dependencies
- Template generation bugs

---

## [0.2.0] - 2017-XX-XX

### Comprehensive Build Tool Features

#### Added

- **Clean Commands**: `clean`, `clean-build`, `clean-cache`, `clean-all`
- **Version Command**: Display version information
- **Release Support**: Generate and build OTP releases
- **Escript Support**: Generate, build, and run escript applications
- **App Generation**: Support for generating LFE applications
- **hex.pm Integration**: Metadata for package publishing

#### Changed

- Improved compile command output
- Enhanced CI/CD workflow

#### Documentation

- Added CHANGELOG
- Added comprehensive usage documentation

---

## [0.1.0] - 2016-XX-XX

### Initial Release

The first release of rebar3_lfe, bringing LFE support to the rebar3 build tool.

#### Added

- **Basic Compilation**: Compile LFE source files to BEAM
- **REPL Support**: Interactive LFE shell integration
- **Run Command**: Execute LFE applications
- **Project Templates**:
  - LFE library template
  - Main script template
- **CI/CD**: GitHub Actions workflow with version matrix
- Basic documentation and README

#### Infrastructure

- Initial project structure
- rebar3 provider interface
- Template system
- Basic testing setup

---

[0.5.0]: https://github.com/lfe-rebar3/rebar3_lfe/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/lfe-rebar3/rebar3_lfe/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/lfe-rebar3/rebar3_lfe/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/lfe-rebar3/rebar3_lfe/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/lfe-rebar3/rebar3_lfe/releases/tag/0.1.0
