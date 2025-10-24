# Phase 7: Documentation & Release Preparation

## Overview

This final phase completes the project with comprehensive documentation, migration guides, examples, and release preparation. We'll ensure users can easily adopt r3lfe and understand how to migrate from the old plugin.

**Goal**: A polished, documented, production-ready release that users love.

## Prerequisites

- Phase 1-6 completed with all tests passing
- Understanding of documentation best practices
- Knowledge of semantic versioning
- Familiarity with hex.pm publishing

## Documentation Structure

```
Documentation Hierarchy:

README.md                    ← Quick start, badges
├── CHANGELOG.md            ← Version history
├── MIGRATION.md            ← 0.4.x → 0.5.0 guide
├── CONTRIBUTING.md         ← Development guide
├── LICENSE                 ← Apache 2.0
│
├── docs/
│   ├── index.md           ← Documentation home
│   ├── installation.md    ← Setup guide
│   ├── quickstart.md      ← Getting started
│   ├── commands.md        ← Command reference
│   ├── configuration.md   ← Config options
│   ├── packages.md        ← Package system
│   ├── troubleshooting.md ← Common issues
│   ├── architecture.md    ← Design overview
│   └── api.md             ← API reference
│
└── examples/
    ├── simple_lib/        ← Basic library
    ├── web_app/           ← Web application
    ├── umbrella/          ← Umbrella project
    └── release/           ← OTP release
```

## Implementation Tasks

### Task 7.1: Create CHANGELOG

**File: `CHANGELOG.md`**

```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.5.0] - 2025-01-XX

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

#### Changed

- **BREAKING**: Plugin package renamed from `rebar3_lfe` to `r3lfe`
- **BREAKING**: All modules renamed with `r3lfe_` prefix
- **BREAKING**: Internal APIs completely redesigned
- **IMPROVED**: Compilation is 10-30x faster for incremental builds
- **IMPROVED**: REPL works reliably on all OTP versions
- **IMPROVED**: Package system handles errors gracefully

#### Removed

- **BREAKING**: Removed undocumented internal functions
- **BREAKING**: Removed deprecated configuration options
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

#### Security

- No longer executes shell commands (uses Erlang APIs)
- Validates all user input
- Sandboxed compilation with proper error handling

### Migration from 0.4.x

See [MIGRATION.md](MIGRATION.md) for detailed upgrade instructions.

## [0.4.12] - 2024-XX-XX

### Changed
- Maintenance release
- Bug fixes for rebar3 3.23+

## [0.4.0] - 2023-XX-XX

### Added
- Initial stable release
- Basic LFE compilation support
- REPL integration
- Test runner support

---

[0.5.0]: https://github.com/lfe-rebar3/rebar3_lfe/compare/v0.4.12...v0.5.0
[0.4.12]: https://github.com/lfe-rebar3/rebar3_lfe/compare/v0.4.0...v0.4.12
[0.4.0]: https://github.com/lfe-rebar3/rebar3_lfe/releases/tag/v0.4.0
```

### Task 7.2: Create Migration Guide

**File: `MIGRATION.md`**

```markdown
# Migration Guide: v0.4.x → v0.5.0

This guide helps you migrate from rebar3_lfe 0.4.x to r3lfe 0.5.0.

## ⚠️ Breaking Changes

### Plugin Name Change

**Before (0.4.x):**
```erlang
{plugins, [
    {rebar3_lfe, "0.4.12"}
]}.
```

**After (0.5.0):**
```erlang
{plugins, [
    {r3lfe, "0.5.0"}
]}.
```

### Global Installation

If you installed the plugin globally:

```bash
# Remove old version
rm -rf ~/.cache/rebar3/plugins/*rebar3_lfe*
rm -rf ~/.config/rebar3/plugins/rebar3_lfe

# Update rebar.config
vim ~/.config/rebar3/rebar.config
# Change: {plugins, [rebar3_lfe]}.
# To:     {plugins, [r3lfe]}.

# Clear cache and reinstall
rm -rf ~/.cache/rebar3/plugins/*
rebar3 lfe version  # Downloads new plugin
```

## Command Changes

Commands remain the same, just the plugin name changed:

```bash
# All these still work:
rebar3 lfe compile
rebar3 lfe clean
rebar3 lfe repl
rebar3 lfe ltest
```

## Configuration Changes

### Basic Configuration

Most configurations remain compatible:

```erlang
%% Still works in 0.5.0
{lfe_opts, [
    debug_info,
    verbose
]}.

{lfe_first_files, [
    "src/records.lfe"
]}.
```

### Removed Options

The following undocumented options were removed:

```erlang
%% ❌ No longer supported
{lfe_opts, [
    {special_internal_option, true}  % Was never documented
]}.
```

If you used any undocumented options, please file an issue.

## Behavioral Changes

### Header Dependency Tracking

**0.4.x behavior:**
- Header file changes were ignored
- Required manual `rebar3 clean` before rebuild

**0.5.0 behavior:**
- Header changes automatically trigger recompilation
- No manual clean needed

### Package System

**0.4.x behavior:**
- Temporary files sometimes left behind
- Race conditions in concurrent builds

**0.5.0 behavior:**
- Temporary files always cleaned up
- Safe for parallel compilation
- Better error messages

### Compilation Speed

**0.5.0 is much faster:**
```
0.4.x: 10 seconds (always full rebuild)
0.5.0: 10 seconds (first build)
       0.3 seconds (incremental, no changes)
       2 seconds (incremental, 1 file changed)
```

## Step-by-Step Migration

### 1. Update rebar.config

```bash
cd your-project

# Backup
cp rebar.config rebar.config.bak

# Edit rebar.config
vim rebar.config
```

Change:
```erlang
{plugins, [
    {rebar3_lfe, "0.4.12"}
]}.
```

To:
```erlang
{plugins, [
    {r3lfe, "0.5.0"}
]}.
```

### 2. Clean Build Artifacts

```bash
# Remove old build artifacts
rm -rf _build
rm -rf rebar.lock

# Remove old global plugin
rm -rf ~/.cache/rebar3/plugins/*rebar3_lfe*
```

### 3. Test Compilation

```bash
# Download new plugin and compile
rebar3 lfe compile

# Verify beam files created
ls -la ebin/*.beam

# Test incremental compilation
touch src/some_file.lfe
rebar3 lfe compile
# Should be fast!
```

### 4. Update CI/CD

**GitHub Actions:**

```yaml
# Before
- name: Compile
  run: rebar3 compile

# After (same, but faster!)
- name: Compile
  run: rebar3 lfe compile
```

### 5. Update Documentation

Update any project documentation that mentions `rebar3_lfe` to `r3lfe`.

## Troubleshooting

### "Plugin not found" error

```
Error: Plugin r3lfe not found
```

**Solution:** Clear cache and retry:
```bash
rm -rf ~/.cache/rebar3/plugins/*
rebar3 lfe compile
```

### Compilation slower than expected

**Check:** Do you have debug logging enabled?

```bash
# Disable debug logging
unset DEBUG
rebar3 lfe compile
```

### REPL won't start

**Solution:** Ensure LFE 2.2+ is in deps:

```erlang
{deps, [
    {lfe, "2.2.0"}  % Minimum version
]}.
```

### "Module not found" in REPL

**Solution:** Use `rebar3 lfe repl` instead of just `lfe`:

```bash
# ❌ Wrong (old way)
lfe -pa ebin -pa deps/*/ebin

# ✅ Correct (new way)
rebar3 lfe repl
```

## Benefits of Upgrading

### ✅ Faster Builds

Incremental compilation is 10-30x faster.

### ✅ Correct Builds

Header changes now properly detected.

### ✅ Better Errors

Clear, actionable error messages.

### ✅ Rock Solid

>90% test coverage, tested on OTP 24-28.

### ✅ Active Development

Modern codebase, easier to maintain and extend.

## Getting Help

- **Issues:** [GitHub Issues](https://github.com/lfe-rebar3/rebar3_lfe/issues)
- **Discussions:** [GitHub Discussions](https://github.com/lfe-rebar3/rebar3_lfe/discussions)
- **Chat:** [LFE Slack](https://lfe-slack.herokuapp.com/)

## Rollback (if needed)

If you need to rollback:

```bash
# In rebar.config
{plugins, [
    {rebar3_lfe, "0.4.12"}
]}.

# Clear cache
rm -rf _build
rm -rf ~/.cache/rebar3/plugins/*

# Rebuild
rebar3 compile
```

We'd love to know why you rolled back - please file an issue!
```

### Task 7.3: Create User Guide

**File: `docs/quickstart.md`**

```markdown
# Quick Start Guide

Get up and running with r3lfe in 5 minutes.

## Installation

### New Project

```bash
# Create project directory
mkdir my-lfe-app
cd my-lfe-app

# Create rebar.config
cat > rebar.config <<EOF
{plugins, [
    {r3lfe, "0.5.0"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.
EOF

# Create source directory
mkdir src

# Create a simple module
cat > src/myapp.lfe <<EOF
(defmodule myapp)

(defun hello (name)
  (io:format "Hello, ~s!~n" (list name)))
EOF

# Compile
rebar3 lfe compile

# Start REPL
rebar3 lfe repl
```

In the REPL:
```lfe
> (myapp:hello "World")
Hello, World!
ok
```

### Existing Project

Add to your `rebar.config`:

```erlang
{plugins, [
    {r3lfe, "0.5.0"}
]}.
```

Then:
```bash
rebar3 lfe compile
```

## Project Structure

### Simple Library

```
my-lib/
├── rebar.config
├── src/
│   ├── mylib.lfe
│   └── mylib-utils.lfe
└── test/
    └── mylib-tests.lfe
```

### With Packages (Nested Modules)

```
my-app/
├── rebar.config
├── src/
│   ├── myapp.lfe           → myapp
│   └── myapp/
│       ├── core.lfe        → myapp.core
│       └── utils/
│           └── helpers.lfe → myapp.utils.helpers
└── include/
    └── records.lfe
```

### Umbrella Project

```
my-umbrella/
├── rebar.config
└── apps/
    ├── web/
    │   ├── rebar.config
    │   └── src/
    │       └── web.lfe
    └── db/
        ├── rebar.config
        └── src/
            └── db.lfe
```

## Common Tasks

### Compiling

```bash
# Compile all files
rebar3 lfe compile

# Compile specific profile
rebar3 as prod lfe compile

# Verbose output
rebar3 lfe compile --verbose
```

### Cleaning

```bash
# Remove beam files
rebar3 lfe clean

# Remove everything (including deps)
rebar3 clean
```

### REPL

```bash
# Start REPL with project loaded
rebar3 lfe repl

# Start with specific apps
rebar3 lfe repl --apps myapp,mylib

# Run script before REPL
rebar3 lfe repl --script init.lfe
```

### Testing

```bash
# Run tests (requires ltest dependency)
rebar3 lfe ltest

# Run specific suite
rebar3 lfe ltest --suite my-suite

# Verbose output
rebar3 lfe ltest --verbose
```

### Releases

```bash
# Build release
rebar3 lfe release

# Build tar
rebar3 as prod lfe release tar
```

## Configuration

### Compiler Options

```erlang
{lfe_opts, [
    debug_info,      % Include debug info
    verbose,         % Verbose compilation
    {i, "include"},  % Additional include dir
    {outdir, "ebin"} % Output directory
]}.
```

### First Files

Compile certain files before others:

```erlang
{lfe_first_files, [
    "src/records.lfe",
    "src/macros.lfe"
]}.
```

### Source Directories

```erlang
{src_dirs, [
    "src",
    "extra_src"
]}.
```

### Include Directories

```erlang
{lfe_include_dirs, [
    "include",
    "extra_include"
]}.
```

## Next Steps

- Read the [Command Reference](commands.md)
- Learn about [Packages](packages.md)
- Check out [Examples](../examples/)
- See [Configuration](configuration.md) for advanced options
```

### Task 7.4: Create Command Reference

**File: `docs/commands.md`**

```markdown
# Command Reference

Complete reference for all r3lfe commands.

## compile

Compile LFE source files.

```bash
rebar3 lfe compile [OPTIONS]
```

**Features:**
- Incremental compilation (only changed files)
- Header dependency tracking
- Package support (nested modules)
- Parallel compilation
- Progress reporting

**Options:**
- `--verbose` - Detailed output

**Examples:**
```bash
# Basic compilation
rebar3 lfe compile

# Verbose output
rebar3 lfe compile --verbose

# Specific profile
rebar3 as prod lfe compile
```

## clean

Remove compiled files.

```bash
rebar3 lfe clean
```

**What it removes:**
- All `.beam` files in `ebin/`
- Temporary package files
- Cached dependency information

**Examples:**
```bash
# Clean current project
rebar3 lfe clean

# Clean all (including deps)
rebar3 clean
```

## repl

Start an LFE REPL.

```bash
rebar3 lfe repl [OPTIONS]
```

**Features:**
- Project and deps on code path
- Optional app startup
- Script execution
- Distributed node support

**Options:**
- `--name NAME` - Long node name
- `--sname NAME` - Short node name
- `--setcookie COOKIE` - Node cookie
- `--apps APPS` - Comma-separated apps to start
- `--script PATH` - Script to run before REPL

**Examples:**
```bash
# Basic REPL
rebar3 lfe repl

# With node name
rebar3 lfe repl --sname mynode

# Start specific apps
rebar3 lfe repl --apps myapp,mylib

# Run initialization script
rebar3 lfe repl --script init.lfe
```

**In REPL:**
```lfe
> (help)                    ; Show help
> (myapp:start)             ; Call your code
> (c "src/myapp.lfe")       ; Recompile file
> (q)                       ; Quit
```

## ltest

Run tests using ltest framework.

```bash
rebar3 lfe ltest [OPTIONS]
```

**Requirements:**
- `{ltest, "~> 0.13"}` in deps

**Options:**
- `--suite NAME` - Specific test suite
- `--test NAME` - Specific test
- `--verbose` - Detailed output

**Examples:**
```bash
# Run all tests
rebar3 lfe ltest

# Run specific suite
rebar3 lfe ltest --suite integration

# Verbose output
rebar3 lfe ltest --verbose
```

## release

Build an OTP release.

```bash
rebar3 lfe release [OPTIONS]
```

**Requirements:**
- relx configuration in `rebar.config`

**Examples:**
```bash
# Build release
rebar3 lfe release

# Production release
rebar3 as prod lfe release

# Create tar
rebar3 as prod lfe release tar
```

**Configuration:**
```erlang
{relx, [
    {release, {myapp, "0.1.0"}, [
        myapp,
        sasl
    ]},
    {dev_mode, false},
    {include_erts, true}
]}.
```

## versions

Display version information.

```bash
rebar3 lfe versions
```

**Shows:**
- Project applications and versions
- LFE version
- Erlang/OTP version
- rebar3 version
- r3lfe version

**Example output:**
```
=== Project Applications ===
  myapp                0.1.0

=== Languages ===
  lfe                  2.2.0
  erlang               27
  erts                 14.2.1

=== Build Tools ===
  rebar3               3.25.0
  r3lfe               0.5.0
```

## Global Installation

Install r3lfe globally for use across all projects:

```bash
# Add to ~/.config/rebar3/rebar.config
{plugins, [r3lfe]}.

# Use in any project
cd any-lfe-project
rebar3 lfe compile
```
```

### Task 7.5: Create Examples

**File: `examples/simple-lib/rebar.config`**

```erlang
{plugins, [
    {r3lfe, "0.5.0"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.

{lfe_first_files, []}.

{profiles, [
    {test, [
        {deps, [
            {ltest, "0.13.5"}
        ]}
    ]}
]}.
```

**File: `examples/simple-lib/src/mylib.lfe`**

```lfe
(defmodule mylib
  (export (add 2)
          (multiply 2)
          (greet 1)))

(defun add (a b)
  "Add two numbers"
  (+ a b))

(defun multiply (a b)
  "Multiply two numbers"
  (* a b))

(defun greet (name)
  "Greet someone"
  (io:format "Hello, ~s!~n" (list name)))
```

**File: `examples/simple-lib/test/mylib-tests.lfe`**

```lfe
(defmodule mylib-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest add-positive
  (is-equal 5 (mylib:add 2 3)))

(deftest add-negative
  (is-equal -1 (mylib:add 2 -3)))

(deftest multiply-positive
  (is-equal 6 (mylib:multiply 2 3)))

(deftest multiply-zero
  (is-equal 0 (mylib:multiply 5 0)))
```

**File: `examples/simple-lib/README.md`**

```markdown
# Simple Library Example

Basic LFE library with tests.

## Usage

```bash
# Compile
rebar3 lfe compile

# Run tests
rebar3 lfe ltest

# Start REPL
rebar3 lfe repl
```

## In REPL

```lfe
> (mylib:add 2 3)
5
> (mylib:multiply 4 5)
20
> (mylib:greet "World")
Hello, World!
ok
```
```

### Task 7.6: Create Troubleshooting Guide

**File: `docs/troubleshooting.md`**

```markdown
# Troubleshooting Guide

Common issues and solutions.

## Compilation Issues

### Files not recompiling

**Symptom:**
```bash
$ touch src/myapp.lfe
$ rebar3 lfe compile
==> All files up to date
```

**Cause:** Timestamp issue or cache problem

**Solution:**
```bash
# Force full recompilation
rebar3 lfe clean
rebar3 lfe compile
```

### Header changes ignored

**In 0.5.0, this should not happen!** If it does:

```bash
# Check header is in include/
ls -la include/*.lfe

# Verify include in source
grep "include-file" src/*.lfe

# Force recompile
rebar3 lfe clean
rebar3 lfe compile
```

**Report as bug if issue persists.**

### "Module not found" errors

**Symptom:**
```
Error: Module 'mymodule' not found
```

**Solutions:**

1. **Check filename matches module name:**
   ```lfe
   ;; In src/mymodule.lfe
   (defmodule mymodule)  ; ✅ Names match
   ```

2. **Check for typos:**
   ```lfe
   (defmodule my-module)  ; Hyphens
   ;; vs
   (defmodule my_module)  ; Underscores
   ```

3. **For packages, check path:**
   ```
   src/my/app/core.lfe → (defmodule my.app.core)
   ```

## REPL Issues

### REPL won't start

**Symptom:**
```bash
$ rebar3 lfe repl
Error: ...
```

**Solutions:**

1. **Check LFE version:**
   ```erlang
   {deps, [
       {lfe, "2.2.0"}  % Must be 2.2+
   ]}.
   ```

2. **Try without apps:**
   ```bash
   rebar3 lfe repl --apps ""
   ```

3. **Check for errors:**
   ```bash
   rebar3 lfe compile
   # Fix any compilation errors first
   ```

### Module not available in REPL

**Symptom:**
```lfe
> (mymodule:function)
** exception error: undefined function mymodule:function/0
```

**Solutions:**

1. **Compile first:**
   ```bash
   rebar3 lfe compile
   rebar3 lfe repl
   ```

2. **Reload in REPL:**
   ```lfe
   > (c "src/mymodule.lfe")
   {ok, mymodule}
   ```

## Package System Issues

### Temporary files not cleaned

**Symptom:**
```bash
$ ls src/
my.package.file.lfe  # Should not be there
```

**In 0.5.0, this should not happen!**

**Workaround:**
```bash
# Manual cleanup
find src -name "*.*.lfe" -type f -delete

# Report as bug
```

### Package name errors

**Symptom:**
```
Error: Invalid module name 'my..package'
```

**Cause:** Double dots in path or filename

**Solution:**
```bash
# ❌ Wrong
src/my//package.lfe

# ✅ Correct
src/my/package.lfe
```

## Performance Issues

### Slow compilation

**Check:**

1. **Debug logging enabled?**
   ```bash
   unset DEBUG
   rebar3 lfe compile
   ```

2. **Full rebuild happening?**
   ```bash
   # Should be fast:
   rebar3 lfe compile
   # If slow, check for issues
   ```

3. **Many files?**
   - First compile: Expected to be slow
   - Incremental: Should be fast
   - Check progress output

### Slow REPL startup

**Solutions:**

1. **Don't start all apps:**
   ```bash
   rebar3 lfe repl --apps ""
   ```

2. **Use release for production:**
   ```bash
   rebar3 lfe release
   ```

## Dependency Issues

### "ltest not found"

**Symptom:**
```
Error: ltest not found
```

**Solution:**
```erlang
{profiles, [
    {test, [
        {deps, [
            {ltest, "0.13.5"}
        ]}
    ]}
]}.
```

Then:
```bash
rebar3 as test lfe ltest
```

### Include from dependency fails

**Symptom:**
```
Error: Cannot find include file "dep/include/file.lfe"
```

**Solution:**
```lfe
;; Use include-lib
(include-lib "depname/include/file.lfe")
```

## Windows-Specific Issues

### Path separator errors

**r3lfe 0.5.0 handles this automatically!**

If you still see issues:
```bash
# Use forward slashes in config
{lfe_include_dirs, ["include"]}.  % Not "include\\"
```

## Getting More Help

### Enable Debug Logging

```bash
DEBUG=1 rebar3 lfe compile
```

### Check Versions

```bash
rebar3 lfe versions
```

### File an Issue

Include:
- r3lfe version
- Erlang/OTP version
- LFE version
- Full error message
- Steps to reproduce

**Issue Tracker:** https://github.com/lfe-rebar3/rebar3_lfe/issues
```

### Task 7.7: Create Contributing Guide

**File: `CONTRIBUTING.md`**

```markdown
# Contributing to r3lfe

Thanks for your interest in contributing!

## Development Setup

### Prerequisites

- Erlang/OTP 24+ (27 recommended)
- rebar3 3.22+
- git
- make

### Clone and Build

```bash
# Clone
git clone https://github.com/lfe-rebar3/rebar3_lfe.git
cd rebar3_lfe

# Create branch
git checkout -b feature/my-feature

# Compile
rebar3 compile

# Run tests
rebar3 ct

# Run all checks
make check
```

## Development Workflow

### 1. Create Feature Branch

```bash
git checkout -b feature/my-feature
```

Branch naming:
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation only
- `refactor/` - Code refactoring
- `test/` - Test improvements

### 2. Make Changes

Follow the code style:

```erlang
%% ✅ Good
-spec my_function(integer()) -> integer().
my_function(N) ->
    N + 1.

%% ❌ Bad (no spec)
my_function(N) ->
    N + 1.
```

### 3. Add Tests

```erlang
%% test/my_feature_SUITE.erl
my_test(_Config) ->
    ?assertEqual(Expected, Actual).
```

### 4. Run Tests

```bash
# Unit tests
rebar3 ct

# Property tests
rebar3 proper

# All checks
make check

# Coverage
make coverage
```

### 5. Commit

```bash
git add .
git commit -m "feat: add amazing feature"
```

Commit message format:
```
type: description

- feat: New feature
- fix: Bug fix
- docs: Documentation
- test: Tests
- refactor: Refactoring
```

### 6. Push and PR

```bash
git push origin feature/my-feature
```

Then create PR on GitHub.

## Code Standards

### Erlang Style

- Use `-spec` for all exported functions
- Max line length: 100 characters
- Use meaningful variable names
- Add comments for complex logic

### Testing

- Aim for >90% coverage
- Unit tests for individual functions
- Integration tests for workflows
- Property tests for invariants

### Documentation

- Update docs for API changes
- Add examples for new features
- Keep CHANGELOG up to date

## Project Structure

```
r3lfe/
├── src/              # Source code
│   ├── r3lfe*.erl   # Modules
│   └── r3lfe.hrl    # Shared header
├── test/             # Tests
│   ├── *_SUITE.erl   # CT suites
│   └── test_utils.erl
├── docs/             # Documentation
└── examples/         # Example projects
```

## Testing Guidelines

### Unit Tests

Test single functions in isolation:

```erlang
simple_test(_Config) ->
    Input = "test",
    Expected = "result",
    Actual = my_module:my_function(Input),
    ?assertEqual(Expected, Actual).
```

### Integration Tests

Test complete workflows:

```erlang
workflow_test(Config) ->
    Setup = setup_test_project(Config),
    Result = run_full_compilation(Setup),
    ?assertMatch({ok, _}, Result).
```

### Property Tests

Test invariants:

```erlang
prop_reversible() ->
    ?FORALL(Input, input_generator(),
        reverse(reverse(Input)) =:= Input).
```

## Common Tasks

### Adding a New Provider

1. Create `src/r3lfe_prv_mycommand.erl`
2. Implement provider behavior
3. Add tests in `test/`
4. Register in `src/r3lfe.erl`
5. Update documentation

### Adding a New Feature

1. Write tests first (TDD)
2. Implement feature
3. Ensure all tests pass
4. Update documentation
5. Add example if applicable

### Fixing a Bug

1. Write failing test
2. Fix the bug
3. Ensure test passes
4. Add regression test
5. Update CHANGELOG

## Release Process

(For maintainers)

```bash
# 1. Update version
vim src/r3lfe.app.src

# 2. Update CHANGELOG
vim CHANGELOG.md

# 3. Tag release
git tag v0.5.0
git push origin v0.5.0

# 4. Publish to hex
rebar3 hex publish
```

## Getting Help

- **Questions:** GitHub Discussions
- **Bugs:** GitHub Issues
- **Chat:** LFE Slack

## Code of Conduct

Be respectful and constructive.

## License

By contributing, you agree your contributions will be licensed under Apache 2.0.
```

### Task 7.8: Create Release Checklist

**File: `RELEASE_CHECKLIST.md`**

```markdown
# Release Checklist

Use this checklist when preparing a release.

## Pre-Release

- [ ] All tests passing on all supported OTP versions (24-28)
- [ ] CI green on main branch
- [ ] Code coverage >90%
- [ ] Dialyzer clean
- [ ] No known critical bugs

## Version Update

- [ ] Update version in `src/r3lfe.app.src`
- [ ] Update CHANGELOG.md with release notes
- [ ] Update README.md if needed
- [ ] Check all documentation up to date

## Testing

- [ ] Run full test suite: `make check`
- [ ] Run integration tests: `make smoke-tests`
- [ ] Test on real projects
- [ ] Test migration from 0.4.x

## Documentation

- [ ] All new features documented
- [ ] All breaking changes noted
- [ ] Examples work
- [ ] Migration guide complete

## Pre-Flight

- [ ] Create release branch: `release/0.5.x`
- [ ] Update CHANGELOG date
- [ ] Commit: `chore: prepare v0.5.0 release`
- [ ] Push to GitHub

## Release

- [ ] Create GitHub release with notes
- [ ] Tag: `git tag v0.5.0`
- [ ] Push tags: `git push origin v0.5.0`
- [ ] Publish to hex.pm: `rebar3 hex publish`

## Post-Release

- [ ] Verify on hex.pm
- [ ] Test installation from hex
- [ ] Announce on LFE mailing list
- [ ] Tweet about release
- [ ] Update documentation site

## If Issues Found

- [ ] Document in GitHub issues
- [ ] Plan hotfix if critical
- [ ] Update troubleshooting guide
```

### Task 7.9: Final README Polish

**File: `README.md` (FINAL VERSION)**

```markdown
# r3lfe - Modern rebar3 Plugin for LFE

[![CI/CD](https://github.com/lfe-rebar3/rebar3_lfe/workflows/CI%2FCD/badge.svg)](https://github.com/lfe-rebar3/rebar3_lfe/actions)
[![Coverage](https://codecov.io/gh/lfe-rebar3/rebar3_lfe/branch/main/graph/badge.svg)](https://codecov.io/gh/lfe-rebar3/rebar3_lfe)
[![Hex.pm](https://img.shields.io/hexpm/v/r3lfe.svg)](https://hex.pm/packages/r3lfe)
[![Hex Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/r3lfe/)
[![LFE](https://img.shields.io/badge/lfe-2.2+-blue.svg)](https://lfe.io)
[![Erlang](https://img.shields.io/badge/erlang-24--28-blue.svg)](https://www.erlang.org)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

**The modern, reliable rebar3 plugin for LFE (Lisp Flavoured Erlang) projects.**

## ✨ Why r3lfe?

- **🚀 Fast**: Incremental compilation is 10-30x faster than full rebuilds
- **🎯 Correct**: Header changes automatically trigger recompilation
- **🛡️ Reliable**: >90% test coverage, tested on Erlang/OTP 24-28
- **📦 Powerful**: Nested module packages with proper cleanup
- **💬 Clear**: Professional error messages that help you fix issues
- **🔧 Modern**: Uses rebar3's latest compiler infrastructure

## Quick Start

```erlang
%% rebar.config
{plugins, [
    {r3lfe, "0.5.0"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.
```

```bash
rebar3 lfe compile    # Compile your code
rebar3 lfe repl       # Start REPL
rebar3 lfe ltest      # Run tests
```

**[See Full Quick Start →](docs/quickstart.md)**

## Features

### 🔥 Smart Compilation

```bash
$ rebar3 lfe compile
Compiling 10 LFE files...
Progress: 10/10 (100%)
Compiled 10 files in 1.25s

$ touch include/records.lfe
$ rebar3 lfe compile
Compiling 3 LFE files...  # Only files using the header
Compiled 3 files in 0.3s
```

### 📦 Package System

Organize your code naturally:

```
src/
├── myapp.lfe           → myapp module
└── myapp/
    ├── core.lfe        → myapp.core module
    └── utils/
        └── helpers.lfe → myapp.utils.helpers module
```

### 🎨 Great Errors

```
src/myapp.lfe:10: error: undefined function foo/1
  Did you mean: bar/1?
```

### ⚡ All the Commands

- `compile` - Smart, incremental compilation
- `clean` - Remove build artifacts
- `repl` - Interactive LFE shell
- `ltest` - Run tests
- `release` - Build OTP releases
- `versions` - Version information

**[See All Commands →](docs/commands.md)**

## Documentation

- **[Quick Start](docs/quickstart.md)** - Get started in 5 minutes
- **[Commands](docs/commands.md)** - Complete command reference
- **[Configuration](docs/configuration.md)** - All config options
- **[Packages](docs/packages.md)** - Nested module system
- **[Troubleshooting](docs/troubleshooting.md)** - Common issues
- **[Migration Guide](MIGRATION.md)** - Upgrade from 0.4.x

## Examples

- [Simple Library](examples/simple-lib/) - Basic LFE library
- [Web Application](examples/web-app/) - Web app example
- [Umbrella Project](examples/umbrella/) - Multi-app project
- [OTP Release](examples/release/) - Production release

## Compatibility

| Erlang/OTP | rebar3  | r3lfe | Status |
|------------|---------|--------|--------|
| 28         | 3.25    | 0.5.0  | ✅ Tested |
| 27         | 3.25    | 0.5.0  | ✅ Tested |
| 26         | 3.25    | 0.5.0  | ✅ Tested |
| 25         | 3.22    | 0.5.0  | ✅ Tested |
| 24         | 3.22    | 0.5.0  | ✅ Tested |

## Breaking Changes from 0.4.x

Version 0.5.0 is a **complete rewrite** with breaking changes:

- Plugin renamed: `rebar3_lfe` → `r3lfe`
- Module prefix: `rebar3_lfe_*` → `r3lfe_*`
- Faster, more reliable compilation
- Better error messages

**[Migration Guide →](MIGRATION.md)**

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md).

```bash
git clone https://github.com/lfe-rebar3/rebar3_lfe.git
cd rebar3_lfe
rebar3 compile
make check
```

## Support

- **Documentation**: [lfe-rebar3.github.io/rebar3_lfe](https://lfe-rebar3.github.io/rebar3_lfe)
- **Issues**: [GitHub Issues](https://github.com/lfe-rebar3/rebar3_lfe/issues)
- **Discussions**: [GitHub Discussions](https://github.com/lfe-rebar3/rebar3_lfe/discussions)
- **Chat**: [LFE Slack](https://lfe-slack.herokuapp.com/)

## License

Apache 2.0 - See [LICENSE](LICENSE)

## Acknowledgments

- Original rebar3_lfe plugin maintainers
- LFE community
- rebar3 team for the excellent build tool

---

Made with ❤️ for the LFE community
```

## Final Steps

### Task 7.10: Pre-Release Verification

```bash
# 1. Full test suite
make clean
make check

# 2. Integration tests
make smoke-tests

# 3. Documentation check
cd docs && mdl *.md

# 4. Version consistency
grep -r "0.5.0" src/ docs/ examples/

# 5. Build clean install
rm -rf _build
rebar3 compile
rebar3 hex build

# 6. Test hex package
rebar3 hex build --unpack
```

## Expected Outcomes

At the end of Phase 7, you should have:

1. ✅ Complete, professional documentation
2. ✅ Migration guide from 0.4.x
3. ✅ Working examples for all use cases
4. ✅ Comprehensive troubleshooting guide
5. ✅ Contributing guide
6. ✅ Release checklist
7. ✅ Polished README with badges
8. ✅ All documentation verified
9. ✅ Ready for 0.5.0 release
10. ✅ Hex.pm package prepared

### Documentation Checklist

- [ ] README complete with badges
- [ ] CHANGELOG.md updated
- [ ] MIGRATION.md complete
- [ ] Quick start guide works
- [ ] All commands documented
- [ ] Configuration reference complete
- [ ] Troubleshooting covers common issues
- [ ] Examples all work
- [ ] Contributing guide clear
- [ ] License file present
- [ ] All docs spell-checked
- [ ] All links work

### Release Readiness

- [ ] All Phase 1-6 tests pass
- [ ] Documentation complete
- [ ] Examples verified
- [ ] Migration tested
- [ ] Version numbers consistent
- [ ] CHANGELOG dated
- [ ] CI green
- [ ] Ready to tag and release!

## Congratulations! 🎉

You've completed the full rewrite of rebar3_lfe as r3lfe! 

The plugin is now:
- **Modern** - Uses latest rebar3 APIs
- **Fast** - Incremental compilation
- **Correct** - Header dependency tracking
- **Tested** - >90% coverage
- **Documented** - Comprehensive guides
- **Production-ready** - Reliable and battle-tested

## Notes for Claude Code

- All documentation should be spell-checked
- Links should be verified before release
- Examples must be tested and working
- Migration guide should cover all edge cases
- Keep documentation maintainable (don't duplicate)
- Use relative links in documentation
- Include realistic examples
- Make troubleshooting actionable
- Contributing guide should lower barriers
- README should sell the value proposition
