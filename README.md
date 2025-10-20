# rebar3_lfe - Modern LFE Plugin for rebar3

[![CI/CD][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Hex.pm](https://img.shields.io/hexpm/v/rebar3_lfe.svg)](https://hex.pm/packages/rebar3_lfe)
[![Downloads][hex-downloads]][hex-package]

*A comprehensive, modern rebar3 plugin for LFE (Lisp Flavoured Erlang) projects*

[![Project Logo][logo]][logo-large]

## Features

- ✅ **Proper Dependency Tracking**: Header file changes trigger recompilation
- ✅ **Package System**: Organize code in nested directories (`src/my/package/*.lfe`)
- ✅ **Incremental Compilation**: Fast rebuilds with smart change detection
- ✅ **Modern rebar3 Integration**: Uses Custom Compiler Modules interface
- ✅ **Professional Error Messages**: Clear, actionable feedback
- ✅ **Comprehensive Testing**: >90% code coverage with property-based tests
- ✅ **Multi-OTP Support**: Works on Erlang/OTP 24-28

## Quick Start

Add to your `rebar.config`:

```erlang
{plugins, [
    {rebar3_lfe, "0.5.0"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.
```

## Commands

### Core Commands (Phase 5 - Current)

- `rebar3 lfe compile` - Compile LFE source files
- `rebar3 lfe clean` - Remove compiled .beam files
- `rebar3 lfe repl` - Start LFE REPL with project loaded
- `rebar3 lfe ltest` - Run tests using ltest framework
- `rebar3 lfe release` - Build OTP release
- `rebar3 lfe versions` - Display version information

### Legacy Commands (0.4.x - Deprecated)

The following commands from 0.4.x are deprecated and will be removed:

- `rebar3 lfe run`, `rebar3 lfe run-escript`, `rebar3 lfe run-release`
- `rebar3 lfe confabulate`
- `rebar3 lfe clean-build`, `rebar3 lfe clean-cache`, `rebar3 lfe clean-all`

## Breaking Changes from 0.4.x

Version 0.5.0 is a **complete rewrite** with the following changes:

1. **New Module Naming**: All internal modules renamed from `rebar3_lfe_*` to `rb3lfe_*`
2. **Package Support**: Nested source directories now supported (e.g., `src/my/package/utils.lfe`)
3. **Improved Architecture**: Better separation of concerns with dedicated compilation worker
4. **Better Dependency Tracking**: Header files properly tracked with dependency DAG
5. **Namespaced Commands**: All commands under `lfe` namespace (e.g., `rebar3 lfe compile`)

### Migration Guide

#### Before (0.4.x)

```erlang
{plugins, [
    {rebar3_lfe, "0.4.11"}
]}.
```

#### After (0.5.x)

```erlang
{plugins, [
    {rebar3_lfe, "0.5.0"}
]}.

%% Optional: Configure LFE compiler options
{lfe_opts, [
    debug_info,
    verbose
]}.

%% Optional: Configure first files (compile order)
{lfe_first_files, [
    "src/macros.lfe"
]}.
```

## Project Structure

The plugin supports both flat and nested project structures:

### Flat Structure

```
myproject/
├── rebar.config
├── src/
│   ├── myproject.lfe
│   └── utils.lfe
└── ebin/
    ├── myproject.beam
    └── utils.beam
```

### Nested Structure (Packages)

```
myproject/
├── rebar.config
├── src/
│   ├── myproject.lfe
│   └── myproject/
│       ├── utils.lfe
│       └── db/
│           └── queries.lfe
└── ebin/
    ├── myproject.beam
    ├── myproject.utils.beam
    └── myproject.db.queries.beam
```

## Development

### Running Tests

```bash
# Full test suite
make test

# With coverage
make coverage

# Just property tests
rebar3 proper

# Full CI check
make ci
```

### Quality Checks

```bash
# Run all checks
make check

# Individual checks
make xref
make dialyzer
```

## Documentation

Full documentation available at [https://lfe-rebar3.github.io/rebar3_lfe](https://lfe-rebar3.github.io/rebar3_lfe)

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup and guidelines.

## License

Apache 2.0 - See [LICENSE](LICENSE) for details.

## Upgrading Globally

If you have `rebar3_lfe` installed globally, update it as follows:

1. Edit `rebar3_lfe` entry in `~/.config/rebar3/rebar.config` to version `0.5.0`
2. Delete existing plugins: `rm -rf ~/.cache/rebar3/plugins/*lfe*`
3. Run any command to download the new version: `rebar3 lfe versions`

## Architecture

The 0.5.x series uses a modern, modular architecture:

- **rb3lfe**: Main plugin initialization
- **rb3lfe_compiler_mod**: rebar3 Custom Compiler Module interface
- **rb3lfe_compile_worker**: Individual file compilation
- **rb3lfe_dependency_scanner**: Header dependency tracking with DAG
- **rb3lfe_package**: Nested directory (package) support
- **rb3lfe_prv_***: Command providers (compile, clean, repl, etc.)

For more details, see the [design documentation](docs/design/).

[gh-actions-badge]: https://github.com/lfe/rebar3/workflows/CI%2FCD/badge.svg
[gh-actions]: https://github.com/lfe/rebar3/actions
[lfe-badge]: https://img.shields.io/badge/lfe-2.2+-blue.svg
[lfe]: https://github.com/lfe/lfe
[erlang-badge]: https://img.shields.io/badge/erlang-24--28-blue.svg
[versions]: https://github.com/lfe/rebar3/blob/master/.github/workflows/ci.yml
[hex-downloads]: https://img.shields.io/hexpm/dt/rebar3_lfe.svg
[hex-package]: https://hex.pm/packages/rebar3_lfe
[logo]: resources/images/lfe-logo-small.png
[logo-large]: resources/images/lfe-logo-large.png
