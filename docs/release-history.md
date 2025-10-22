# Release History

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
- **Data Conversion**: `rebar3 lfe confabulate` for LFE to Erlang data conversion
  - Config file conversion
  - Test data generation
  - Data migration tools

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
