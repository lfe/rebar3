# rebar3_lfe

[![CI/CD][gh-actions-badge]][gh-actions]
[![Hex.pm](https://img.shields.io/hexpm/v/rebar3_lfe.svg)](https://hex.pm/packages/rebar3_lfe)
[![LFE](https://img.shields.io/badge/lfe-2.2+-blue.svg)](https://lfe.io)
[![Erlang](https://img.shields.io/badge/erlang-24--28-blue.svg)](https://www.erlang.org)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

[![Project Logo][logo]][logo-large]

**A modern rebar3 plugin for LFE projects**

## ✨ Why rebar3_lfe?

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
    {rebar3_lfe, "0.5.0"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.
```

```bash
rebar3 lfe compile    # Compile your code
rebar3 lfe repl       # Start REPL
rebar3 lfe eval '(+ 1 2 3)'  # Evaluate LFE expressions
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

### 🤖 Updated REPL

REPL support in rebar3_lfe has changed slightly in 0.5.0:

- Easier support for customising the LFE REPL prompt
- `rlwrap` for more consistent experience with readline support (dedicated LFE history file, etc.)

There is a new `Makefile` target that is included with all generated projects (`rebar3 new lfe-*`) which makes it easier for projects to use rlwrap and prompt customisations:

```
make repl
```

[![LFE REPL](priv/images/screenshot-repl.png)](priv/images/screenshot-repl.png)

Autocompletion support is currently in progress; when complete, example usage will be shown here.

### 📦 Package System

Optional!

Organize your code by directories:

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

**Core:**

- `compile` - Smart, incremental compilation
- `clean` - Remove build artifacts
- `repl` - Interactive LFE shell
- `eval` - Evaluate LFE expressions
- `ltest` - Run tests
- `versions` - Version information

**Scripts & Escripts:**

- `run` - Execute LFE scripts (main/1)
- `escriptize` - Build standalone executables
- `run-escript` - Execute built escripts

**Releases:**

- `release` - Build OTP releases
- `run-release` - Manage releases (start/stop/console/etc)

**Utilities:**

- `confabulate` - Convert LFE data to Erlang format

**[See All Commands →](docs/commands.md)**

## Documentation

- **[Quick Start](docs/quickstart.md)** - Get started in 5 minutes
- **[Commands](docs/commands.md)** - Complete command reference
- **[Troubleshooting](docs/troubleshooting.md)** - Common issues
- **[Migration Guide](docs/0.4-to-0.5-migration.md)** - Upgrade from 0.4.x

## Examples

- [Simple Library](examples/simple-lib/) - Basic LFE library

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

- Module prefix: `rebar3_lfe_*` → `r3lfe_*`
- Faster, more reliable compilation
- Better error messages

**[Migration Guide →](./docs/0.4-to-0.5-migration.md)**

## Contributing

See [contributing](./docs/contributing.md).

```bash
git clone https://github.com/lfe/rebar3.git rebar3_lfe
cd rebar3_lfe
rebar3 compile
make check
```

## Support

- **Documentation**: [lfe.github.io/rebar3](https://lfe.github.io/rebar3)
- **Issues**: [GitHub Issues](https://github.com/lfe/rebar3/issues)
- **Discussions**: [GitHub Discussions](https://github.com/lfe/rebar3/discussions)
- **Chat**: `#tooling` in [LFE Discord](https://discord.gg/Uf3PszVHtF)

## License

Apache 2.0 - See [LICENSE](LICENSE)

---

[logo]: https://avatars2.githubusercontent.com/u/15242004?s=250
[logo-large]: https://avatars2.githubusercontent.com/u/15242004
[gh-actions-badge]: https://github.com/lfe-rebar3/rebar3_lfe/workflows/CI%2FCD/badge.svg
[gh-actions]: https://github.com/lfe-rebar3/rebar3_lfe/actions
