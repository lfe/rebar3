# Examples

Complete, working examples for all r3lfe features.

## Project Types

### [Simple Library](simple-lib/)

Basic LFE library with functions and tests.

**Features:**
- Basic module structure
- Exported functions
- ltest unit tests
- Clean project layout

**Commands:**
```bash
cd simple-lib
rebar3 lfe compile
rebar3 lfe ltest
rebar3 lfe repl
```

**Use When:**
- Learning LFE basics
- Creating reusable libraries
- Publishing to Hex.pm

---

### [Main Script](main-script/)

Runnable script with main/1 function.

**Features:**
- main/1 entry point
- Command-line argument parsing
- File I/O operations
- Error handling

**Commands:**
```bash
cd main-script
rebar3 lfe run -- input.txt output.txt
rebar3 lfe run --main src/process.lfe -- data.csv
```

**Use When:**
- Quick utility scripts
- Data processing tools
- Development automation

---

## Learning Path

### Beginner

1. [Simple Library](simple-lib/) - Learn basics

### Intermediate

2. Packages - Code organization (see nested module examples in simple-lib)

### Advanced

3. Release Management - Production deployment (coming soon)

## Quick Start

Clone and run any example:

```bash
# Clone r3lfe repository
git clone https://github.com/lfe-rebar3/rebar3_lfe.git
cd rebar3_lfe/examples

# Try an example
cd simple-lib
rebar3 lfe compile
rebar3 lfe ltest
```

## Contributing Examples

We welcome example contributions!

**Guidelines:**
- Keep examples focused and minimal
- Include clear README
- Add comprehensive comments
- Test all commands work
- Follow project structure conventions

**Submit:**
```bash
# Fork repository
# Add example in examples/your-example/
# Submit pull request
```

## See Also

- [Quick Start Guide](../docs/quickstart.md)
- [Command Reference](../docs/commands.md)
