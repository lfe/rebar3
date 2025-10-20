# Contributing to rb3lfe

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
rb3lfe/
├── src/              # Source code
│   ├── rb3lfe*.erl   # Modules
│   └── rb3lfe.hrl    # Shared header
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

1. Create `src/rb3lfe_prv_mycommand.erl`
2. Implement provider behavior
3. Add tests in `test/`
4. Register in `src/rb3lfe.erl`
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
vim src/rb3lfe.app.src

# 2. Update CHANGELOG
vim docs/release-history.md

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
