# Phase 9: Module Namespace Consolidation (rb3lfe → r3lfe)

## Overview

**Objective**: Consolidate the module namespace from `rb3lfe` to `r3lfe` across the entire codebase before the 0.5.0 release. This is the final polish phase before production release, ensuring a clean, concise namespace that will be maintained long-term.

**Rationale**: The `rb3lfe` prefix was used during development phases but is unnecessarily verbose. The `r3lfe` prefix is:
- More concise while remaining clear (rebar3 + LFE)
- Easier to type for developers
- Consistent with common Erlang naming patterns
- Not a breaking change since 0.5.0 has not been released

**Scope**: This phase involves systematic renaming of:
1. Module files (`.erl` source and test files)
2. Module declarations inside files (`-module(...)`)
3. All references to old module names throughout the codebase
4. Header files and include guards
5. Documentation files
6. Internal references in comments and documentation

**Timeline**: This is a mechanical refactoring that should be completed in a single session to avoid intermediate inconsistent states.

---

## File Rename Manifest

### Source Modules (src/)

```
OLD NAME                          → NEW NAME
================================================================================
src/rb3lfe.erl                    → src/r3lfe.erl
src/rb3lfe_compile_opts.erl       → src/r3lfe_compile_opts.erl
src/rb3lfe_compile_worker.erl     → src/r3lfe_compile_worker.erl
src/rb3lfe_compiler_mod.erl       → src/r3lfe_compiler_mod.erl
src/rb3lfe_config.erl             → src/r3lfe_config.erl
src/rb3lfe_dep_cache.erl          → src/r3lfe_dep_cache.erl
src/rb3lfe_dependency_scanner.erl → src/r3lfe_dependency_scanner.erl
src/rb3lfe_package.erl            → src/r3lfe_package.erl
src/rb3lfe_package_tracker.erl    → src/r3lfe_package_tracker.erl
src/rb3lfe_paths.erl              → src/r3lfe_paths.erl
src/rb3lfe_progress.erl           → src/r3lfe_progress.erl
src/rb3lfe_prv_clean.erl          → src/r3lfe_prv_clean.erl
src/rb3lfe_prv_compile.erl        → src/r3lfe_prv_compile.erl
src/rb3lfe_prv_confabulate.erl    → src/r3lfe_prv_confabulate.erl
src/rb3lfe_prv_escriptize.erl     → src/r3lfe_prv_escriptize.erl
src/rb3lfe_prv_ltest.erl          → src/r3lfe_prv_ltest.erl
src/rb3lfe_prv_release.erl        → src/r3lfe_prv_release.erl
src/rb3lfe_prv_repl.erl           → src/r3lfe_prv_repl.erl
src/rb3lfe_prv_run.erl            → src/r3lfe_prv_run.erl
src/rb3lfe_prv_run_escript.erl    → src/r3lfe_prv_run_escript.erl
src/rb3lfe_prv_run_release.erl    → src/r3lfe_prv_run_release.erl
src/rb3lfe_prv_versions.erl       → src/r3lfe_prv_versions.erl
```

**Total**: 22 source files

### Test Modules (test/)

```
OLD NAME                                → NEW NAME
================================================================================
test/rb3lfe_benchmarks.erl              → test/r3lfe_benchmarks.erl
test/rb3lfe_compile_worker_SUITE.erl    → test/r3lfe_compile_worker_SUITE.erl
test/rb3lfe_config_SUITE.erl            → test/r3lfe_config_SUITE.erl
test/rb3lfe_dependency_scanner_SUITE.erl → test/r3lfe_dependency_scanner_SUITE.erl
test/rb3lfe_package_SUITE.erl           → test/r3lfe_package_SUITE.erl
test/rb3lfe_paths_SUITE.erl             → test/r3lfe_paths_SUITE.erl
test/rb3lfe_properties.erl              → test/r3lfe_properties.erl
test/rb3lfe_prv_confabulate_SUITE.erl   → test/r3lfe_prv_confabulate_SUITE.erl
test/rb3lfe_prv_escriptize_SUITE.erl    → test/r3lfe_prv_escriptize_SUITE.erl
test/rb3lfe_prv_run_SUITE.erl           → test/r3lfe_prv_run_SUITE.erl
test/rb3lfe_prv_run_release_SUITE.erl   → test/r3lfe_prv_run_release_SUITE.erl
```

**Total**: 11 test files

### Header Files (include/)

```
OLD NAME                → NEW NAME
================================================================================
include/rb3lfe.hrl      → include/r3lfe.hrl
```

**Include Guard Update**: Change `RB3LFE_HRL` to `R3LFE_HRL`

### Documentation Files (docs/design/)

```
OLD NAME                          → NEW NAME
================================================================================
docs/design/rb3lfe_phase1.md      → docs/design/r3lfe_phase1.md
docs/design/rb3lfe_phase2.md      → docs/design/r3lfe_phase2.md
docs/design/rb3lfe_phase3.md      → docs/design/r3lfe_phase3.md
docs/design/rb3lfe_phase4.md      → docs/design/r3lfe_phase4.md
docs/design/rb3lfe_phase5.md      → docs/design/r3lfe_phase5.md
docs/design/rb3lfe_phase6.md      → docs/design/r3lfe_phase6.md
docs/design/rb3lfe_phase6_1.md    → docs/design/r3lfe_phase6_1.md
docs/design/rb3lfe_phase6_2.md    → docs/design/r3lfe_phase6_2.md
docs/design/rb3lfe_phase6_3.md    → docs/design/r3lfe_phase6_3.md
docs/design/rb3lfe_phase7.md      → docs/design/r3lfe_phase7.md
docs/design/rb3lfe_phase7_1.md    → docs/design/r3lfe_phase7_1.md
docs/design/rb3lfe_phase8.md      → docs/design/r3lfe_phase8.md
```

**Total**: 12 documentation files

---

## Search and Replace Strategy

### Phase 9.1: File Renaming

**Execution Order**: Rename files before updating content to avoid confusion.

**Git Operations**:
```bash
# Source files
git mv src/rb3lfe.erl src/r3lfe.erl
git mv src/rb3lfe_compile_opts.erl src/r3lfe_compile_opts.erl
git mv src/rb3lfe_compile_worker.erl src/r3lfe_compile_worker.erl
git mv src/rb3lfe_compiler_mod.erl src/r3lfe_compiler_mod.erl
git mv src/rb3lfe_config.erl src/r3lfe_config.erl
git mv src/rb3lfe_dep_cache.erl src/r3lfe_dep_cache.erl
git mv src/rb3lfe_dependency_scanner.erl src/r3lfe_dependency_scanner.erl
git mv src/rb3lfe_package.erl src/r3lfe_package.erl
git mv src/rb3lfe_package_tracker.erl src/r3lfe_package_tracker.erl
git mv src/rb3lfe_paths.erl src/r3lfe_paths.erl
git mv src/rb3lfe_progress.erl src/r3lfe_progress.erl
git mv src/rb3lfe_prv_clean.erl src/r3lfe_prv_clean.erl
git mv src/rb3lfe_prv_compile.erl src/r3lfe_prv_compile.erl
git mv src/rb3lfe_prv_confabulate.erl src/r3lfe_prv_confabulate.erl
git mv src/rb3lfe_prv_escriptize.erl src/r3lfe_prv_escriptize.erl
git mv src/rb3lfe_prv_ltest.erl src/r3lfe_prv_ltest.erl
git mv src/rb3lfe_prv_release.erl src/r3lfe_prv_release.erl
git mv src/rb3lfe_prv_repl.erl src/r3lfe_prv_repl.erl
git mv src/rb3lfe_prv_run.erl src/r3lfe_prv_run.erl
git mv src/rb3lfe_prv_run_escript.erl src/r3lfe_prv_run_escript.erl
git mv src/rb3lfe_prv_run_release.erl src/r3lfe_prv_run_release.erl
git mv src/rb3lfe_prv_versions.erl src/r3lfe_prv_versions.erl

# Test files
git mv test/rb3lfe_benchmarks.erl test/r3lfe_benchmarks.erl
git mv test/rb3lfe_compile_worker_SUITE.erl test/r3lfe_compile_worker_SUITE.erl
git mv test/rb3lfe_config_SUITE.erl test/r3lfe_config_SUITE.erl
git mv test/rb3lfe_dependency_scanner_SUITE.erl test/r3lfe_dependency_scanner_SUITE.erl
git mv test/rb3lfe_package_SUITE.erl test/r3lfe_package_SUITE.erl
git mv test/rb3lfe_paths_SUITE.erl test/r3lfe_paths_SUITE.erl
git mv test/rb3lfe_properties.erl test/r3lfe_properties.erl
git mv test/rb3lfe_prv_confabulate_SUITE.erl test/r3lfe_prv_confabulate_SUITE.erl
git mv test/rb3lfe_prv_escriptize_SUITE.erl test/r3lfe_prv_escriptize_SUITE.erl
git mv test/rb3lfe_prv_run_SUITE.erl test/r3lfe_prv_run_SUITE.erl
git mv test/rb3lfe_prv_run_release_SUITE.erl test/r3lfe_prv_run_release_SUITE.erl

# Header files
git mv include/rb3lfe.hrl include/r3lfe.hrl

# Documentation files
git mv docs/design/rb3lfe_phase1.md docs/design/r3lfe_phase1.md
git mv docs/design/rb3lfe_phase2.md docs/design/r3lfe_phase2.md
git mv docs/design/rb3lfe_phase3.md docs/design/r3lfe_phase3.md
git mv docs/design/rb3lfe_phase4.md docs/design/r3lfe_phase4.md
git mv docs/design/rb3lfe_phase5.md docs/design/r3lfe_phase5.md
git mv docs/design/rb3lfe_phase6.md docs/design/r3lfe_phase6.md
git mv docs/design/rb3lfe_phase6_1.md docs/design/r3lfe_phase6_1.md
git mv docs/design/rb3lfe_phase6_2.md docs/design/r3lfe_phase6_2.md
git mv docs/design/rb3lfe_phase6_3.md docs/design/r3lfe_phase6_3.md
git mv docs/design/rb3lfe_phase7.md docs/design/r3lfe_phase7.md
git mv docs/design/rb3lfe_phase7_1.md docs/design/r3lfe_phase7_1.md
git mv docs/design/rb3lfe_phase8.md docs/design/r3lfe_phase8.md
```

### Phase 9.2: Content Updates

**Global Search and Replace Patterns**:

1. **Module declarations** (`-module(...)`)
   - Pattern: `-module(rb3lfe`
   - Replace: `-module(r3lfe`

2. **Module references** (function calls, references)
   - Pattern: `rb3lfe:`
   - Replace: `r3lfe:`

3. **Include directives**
   - Pattern: `include/rb3lfe.hrl`
   - Replace: `include/r3lfe.hrl`
   - Pattern: `"rb3lfe.hrl"`
   - Replace: `"r3lfe.hrl"`

4. **Include guards**
   - Pattern: `RB3LFE_HRL`
   - Replace: `R3LFE_HRL`

5. **Atom references**
   - Pattern: `'rb3lfe`
   - Replace: `'r3lfe`

6. **String references** (in comments, docs, error messages)
   - Pattern: `rb3lfe`
   - Replace: `r3lfe`

**Files Requiring Content Updates**: ALL files in the repository

**Critical Files List**:
```
src/*.erl                          (all source files)
test/*.erl                         (all test files)
include/*.hrl                      (all header files)
docs/**/*.md                       (all documentation)
README.md
CHANGELOG
MIGRATION.md
CONTRIBUTING.md
PHASE2_STATUS.md
RELEASE_CHECKLIST.md
rebar.config                       (if module names referenced)
src/rebar3_lfe.app.src            (application specification)
.github/workflows/*.yml           (CI/CD configurations)
Makefile                          (build targets)
```

### Phase 9.3: Special Cases

**1. Application Name Considerations**

The plugin is called `rebar3_lfe`, which should remain unchanged:
- `rebar.config` dependency: `{rebar3_lfe, ...}` ✓ (keep as is)
- `src/rebar3_lfe.app.src` application name: `rebar3_lfe` ✓ (keep as is)
- Plugin entry point: `src/rebar3_lfe.erl` ✓ (keep as is)

**Only internal modules change**: `rb3lfe_*` → `r3lfe_*`

**2. Backward Compatibility Modules**

The following modules in the legacy namespace should remain unchanged (they already exist for backward compatibility):
- `src/rebar3_lfe_*.erl` (legacy provider interface)
- These modules delegate to the new `r3lfe_*` modules

**Update these files** to reference `r3lfe_*` instead of `rb3lfe_*`:
```erlang
% src/rebar3_lfe_prv_compile.erl
-module(rebar3_lfe_prv_compile).
% ... but internally calls:
% rb3lfe_prv_compile:do(State)  → r3lfe_prv_compile:do(State)
```

**3. Test Suite References**

Update test helper modules:
- `test/test_utils.erl` - update any references to `rb3lfe_*` modules
- `test/*.erl` - update all `-include` directives
- `test/test.config` - update module references if any

**4. Documentation Cross-References**

Update internal links in markdown files:
```markdown
<!-- OLD -->
See [rb3lfe_phase1.md](rb3lfe_phase1.md) for details.

<!-- NEW -->
See [r3lfe_phase1.md](r3lfe_phase1.md) for details.
```

**5. CI/CD and Build Scripts**

Update any references in:
- `.github/workflows/*.yml`
- `Makefile` targets
- Build scripts that reference module names

---

## Verification Checklist

### Pre-Rename Verification

- [ ] All tests pass with current naming
- [ ] Clean build succeeds: `rebar3 clean && rebar3 compile`
- [ ] No uncommitted changes in working directory
- [ ] Create backup branch: `git checkout -b pre-phase9-backup`

### Post-Rename Verification

**Compilation Checks**:
- [ ] Clean compile succeeds: `rebar3 clean && rebar3 compile`
- [ ] No compilation warnings
- [ ] No undefined module errors
- [ ] Dialyzer passes: `rebar3 dialyzer`

**Test Suite Checks**:
- [ ] All unit tests pass: `rebar3 eunit`
- [ ] All CT suites pass: `rebar3 ct`
- [ ] All property tests pass
- [ ] Coverage remains stable: `rebar3 cover`

**Module Reference Checks**:
```bash
# Verify no old references remain in source
grep -r "rb3lfe" src/ test/ include/
# Should only find matches in rebar3_lfe_* legacy modules

# Verify all new references are correct
grep -r "r3lfe:" src/ test/ include/
# Should find all function calls to new modules

# Check module declarations
grep "^-module(rb3lfe" src/*.erl test/*.erl
# Should return empty (all should be r3lfe)

# Check include directives
grep '#include.*rb3lfe' src/*.erl test/*.erl include/*.hrl
# Should return empty (all should be r3lfe)
```

**Documentation Checks**:
- [ ] All links in README.md work
- [ ] Phase documentation references updated
- [ ] CHANGELOG reflects naming change
- [ ] No broken links in docs/

**Integration Checks**:
- [ ] Plugin loads correctly: `rebar3 plugins list`
- [ ] Commands work: `rebar3 lfe compile`
- [ ] REPL starts: `rebar3 lfe repl`
- [ ] Tests run: `rebar3 lfe ltest`

### Grep Pattern Verification

Run these checks to ensure complete migration:

```bash
# Should find NO occurrences (except in this phase9 doc and CHANGELOG):
git grep -n "rb3lfe_compile_opts"
git grep -n "rb3lfe_compiler_mod"
git grep -n "rb3lfe_config"
git grep -n "rb3lfe_dependency_scanner"
git grep -n "rb3lfe_package"

# Should find MANY occurrences (in new module names):
git grep -n "r3lfe_compile_opts"
git grep -n "r3lfe_compiler_mod"
git grep -n "r3lfe_config"

# Verify include guard updated:
git grep -n "RB3LFE_HRL"  # Should be empty
git grep -n "R3LFE_HRL"   # Should find header file
```

---

## Implementation Script

For automated execution, use this script:

```bash
#!/bin/bash
# phase9_rename.sh - Automated namespace consolidation

set -e  # Exit on any error

echo "=== Phase 9: rb3lfe → r3lfe Namespace Consolidation ==="
echo

# Safety check
if [ -n "$(git status --porcelain)" ]; then
    echo "ERROR: Working directory has uncommitted changes"
    echo "Please commit or stash changes before running this script"
    exit 1
fi

echo "Step 1: Creating backup branch..."
git checkout -b pre-phase9-backup
git checkout -

echo "Step 2: Renaming source files..."
git mv src/rb3lfe.erl src/r3lfe.erl
git mv src/rb3lfe_compile_opts.erl src/r3lfe_compile_opts.erl
git mv src/rb3lfe_compile_worker.erl src/r3lfe_compile_worker.erl
git mv src/rb3lfe_compiler_mod.erl src/r3lfe_compiler_mod.erl
git mv src/rb3lfe_config.erl src/r3lfe_config.erl
git mv src/rb3lfe_dep_cache.erl src/r3lfe_dep_cache.erl
git mv src/rb3lfe_dependency_scanner.erl src/r3lfe_dependency_scanner.erl
git mv src/rb3lfe_package.erl src/r3lfe_package.erl
git mv src/rb3lfe_package_tracker.erl src/r3lfe_package_tracker.erl
git mv src/rb3lfe_paths.erl src/r3lfe_paths.erl
git mv src/rb3lfe_progress.erl src/r3lfe_progress.erl
git mv src/rb3lfe_prv_clean.erl src/r3lfe_prv_clean.erl
git mv src/rb3lfe_prv_compile.erl src/r3lfe_prv_compile.erl
git mv src/rb3lfe_prv_confabulate.erl src/r3lfe_prv_confabulate.erl
git mv src/rb3lfe_prv_escriptize.erl src/r3lfe_prv_escriptize.erl
git mv src/rb3lfe_prv_ltest.erl src/r3lfe_prv_ltest.erl
git mv src/rb3lfe_prv_release.erl src/r3lfe_prv_release.erl
git mv src/rb3lfe_prv_repl.erl src/r3lfe_prv_repl.erl
git mv src/rb3lfe_prv_run.erl src/r3lfe_prv_run.erl
git mv src/rb3lfe_prv_run_escript.erl src/r3lfe_prv_run_escript.erl
git mv src/rb3lfe_prv_run_release.erl src/r3lfe_prv_run_release.erl
git mv src/rb3lfe_prv_versions.erl src/r3lfe_prv_versions.erl

echo "Step 3: Renaming test files..."
git mv test/rb3lfe_benchmarks.erl test/r3lfe_benchmarks.erl
git mv test/rb3lfe_compile_worker_SUITE.erl test/r3lfe_compile_worker_SUITE.erl
git mv test/rb3lfe_config_SUITE.erl test/r3lfe_config_SUITE.erl
git mv test/rb3lfe_dependency_scanner_SUITE.erl test/r3lfe_dependency_scanner_SUITE.erl
git mv test/rb3lfe_package_SUITE.erl test/r3lfe_package_SUITE.erl
git mv test/rb3lfe_paths_SUITE.erl test/r3lfe_paths_SUITE.erl
git mv test/rb3lfe_properties.erl test/r3lfe_properties.erl
git mv test/rb3lfe_prv_confabulate_SUITE.erl test/r3lfe_prv_confabulate_SUITE.erl
git mv test/rb3lfe_prv_escriptize_SUITE.erl test/r3lfe_prv_escriptize_SUITE.erl
git mv test/rb3lfe_prv_run_SUITE.erl test/r3lfe_prv_run_SUITE.erl
git mv test/rb3lfe_prv_run_release_SUITE.erl test/r3lfe_prv_run_release_SUITE.erl

echo "Step 4: Renaming header files..."
git mv include/rb3lfe.hrl include/r3lfe.hrl

echo "Step 5: Renaming documentation files..."
git mv docs/design/rb3lfe_phase1.md docs/design/r3lfe_phase1.md
git mv docs/design/rb3lfe_phase2.md docs/design/r3lfe_phase2.md
git mv docs/design/rb3lfe_phase3.md docs/design/r3lfe_phase3.md
git mv docs/design/rb3lfe_phase4.md docs/design/r3lfe_phase4.md
git mv docs/design/rb3lfe_phase5.md docs/design/r3lfe_phase5.md
git mv docs/design/rb3lfe_phase6.md docs/design/r3lfe_phase6.md
git mv docs/design/rb3lfe_phase6_1.md docs/design/r3lfe_phase6_1.md
git mv docs/design/rb3lfe_phase6_2.md docs/design/r3lfe_phase6_2.md
git mv docs/design/rb3lfe_phase6_3.md docs/design/r3lfe_phase6_3.md
git mv docs/design/rb3lfe_phase7.md docs/design/r3lfe_phase7.md
git mv docs/design/rb3lfe_phase7_1.md docs/design/r3lfe_phase7_1.md
git mv docs/design/rb3lfe_phase8.md docs/design/r3lfe_phase8.md

echo "Step 6: Updating content (module declarations)..."
find src test -name "*.erl" -exec sed -i 's/-module(rb3lfe/-module(r3lfe/g' {} +

echo "Step 7: Updating content (module references)..."
find src test include docs -type f -exec sed -i 's/rb3lfe:/r3lfe:/g' {} +

echo "Step 8: Updating content (include directives)..."
find src test include -type f -exec sed -i 's/rb3lfe\.hrl/r3lfe.hrl/g' {} +

echo "Step 9: Updating content (include guards)..."
find include -name "*.hrl" -exec sed -i 's/RB3LFE_HRL/R3LFE_HRL/g' {} +

echo "Step 10: Updating content (atom references)..."
find src test -name "*.erl" -exec sed -i "s/'rb3lfe/'r3lfe/g" {} +

echo "Step 11: Updating content (general string references)..."
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_compile_opts/r3lfe_compile_opts/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_compile_worker/r3lfe_compile_worker/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_compiler_mod/r3lfe_compiler_mod/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_config/r3lfe_config/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_dep_cache/r3lfe_dep_cache/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_dependency_scanner/r3lfe_dependency_scanner/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_package/r3lfe_package/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_package_tracker/r3lfe_package_tracker/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_paths/r3lfe_paths/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_progress/r3lfe_progress/g' {} +
find . -type f ! -path "./.git/*" ! -path "./_build/*" -exec sed -i 's/rb3lfe_prv_/r3lfe_prv_/g' {} +

echo "Step 12: Verifying changes..."
echo "Checking for remaining rb3lfe references (excluding legacy rebar3_lfe modules)..."
REMAINING=$(git grep -n "rb3lfe" | grep -v "rebar3_lfe" | grep -v "phase9" | grep -v "CHANGELOG" || true)
if [ -n "$REMAINING" ]; then
    echo "WARNING: Found remaining rb3lfe references:"
    echo "$REMAINING"
else
    echo "✓ No remaining rb3lfe references found"
fi

echo
echo "Step 13: Testing compilation..."
rebar3 clean
if rebar3 compile; then
    echo "✓ Compilation successful"
else
    echo "✗ Compilation failed - review changes"
    exit 1
fi

echo
echo "Step 14: Running tests..."
if rebar3 ct; then
    echo "✓ Tests passed"
else
    echo "✗ Tests failed - review changes"
    exit 1
fi

echo
echo "=== Phase 9 Complete ==="
echo "Next steps:"
echo "  1. Review git diff"
echo "  2. Run additional manual verification"
echo "  3. Update CHANGELOG with rename notes"
echo "  4. Commit changes: git commit -m 'Phase 9: Consolidate namespace (rb3lfe → r3lfe)'"
echo
echo "Backup branch available: pre-phase9-backup"
```

**Note**: Make script executable: `chmod +x phase9_rename.sh`

---

## Manual Verification Procedures

After running automated script, perform these manual checks:

### 1. Module Loading Test

```erlang
% In Erlang shell
1> code:ensure_loaded(r3lfe).
{module, r3lfe}
2> code:ensure_loaded(r3lfe_config).
{module, r3lfe_config}
3> code:ensure_loaded(r3lfe_compiler_mod).
{module, r3lfe_compiler_mod}
```

### 2. Provider Registration Test

```bash
rebar3 help lfe compile
# Should show help for compile command

rebar3 plugins list
# Should show rebar3_lfe plugin loaded
```

### 3. Full Integration Test

```bash
# Create test project
mkdir /tmp/test_phase9
cd /tmp/test_phase9
rebar3 new lfe-lib testlib

# Add plugin to rebar.config
echo '{plugins, [rebar3_lfe]}.' >> rebar.config

# Test compilation
rebar3 lfe compile

# Test other commands
rebar3 lfe ltest
rebar3 lfe repl --help
```

### 4. Documentation Link Test

```bash
# Check for broken links
cd docs/
for file in *.md design/*.md; do
    echo "Checking $file..."
    grep -o '\[.*\](.*\.md)' "$file" | while read link; do
        target=$(echo "$link" | sed 's/.*(\(.*\))/\1/')
        if [ ! -f "$target" ] && [ ! -f "design/$target" ]; then
            echo "  BROKEN: $link in $file"
        fi
    done
done
```

### 5. Module Dependency Graph Check

```bash
# Generate xref analysis
rebar3 xref

# Should show no undefined function calls
# Should show all r3lfe_* modules properly referenced
```

---

## CHANGELOG Entry

Add to `CHANGELOG`:

```markdown
## [0.5.0] - YYYY-MM-DD

### Changed
- **BREAKING (internal only)**: Consolidated internal module namespace from `rb3lfe_*` to `r3lfe_*`
  - All internal modules renamed for conciseness
  - Public API unchanged (still `rebar3_lfe` plugin name)
  - No user-facing changes required
  - Affects only developers extending the plugin
  - Legacy `rebar3_lfe_*` compatibility modules maintained
- Module renames:
  - `rb3lfe_compiler_mod` → `r3lfe_compiler_mod`
  - `rb3lfe_dependency_scanner` → `r3lfe_dependency_scanner`
  - `rb3lfe_config` → `r3lfe_config`
  - And all other `rb3lfe_*` modules
- Header file: `include/rb3lfe.hrl` → `include/r3lfe.hrl`
- Updated all internal references across codebase
- Documentation updated to reflect new naming
```

---

## Rollback Procedure

If issues arise after Phase 9:

```bash
# Reset to pre-phase9 state
git reset --hard pre-phase9-backup

# Or, if already committed
git revert <commit-hash>

# Or, restore individual files
git checkout pre-phase9-backup -- src/rb3lfe*.erl
```

---

## Post-Phase 9 Tasks

### Immediate

- [ ] Update CHANGELOG with rename details
- [ ] Review all git diffs for accuracy
- [ ] Commit with descriptive message
- [ ] Push to feature branch for review
- [ ] Run CI/CD pipeline

### Before 0.5.0 Release

- [ ] Update any external documentation referencing module names
- [ ] Verify hex.pm documentation will build correctly
- [ ] Update any blog posts or tutorials (if applicable)
- [ ] Notify maintainers of dependent projects (if any)

### Documentation Updates

- [ ] Verify README.md examples work
- [ ] Test quickstart guide
- [ ] Check troubleshooting guide references
- [ ] Validate all command examples

---

## Risk Assessment

**Risk Level**: LOW

**Justification**:
1. Pure internal refactoring (no API changes)
2. Version 0.5.0 not yet released (no breaking changes to users)
3. Automated script with verification steps
4. Comprehensive test suite catches issues
5. Backup branch available for rollback

**Mitigation**:
- Automated script reduces manual errors
- Multiple verification layers (compile, test, integration)
- Gradual rollout via feature branch
- CI/CD gates prevent broken releases

---

## Success Criteria

Phase 9 is complete when:

- [ ] All 22 source files renamed
- [ ] All 11 test files renamed
- [ ] Header file renamed with guard updated
- [ ] All 12 documentation files renamed
- [ ] Zero occurrences of `rb3lfe_*` in codebase (except legacy compatibility modules)
- [ ] All tests pass
- [ ] Clean compilation with no warnings
- [ ] Dialyzer analysis passes
- [ ] Plugin commands work correctly
- [ ] Documentation links valid
- [ ] CHANGELOG updated
- [ ] Changes committed to git

---

## Timeline

**Estimated Duration**: 2-3 hours

**Breakdown**:
- File renaming: 15 minutes (automated)
- Content updates: 30 minutes (automated + verification)
- Compilation testing: 15 minutes
- Test suite execution: 30 minutes
- Manual verification: 30 minutes
- Documentation review: 30 minutes
- Buffer for issues: 30 minutes

**Recommended Schedule**:
- Execute during low-activity period
- Single continuous session (avoid partial completion)
- Have rollback plan ready
- Test thoroughly before committing

---

## Notes

**Why r3lfe?**
- **r3**: Clear abbreviation of rebar3
- **lfe**: Official LFE language acronym
- **Concise**: 5 characters vs 6 in rb3lfe
- **Consistent**: Matches patterns like `r3_hex`, `r3_plugins`
- **Memorable**: Easy to type and remember

**What Stays the Same?**
- Plugin name: `rebar3_lfe` (public API)
- Commands: `rebar3 lfe compile`, etc.
- Configuration: Still use `lfe` key in rebar.config
- Application: `rebar3_lfe.app.src` unchanged
- User experience: Completely transparent

**Future Considerations**:
- Module namespace now consistent for long-term maintenance
- Easier to extend with new modules
- Clearer distinction between public API (`rebar3_lfe_*`) and internal modules (`r3lfe_*`)
- Sets pattern for any future internal refactoring

---

## Conclusion

Phase 9 represents the final polish before the 0.5.0 release, consolidating the internal module namespace for long-term maintainability. This mechanical refactoring has no user-facing impact but significantly improves code clarity and developer experience. The comprehensive verification procedures ensure a smooth transition with minimal risk.

**Next Phase**: After Phase 9 completion, proceed to final 0.5.0 release preparation (version bumps, final documentation review, release notes compilation).
