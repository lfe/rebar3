# Phase 8: Legacy Code Removal & Clean-up

## Overview

This phase removes all old rebar3_lfe code that has been replaced by the modern r3lfe implementation. We'll systematically identify, verify, and remove deprecated modules while ensuring nothing breaks.

**Goal**: Clean, maintainable codebase with no dead code.

## Prerequisites

- Phase 1-5 completed
- Phase 6.1, 6.2, 6.3 completed
- Phase 7 and 7.1 completed
- All tests passing
- All new providers working

## Old Code Inventory

Based on the files you provided, here's what needs to be removed:

### Old Provider Files (To Remove)

```
src/rebar3_lfe_prv_confabulate.erl    → Replaced by r3lfe_prv_confabulate.erl
src/rebar3_lfe_prv_escriptize.erl     → Replaced by r3lfe_prv_escriptize.erl
src/rebar3_lfe_prv_run.erl            → Replaced by r3lfe_prv_run.erl
src/rebar3_lfe_prv_run_escript.erl    → Replaced by r3lfe_prv_run_escript.erl
src/rebar3_lfe_prv_run_release.erl    → Replaced by r3lfe_prv_run_release.erl
```

### Other Old Files (To Verify and Remove)

These likely exist but weren't in your sample:

```
src/rebar3_lfe.erl                    → Replaced by r3lfe.erl
src/rebar3_lfe.hrl                    → Replaced by r3lfe.hrl
src/rebar3_lfe_prv_compile.erl        → Replaced by r3lfe_prv_compile.erl
src/rebar3_lfe_prv_clean.erl          → Replaced by r3lfe_prv_clean.erl
src/rebar3_lfe_prv_repl.erl           → Replaced by r3lfe_prv_repl.erl
src/rebar3_lfe_prv_ltest.erl          → Replaced by r3lfe_prv_ltest.erl
src/rebar3_lfe_prv_release.erl        → Replaced by r3lfe_prv_release.erl
src/rebar3_lfe_prv_versions.erl       → Replaced by r3lfe_prv_versions.erl
src/rebar3_lfe_utils.erl              → Replaced by r3lfe_* modules
src/rebar3_lfe_package.erl            → Replaced by r3lfe_package.erl
```

### Old App File

```
src/rebar3_lfe.app.src                → Replaced by r3lfe.app.src
```

## Implementation Tasks

### Task 8.1: Inventory Old Files

**Create Script: `scripts/find_old_code.sh`**

```bash
#!/bin/bash
# Find all old rebar3_lfe files

echo "=== Finding old rebar3_lfe files ==="
echo

echo "Source files:"
find src -name "rebar3_lfe*" -type f | sort

echo
echo "Include files:"
find include -name "rebar3_lfe*" -type f | sort

echo
echo "Test files:"
find test -name "rebar3_lfe*" -type f | sort

echo
echo "App file:"
find . -name "rebar3_lfe.app.src" -type f

echo
echo "=== Summary ==="
echo "Total old files: $(find . -name "rebar3_lfe*" -type f | wc -l)"
```

**Run it:**

```bash
chmod +x scripts/find_old_code.sh
./scripts/find_old_code.sh > old_files_inventory.txt
cat old_files_inventory.txt
```

### Task 8.2: Verify New Implementations Exist

**Create Script: `scripts/verify_replacements.sh`**

```bash
#!/bin/bash
# Verify all old files have new replacements

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "=== Verifying replacements for old files ==="
echo

# List of old files that should have replacements
OLD_FILES=(
    "src/rebar3_lfe.erl:src/r3lfe.erl"
    "src/rebar3_lfe.hrl:include/r3lfe.hrl"
    "src/rebar3_lfe_prv_compile.erl:src/r3lfe_prv_compile.erl"
    "src/rebar3_lfe_prv_clean.erl:src/r3lfe_prv_clean.erl"
    "src/rebar3_lfe_prv_repl.erl:src/r3lfe_prv_repl.erl"
    "src/rebar3_lfe_prv_ltest.erl:src/r3lfe_prv_ltest.erl"
    "src/rebar3_lfe_prv_release.erl:src/r3lfe_prv_release.erl"
    "src/rebar3_lfe_prv_versions.erl:src/r3lfe_prv_versions.erl"
    "src/rebar3_lfe_prv_run.erl:src/r3lfe_prv_run.erl"
    "src/rebar3_lfe_prv_escriptize.erl:src/r3lfe_prv_escriptize.erl"
    "src/rebar3_lfe_prv_run_escript.erl:src/r3lfe_prv_run_escript.erl"
    "src/rebar3_lfe_prv_run_release.erl:src/r3lfe_prv_run_release.erl"
    "src/rebar3_lfe_prv_confabulate.erl:src/r3lfe_prv_confabulate.erl"
    "src/rebar3_lfe_utils.erl:SPLIT"
    "src/rebar3_lfe_package.erl:src/r3lfe_package.erl"
    "src/rebar3_lfe.app.src:src/r3lfe.app.src"
)

MISSING=0

for pair in "${OLD_FILES[@]}"; do
    OLD="${pair%%:*}"
    NEW="${pair##*:}"

    if [ -f "$OLD" ]; then
        if [ "$NEW" = "SPLIT" ]; then
            echo -e "${GREEN}✓${NC} $OLD (split into multiple r3lfe_* modules)"
        elif [ -f "$NEW" ]; then
            echo -e "${GREEN}✓${NC} $OLD → $NEW"
        else
            echo -e "${RED}✗${NC} $OLD exists but $NEW is missing!"
            MISSING=$((MISSING + 1))
        fi
    fi
done

echo
if [ $MISSING -eq 0 ]; then
    echo -e "${GREEN}All old files have replacements!${NC}"
    exit 0
else
    echo -e "${RED}ERROR: $MISSING files are missing replacements!${NC}"
    exit 1
fi
```

**Run it:**

```bash
chmod +x scripts/verify_replacements.sh
./scripts/verify_replacements.sh
```

### Task 8.3: Verify Tests Still Pass

Before removing anything, ensure everything works:

```bash
# Full test suite
rebar3 ct

# Dialyzer
rebar3 dialyzer

# Xref
rebar3 xref

# Full check
make check
```

**Document results:**

```bash
echo "=== Test Results Before Cleanup ===" > cleanup_test_results.txt
rebar3 ct >> cleanup_test_results.txt 2>&1
echo "Exit code: $?" >> cleanup_test_results.txt
```

### Task 8.4: Create Backup

**Before removing anything:**

```bash
# Create backup branch
git checkout -b backup-before-cleanup
git commit -am "Backup before Phase 8 cleanup"
git checkout main

# Or create archive
tar -czf backup-old-code-$(date +%Y%m%d).tar.gz \
    src/rebar3_lfe* \
    include/rebar3_lfe* \
    test/rebar3_lfe* \
    2>/dev/null || true
```

### Task 8.5: Remove Old Files

**Create Script: `scripts/remove_old_code.sh`**

```bash
#!/bin/bash
# Remove old rebar3_lfe files

set -e  # Exit on error

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=== Phase 8: Removing Old Code ==="
echo

# Confirm with user
read -p "This will DELETE old rebar3_lfe files. Continue? (yes/no): " confirm
if [ "$confirm" != "yes" ]; then
    echo "Aborted."
    exit 1
fi

echo
echo "Creating backup..."
git checkout -b backup-old-code-$(date +%Y%m%d) 2>/dev/null || true
git checkout main

echo
echo "Removing old files..."

# Remove old source files
OLD_SRC_FILES=(
    "src/rebar3_lfe.erl"
    "src/rebar3_lfe_prv_compile.erl"
    "src/rebar3_lfe_prv_clean.erl"
    "src/rebar3_lfe_prv_repl.erl"
    "src/rebar3_lfe_prv_ltest.erl"
    "src/rebar3_lfe_prv_release.erl"
    "src/rebar3_lfe_prv_versions.erl"
    "src/rebar3_lfe_prv_run.erl"
    "src/rebar3_lfe_prv_escriptize.erl"
    "src/rebar3_lfe_prv_run_escript.erl"
    "src/rebar3_lfe_prv_run_release.erl"
    "src/rebar3_lfe_prv_confabulate.erl"
    "src/rebar3_lfe_utils.erl"
    "src/rebar3_lfe_package.erl"
    "src/rebar3_lfe_compiler.erl"
)

for file in "${OLD_SRC_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo -e "  ${RED}Removing${NC} $file"
        git rm -f "$file" 2>/dev/null || rm -f "$file"
    fi
done

# Remove old header files
OLD_HEADER_FILES=(
    "src/rebar3_lfe.hrl"
    "include/rebar3_lfe.hrl"
)

for file in "${OLD_HEADER_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo -e "  ${RED}Removing${NC} $file"
        git rm -f "$file" 2>/dev/null || rm -f "$file"
    fi
done

# Remove old app file
if [ -f "src/rebar3_lfe.app.src" ]; then
    echo -e "  ${RED}Removing${NC} src/rebar3_lfe.app.src"
    git rm -f "src/rebar3_lfe.app.src" 2>/dev/null || rm -f "src/rebar3_lfe.app.src"
fi

# Remove old test files (if any)
find test -name "rebar3_lfe*" -type f | while read file; do
    echo -e "  ${RED}Removing${NC} $file"
    git rm -f "$file" 2>/dev/null || rm -f "$file"
done

echo
echo -e "${GREEN}Old files removed!${NC}"

echo
echo "Files remaining with 'rebar3_lfe' in name:"
find . -name "*rebar3_lfe*" -type f | grep -v "\.git" || echo "  (none)"

echo
echo "Next steps:"
echo "  1. Verify everything compiles: rebar3 compile"
echo "  2. Run tests: rebar3 ct"
echo "  3. Check xref: rebar3 xref"
echo "  4. Commit: git commit -m 'Remove old rebar3_lfe code (Phase 8)'"
```

**Run it:**

```bash
chmod +x scripts/remove_old_code.sh
./scripts/remove_old_code.sh
```

### Task 8.6: Update .app.src Files List

**File: `src/r3lfe.app.src` (UPDATE)**

Ensure the files list doesn't reference old code:

```erlang
{application, r3lfe, [
    {description, "Modern rebar3 plugin for LFE projects"},
    {vsn, "0.5.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        lfe
    ]},
    {env, []},
    {modules, []},  % Auto-filled, but verify no rebar3_lfe_* modules

    %% Hex.pm metadata
    {licenses, ["Apache-2.0"]},
    {links, [
        {"GitHub", "https://github.com/lfe-rebar3/rebar3_lfe"},
        {"Hex", "https://hex.pm/packages/r3lfe"},
        {"LFE", "https://lfe.io"}
    ]},

    %% Only include new code
    {files, [
        "src",
        "include",
        "priv",
        "README.md",
        "LICENSE",
        "rebar.config"
    ]},

    %% Breaking changes notice
    {notes, "v0.5.0 is a complete rewrite with breaking changes from v0.4.x"}
]}.
```

### Task 8.7: Clean Up Documentation References

**Search for old references:**

```bash
# Find old module references in docs
grep -r "rebar3_lfe_" docs/ README.md CHANGELOG.md || echo "No references found"

# Find old plugin name references
grep -r "rebar3_lfe[^_]" docs/ README.md CHANGELOG.md | grep -v "r3lfe"
```

**File: `MIGRATION.md` (UPDATE)**

Ensure migration guide is clear about the rename:

```markdown
## Module Name Changes

**All modules renamed:**

| Old (0.4.x) | New (0.5.0) |
|-------------|-------------|
| `rebar3_lfe` | `r3lfe` |
| `rebar3_lfe_prv_compile` | `r3lfe_prv_compile` |
| `rebar3_lfe_prv_clean` | `r3lfe_prv_clean` |
| `rebar3_lfe_prv_repl` | `r3lfe_prv_repl` |
| `rebar3_lfe_utils` | Split into `r3lfe_config`, `r3lfe_paths`, etc. |
| `rebar3_lfe_package` | `r3lfe_package` |
| (etc.) | (all modules renamed) |

**Impact:**

- If you depend on internal modules (not recommended), update imports
- If you have custom hooks calling plugin modules, update references
- Configuration keys remain the same: `{lfe, [...]}`
```

### Task 8.8: Verify Clean Build

After removal:

```bash
# Clean everything
rebar3 clean -a
rm -rf _build

# Rebuild from scratch
rebar3 compile

# Should show NO rebar3_lfe_* modules
rebar3 compile | grep -i rebar3_lfe

# Check beam files
find _build -name "rebar3_lfe*.beam" && echo "ERROR: Old beams found!" || echo "✓ No old beams"
```

### Task 8.9: Update xref Ignores

**File: `rebar.config` (UPDATE)**

Remove any xref ignores for old modules:

```erlang
{xref_checks, [
    undefined_function_calls,
    undefined_functions,
    locals_not_used,
    deprecated_function_calls,
    deprecated_functions
]}.

%% Remove any old module ignores
{xref_ignores, [
    %% No rebar3_lfe_* modules to ignore anymore
]}.
```

### Task 8.10: Final Verification

**Create Test: `test/no_old_code_SUITE.erl`**

```erlang
-module(no_old_code_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    no_old_modules_loaded/1,
    no_old_source_files/1,
    no_old_beam_files/1,
    no_old_references/1
]).

all() ->
    [
        no_old_modules_loaded,
        no_old_source_files,
        no_old_beam_files,
        no_old_references
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

no_old_modules_loaded(_Config) ->
    %% Check that no old rebar3_lfe_* modules are loaded
    Loaded = [M || {M, _} <- code:all_loaded()],

    OldModules = lists:filter(
        fun(M) ->
            ModStr = atom_to_list(M),
            string:find(ModStr, "rebar3_lfe_") =/= nomatch
        end,
        Loaded
    ),

    ?assertEqual([], OldModules, "Old rebar3_lfe_* modules should not be loaded"),

    ok.

no_old_source_files(_Config) ->
    %% Check that no old source files exist
    OldSrcFiles = filelib:wildcard("src/rebar3_lfe*.erl"),

    ?assertEqual([], OldSrcFiles, "Old source files should be removed"),

    ok.

no_old_beam_files(_Config) ->
    %% Check that no old beam files exist in _build
    OldBeamFiles = filelib:wildcard("_build/**/rebar3_lfe*.beam"),

    ?assertEqual([], OldBeamFiles, "Old beam files should not exist"),

    ok.

no_old_references(_Config) ->
    %% Check that src files don't reference old modules
    SrcFiles = filelib:wildcard("src/*.erl"),

    BadRefs = lists:flatmap(
        fun(File) ->
            {ok, Content} = file:read_file(File),
            ContentStr = binary_to_list(Content),

            case string:find(ContentStr, "rebar3_lfe_") of
                nomatch ->
                    [];
                _ ->
                    [{File, "references old modules"}]
            end
        end,
        SrcFiles
    ),

    ?assertEqual([], BadRefs, "Source files should not reference old modules"),

    ok.
```

**Run the test:**

```bash
rebar3 ct --suite=test/no_old_code_SUITE
```

## Testing Instructions

### Pre-Removal Checklist

- [ ] All Phase 6 features working
- [ ] All tests passing
- [ ] Backup created
- [ ] Old files inventoried
- [ ] Replacements verified

### Removal Process

```bash
# 1. Inventory
./scripts/find_old_code.sh > old_files_inventory.txt

# 2. Verify replacements
./scripts/verify_replacements.sh

# 3. Verify tests pass
rebar3 ct
echo "Exit code: $?" >> pre_cleanup_tests.txt

# 4. Create backup
git checkout -b backup-old-code-$(date +%Y%m%d)
git checkout main

# 5. Remove old code
./scripts/remove_old_code.sh

# 6. Verify clean build
rebar3 clean -a
rebar3 compile

# 7. Run tests
rebar3 ct

# 8. Run new test suite
rebar3 ct --suite=test/no_old_code_SUITE

# 9. Check xref
rebar3 xref

# 10. Full check
make check
```

### Post-Removal Checklist

- [ ] All tests still pass
- [ ] No old modules loaded
- [ ] No old source files exist
- [ ] No old beam files exist
- [ ] Clean xref
- [ ] Clean dialyzer
- [ ] Documentation updated
- [ ] Ready to commit

## Expected Outcomes

At the end of Phase 8, you should have:

1. ✅ All old rebar3_lfe_* files removed
2. ✅ No references to old code
3. ✅ Clean build from scratch
4. ✅ All tests passing
5. ✅ Clean xref and dialyzer
6. ✅ Backup of old code preserved
7. ✅ Documentation updated
8. ✅ Test suite verifying no old code

### File Counts

**Before Phase 8:**

- ~15+ old rebar3_lfe_* files

**After Phase 8:**

- 0 old rebar3_lfe_* files
- Only r3lfe_* files remain

### Code Quality

- ✅ No dead code
- ✅ No module name conflicts
- ✅ Clear namespace (r3lfe)
- ✅ Maintainable codebase

## Commit Message

```
Remove old rebar3_lfe code (Phase 8)

Complete removal of legacy code from 0.4.x. All functionality has been
replaced by modern r3lfe_* modules.

Changes:
- Remove all rebar3_lfe_* source files
- Remove old rebar3_lfe.app.src
- Remove old header files
- Update .app.src to exclude old code
- Add test suite verifying no old code remains
- Update documentation removing old references

Verified:
- All tests pass
- Clean xref
- Clean dialyzer
- No old modules loaded
- No old files remain

Backup preserved in backup-old-code-* branch.
```

## Next Steps

After Phase 8:

- Proceed to Phase 7.1 (if not already done)
- Or proceed directly to release (if 7.1 complete)

## Notes for Claude Code

### Safety First

- **Always create backup before removal**
- **Verify tests pass before AND after**
- **Remove files one category at a time**
- **Check for references before deletion**

### Common Issues

**Old modules still loading:**

- Solution: `rebar3 clean -a` then rebuild

**References in documentation:**

- Solution: Search and update all docs

**Git tracking issues:**

- Solution: Use `git rm` instead of `rm`

### Verification Strategy

1. **Static verification:** File existence checks
2. **Build verification:** Clean build succeeds
3. **Runtime verification:** Tests pass
4. **Code verification:** Xref and dialyzer clean
5. **Test verification:** New test suite passes

### Preservation

Keep backup branch for:

- Reference if needed
- Rollback if issues found
- Historical record

### Final Check

Before committing:

```bash
# Everything compiles
rebar3 compile

# No old code
find . -name "rebar3_lfe*" | grep -v ".git"

# Tests pass
rebar3 ct

# Quality checks pass
rebar3 xref
rebar3 dialyzer
```
