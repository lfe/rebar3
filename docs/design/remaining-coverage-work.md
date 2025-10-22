# Remaining Coverage Improvement Work

## Current Status
- **Total Coverage**: 87% (was 83%)
- **Target**: 90%+
- **Tests**: 288 passing (was 252)

## Completed Work
✅ r3lfe_prv_release: 8% → 50-60% (8 new tests added)
✅ r3lfe_prv_ltest: 25% → 48% (6 new tests added)
✅ r3lfe_prv_run_escript: 35% → 59% (15 new tests added - created suite from scratch)
✅ r3lfe_prv_run_release: 36% → 53% (15 new tests added)
✅ r3lfe_dependency_scanner: Added 8 tests
✅ r3lfe_config: Added 9 tests
✅ r3lfe_package: Added 5 tests
✅ r3lfe_paths: Added 4 tests

## Remaining Critical Priority Providers

### 1. ✅ r3lfe_prv_ltest (25% → 48%)
**File**: `src/r3lfe_prv_ltest.erl`
**Current Tests**: `test/r3lfe_prv_ltest_SUITE.erl` (13 tests)

**Completed**:
- ✅ Exported build_test_opts/1 and add_test_paths/1 for testing
- ✅ Test different listener types (ltest, eunit, unknown)
- ✅ Test suite, test, and verbose options
- ✅ Test path discovery with/without test directories
- ✅ Test multiple apps test path addition
- ✅ Test error formatting

**Note**: Testing `do/1` with actual ltest execution would require ltest dependency and is complex to mock. Current 48% coverage is excellent for a provider module.

### 2. ✅ r3lfe_prv_run_escript (35% → 59%)
**File**: `src/r3lfe_prv_run_escript.erl`
**Current Tests**: `test/r3lfe_prv_run_escript_SUITE.erl` (15 tests - created from scratch)

**Completed**:
- ✅ Created comprehensive test suite with 15 tests
- ✅ Exported find_escript/1, determine_escript_path/1, parse_args/1, build_command/2 for testing
- ✅ Fixed determine_escript_path/1 to handle binary app names
- ✅ Test provider initialization
- ✅ Test escript path finding from state and config
- ✅ Test argument parsing (empty, with separator, multiple args)
- ✅ Test command building with and without args
- ✅ Test error formatting for various error types

**Note**: Testing actual escript execution (run_escript/2, collect_output/1) requires complex port mocking and is difficult to test reliably. Current 59% coverage is excellent for this provider.

### 3. ✅ r3lfe_prv_run_release (36% → 53%)
**File**: `src/r3lfe_prv_run_release.erl`
**Current Tests**: `test/r3lfe_prv_run_release_SUITE.erl` (23 tests)

**Completed**:
- ✅ Enhanced existing test suite with 15 new tests
- ✅ Exported 7 helper functions for testing
- ✅ Fixed get_release_name/1 to handle binary app names
- ✅ Test release name extraction (from config, app, fallback)
- ✅ Test release output directory (default, custom, absolute)
- ✅ Test command extraction and validation
- ✅ Test command line building
- ✅ Test interactive command detection
- ✅ Test error formatting for all error types

**Note**: Testing actual release execution (run_release_command/2, execute_command/2, collect_output/1) requires complex port mocking and actual release structures. Current 53% coverage is excellent for this provider.

### 4. r3lfe_prv_escriptize (44% → Target: 60-70%)
**File**: `src/r3lfe_prv_escriptize.erl`
**Current Tests**: `test/r3lfe_prv_escriptize_SUITE.erl` (4 tests)

**Needed Tests**:
- Test `do/1` with escript generation
- Test main module detection from different sources
- Test escript file creation and permissions
- Test config merging (escript_opts)
- Test error when no main module found
- Test shebang and emulator args
- Test include_erts option

**Implementation Hints**:
- Test find_main_module/1 function
- Test build_escript_opts/2 function
- Test create_escript_file/3 function
- Verify generated escript is executable

### 5. r3lfe_prv_repl (45% → Target: 60-70%)
**File**: `src/r3lfe_prv_repl.erl`
**Current Tests**: `test/r3lfe_prv_repl_SUITE.erl` (10 tests)

**Needed Tests**:
- Test `do/1` with shell startup (difficult - may need mocking)
- Test app loading with different app lists
- Test load_apps/1 function
- Test start_shell/2 function
- Test error handling in shell startup
- Test banner display options

**Implementation Hints**:
- Test load_apps/1 function
- Mock shell:start_interactive/1 calls
- Test configuration merging
- Test app path setup

### 6. r3lfe_prv_run (51% → Target: 65-75%)
**File**: `src/r3lfe_prv_run.erl`
**Current Tests**: `test/r3lfe_prv_run_SUITE.erl` (3 tests)

**Needed Tests**:
- Test `do/1` with file execution
- Test finding LFE files with different patterns
- Test main function execution
- Test argument passing
- Test error when file not found
- Test error when main function fails

**Implementation Hints**:
- Test find_lfe_file/2 function
- Test execute_file/3 function
- Create test LFE files
- Test file:script/2 invocation

## Lower Priority (Will help but less impact)

### 7. r3lfe_paths (60% → Target: 80-85%)
- Add more edge case tests
- Test set_paths/2 and unset_paths/2 more thoroughly
- Test with_paths/2 error scenarios

### 8. r3lfe_compile_worker (68% → Target: 85-90%)
- Test more compilation error scenarios
- Test warning handling variations
- Test option building edge cases

### 9. r3lfe_prv_compile (66% → Target: 80-85%)
- Test `do/1` with actual compilation
- Test dependency ordering
- Test parallel compilation
- Test first_files handling

### 10. r3lfe_dependency_scanner (73% → Target: 85-90%)
- Add more resolve_include edge cases
- Test caching behavior
- Test circular dependency detection

## Testing Strategy

For each provider module:

1. **Read the source code** to identify untested functions
2. **Focus on do/1 function** - this is the main provider entry point
3. **Test error paths** - format_error/1 and all error returns
4. **Test helper functions** - export for testing if needed
5. **Mock external dependencies** when needed (e.g., shell, escript execution)
6. **Create realistic test fixtures** (apps, files, configs)

## Expected Impact

After completing priority providers (1-6):
- **Current**: 83%
- **After r3lfe_prv_release**: ~84-85%
- **After all 6 priority providers**: **~88-90%**
- **With lower priority improvements**: **90-92%**

## Implementation Order

Recommended order for maximum coverage gain:

1. ✅ **r3lfe_prv_release** (8% → 50-60%) - COMPLETED - Total: 84%
2. ✅ **r3lfe_prv_ltest** (25% → 48%) - COMPLETED - Total: 86%
3. ✅ **r3lfe_prv_run_escript** (35% → 59%) - COMPLETED - Total: 86%
4. ✅ **r3lfe_prv_run_release** (36% → 53%) - COMPLETED - Total: 87%
5. **r3lfe_prv_escriptize** (44% → 60-70%) - ~1% total impact
6. **r3lfe_prv_repl** (45% → 60-70%) - ~1% total impact
7. **r3lfe_prv_run** (51% → 65-75%) - ~0.5-1% total impact

## Notes

- Provider modules have inherently lower max coverage due to UI/external interaction code
- Getting providers to 50-70% is excellent given their nature
- Focus on testable logic paths, not UI/display code
- Some `do/1` functions may be hard to test without mocking - that's OK
- The goal is 90% overall, not 90% on every single module

## Next Steps

1. Pick the next module from the priority list
2. Read the source code to understand functions
3. Create/enhance test suite with new tests
4. Run tests: `rebar3 ct`
5. Check coverage: `rebar3 cover`
6. Commit progress
7. Repeat until 90% total coverage achieved
