# Implementation Prompt: Fix and Enhance `rebar3 lfe versions` Command

## Context

You are working on an Erlang/LFE project that uses rebar3 as its build tool. The project includes a provider plugin called `r3lfe_prv_versions` that implements the `rebar3 lfe versions` command. This command displays version information about the project's applications, languages, and build tools.

The source code is in `src/r3lfe_prv_versions.erl` and tests are in `test/r3lfe_prv_versions_SUITE.erl`.

## Current Problems

The current implementation has several issues:

1. **Empty sections displayed**: When there are no project applications, the "=== Project Applications ===" header is still displayed with no content underneath
2. **LFE version not detected**: The LFE version shows as "unknown" even though LFE is installed and running
3. **Incorrect plugin name**: The code references `r3lfe` but the actual plugin name is `rebar3_lfe`
4. **Missing rebar3_hex**: The `rebar3_hex` plugin should be included in the Build Tools section if it exists in the project's plugins

## Required Fixes

### Fix 1: Conditional Header Display

Do not display section headers when the section would be empty. Specifically:

- Only display "=== Project Applications ===" if there are project applications to show
- Only display "=== Dependencies ===" if there are dependencies OTHER than LFE (see Enhancement 1)
- Only display "=== Plugins ===" if there are plugins OTHER than `rebar3_lfe` and `rebar3_hex` (see Enhancement 2)

### Fix 2: LFE Version Detection

The LFE version is showing as "unknown". This needs to be fixed:

- **CRITICAL**: Before attempting to get the version of ANY application (LFE, plugins, dependencies), you MUST ensure the application is loaded using `application:load/1` or `application:ensure_loaded/1`
- After ensuring the application is loaded, use `application:get_key(lfe, vsn)` to retrieve the version
- If the application cannot be loaded or the version cannot be retrieved, then return "unknown"

### Fix 3: Correct Plugin Name

Change the reference from `r3lfe` to `rebar3_lfe`:

- In the `get_tool_versions/0` function, change the tuple from `{r3lfe, get_version(r3lfe)}` to `{rebar3_lfe, get_version(rebar3_lfe)}`
- Remember to ensure the application is loaded before getting its version

### Fix 4: Include rebar3_hex

Add `rebar3_hex` to the Build Tools section if it's available:

- Check if `rebar3_hex` exists in the project's plugins or in any profile's plugins
- If found, include it in the Build Tools section with its version
- Ensure the application is loaded before attempting to get its version
- Use `application:get_key(rebar3_hex, vsn)` to get the version

## Required Enhancements

### Enhancement 1: Dependencies Section

Add a new "=== Dependencies ===" section that displays all project dependencies except LFE (since LFE is already shown in the Languages section).

**Implementation Requirements**:

1. **Gathering Dependencies**: You need to extract ALL dependencies from the rebar3 state, including dependencies from all profiles. Based on research, there doesn't appear to be a single rebar3 API function that returns dependencies from all profiles with profile information preserved. Therefore:

   - Create a utility function that extracts dependencies from the rebar3 state
   - This function should be placed in an appropriate utility module (likely `src/r3lfe_util.erl` or similar) and exported
   - The function signature should be: `get_all_deps_with_profiles(State) -> [#{name => atom(), version => string(), profile => atom()}]`
   - Implementation approach:
     - Get the top-level dependencies using `rebar_state:get(State, deps, [])`
     - Get the profiles configuration using `rebar_state:get(State, profiles, [])`
     - For each profile in the profiles list, extract its `deps` configuration
     - For top-level deps, use profile name `default`
     - For each dependency, extract its name and version
     - Ensure each application is loaded before getting its version
     - Build a list of maps with keys: `name`, `version`, `profile`

2. **Display Requirements**:
   - Filter out LFE from the dependencies list (it's shown in Languages section)
   - If the resulting list is empty, DO NOT display the "=== Dependencies ===" header
   - Sort dependencies alphabetically by name
   - Display format: `name                 version` for default profile deps
   - Display format: `name                 version (profile)` for non-default profile deps
   - Use the same formatting as other sections: `io:format("  ~-20s ~s~n", [Name, Vsn])` for default profile
   - For non-default profiles: `io:format("  ~-20s ~s (~s)~n", [Name, Vsn, Profile])`
   - Only show the profile name if it's NOT "default" (no empty parentheses)

### Enhancement 2: Plugins Section

Add a new "=== Plugins ===" section that displays all project plugins except `rebar3_lfe` and `rebar3_hex` (these are shown in Build Tools).

**Implementation Requirements**:

1. **Gathering Plugins**: You need to extract ALL plugins from the rebar3 state, including plugins from all profiles. Based on research, plugins can be specified as:
   - Top-level `{plugins, [...]}`
   - Top-level `{project_plugins, [...]}`
   - Profile-specific `{plugins, [...]}`
   - Profile-specific `{project_plugins, [...]}`

   Create a utility function that extracts plugins:
   - Function signature: `get_all_plugins_with_profiles(State) -> [#{name => atom(), version => string(), profile => atom()}]`
   - This should be in the same utility module as `get_all_deps_with_profiles/1`
   - Implementation approach:
     - Get top-level plugins using `rebar_state:get(State, plugins, [])`
     - Get top-level project_plugins using `rebar_state:get(State, project_plugins, [])`
     - Get profiles configuration using `rebar_state:get(State, profiles, [])`
     - For each profile, extract both `plugins` and `project_plugins`
     - For top-level plugins/project_plugins, use profile name `default`
     - For each plugin, extract its name
     - Ensure each plugin application is loaded before getting its version
     - Use `application:get_key(PluginName, vsn)` to get version
     - Build a list of maps with keys: `name`, `version`, `profile`

2. **Display Requirements**:
   - Filter out `rebar3_lfe` and `rebar3_hex` from the plugins list (they're in Build Tools)
   - If the resulting list is empty, DO NOT display the "=== Plugins ===" header
   - Sort plugins alphabetically by name
   - Display format: same as dependencies
   - Use `io:format("  ~-20s ~s~n", [Name, Vsn])` for default profile
   - For non-default profiles: `io:format("  ~-20s ~s (~s)~n", [Name, Vsn, Profile])`
   - Only show the profile name if it's NOT "default"

## Implementation Details

### Module Structure Changes

1. **Create or update `src/r3lfe_util.erl`**:
   - Export `get_all_deps_with_profiles/1`
   - Export `get_all_plugins_with_profiles/1`
   - These are general-purpose utility functions that may be useful elsewhere

2. **Update `src/r3lfe_prv_versions.erl`**:
   - Import the utility functions from `r3lfe_util`
   - Update `do/1` to gather dependencies and plugins information
   - Update `display_versions/1` to accept and display the new sections
   - Update `get_version/1` to ensure applications are loaded
   - Update `get_tool_versions/0` to fix the plugin names and include `rebar3_hex`

### Application Loading Strategy

**CRITICAL**: Before calling `application:get_key(App, vsn)` for ANY application:

```erlang
ensure_app_loaded(AppName) ->
    case application:load(AppName) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        _ -> error
    end.

get_version(App) ->
    case ensure_app_loaded(App) of
        ok ->
            case application:get_key(App, vsn) of
                {ok, Vsn} -> Vsn;
                undefined -> "unknown"
            end;
        error ->
            "unknown"
    end.
```

### Handling Plugin/Dependency Specifications

Plugins and dependencies can be specified in multiple formats:

- `plugin_name` (atom only)
- `{plugin_name, "version"}` (tuple with version string)
- `{plugin_name, {git, ...}}` (tuple with git source)
- `{plugin_name, {pkg, package_name}}` (tuple with package)
- etc.

Your extraction functions must handle all these formats and extract just the plugin/dependency name (the first element if it's a tuple, or the atom itself if it's just an atom).

### Version Detection for Plugins and Dependencies

For plugins and dependencies from sources like git or hex:

1. First ensure the application is loaded
2. Try to get the version using `application:get_key(Name, vsn)`
3. If that fails, you may need to look at the rebar3 lock file or the app info
4. For dependencies, you can also use `rebar_app_info` functions if you have app info structures

## Testing Requirements

### Update `test/r3lfe_prv_versions_SUITE.erl`

Add comprehensive test cases:

1. **Test for conditional header display**:
   - Test that empty project apps section doesn't show header
   - Test that empty dependencies section doesn't show header (after filtering LFE)
   - Test that empty plugins section doesn't show header (after filtering rebar3_lfe and rebar3_hex)

2. **Test for LFE version detection**:
   - Test that `get_version(lfe)` returns a valid version string (not "unknown")
   - Test that the application is loaded before checking version

3. **Test for utility functions**:
   - `test/r3lfe_util_SUITE.erl` - Create a new test suite for utility functions
   - Test `get_all_deps_with_profiles/1` with various state configurations
   - Test `get_all_plugins_with_profiles/1` with various state configurations
   - Test with multiple profiles
   - Test with empty deps/plugins lists
   - Test with deps/plugins in both top-level and profile-specific configs

4. **Test for dependencies section**:
   - Test with no dependencies (should not show header)
   - Test with only LFE as dependency (should not show header)
   - Test with multiple dependencies including LFE (should filter LFE)
   - Test with dependencies from different profiles
   - Test alphabetical sorting
   - Test profile display (should not show for default profile)

5. **Test for plugins section**:
   - Test with no plugins (should not show header)
   - Test with only rebar3_lfe and rebar3_hex (should not show header)
   - Test with additional plugins
   - Test with plugins from different profiles
   - Test alphabetical sorting
   - Test profile display

6. **Test for Build Tools section**:
   - Test that rebar3_lfe appears (not r3lfe)
   - Test that rebar3_hex appears if it's in the project
   - Test version detection for both

7. **Integration tests**:
   - Test the complete output format
   - Test with a realistic rebar.config that includes multiple profiles, deps, and plugins
   - Verify the order of sections: Languages, Build Tools, Dependencies, Plugins, Applications

## Expected Output Format

After implementation, the output should look like this:

```
=== Languages ===
  lfe                  2.1.5
  erlang               28
  erts                 16.0

=== Build Tools ===
  rebar3               3.25.0
  rebar3_hex           7.0.8
  rebar3_lfe           0.5.0

=== Dependencies ===
  cowboy               2.9.0
  jsx                  3.1.0
  meck                 0.9.2 (test)
  proper               1.4.0 (test)

=== Plugins ===
  rebar3_format        1.3.0
  rebar3_lint          3.2.5
  rebar3_proper        0.12.1 (test)

=== Project Applications ===
  myapp                1.0.0

```

Or if there are no project apps, no extra dependencies, and no extra plugins:

```
=== Languages ===
  lfe                  2.1.5
  erlang               28
  erts                 16.0

=== Build Tools ===
  rebar3               3.25.0
  rebar3_hex           7.0.8
  rebar3_lfe           0.5.0
```

## Code Quality Requirements

1. **Maintain existing code style**: Follow the patterns established in the codebase
2. **Add proper type specs**: All new functions should have `-spec` declarations
3. **Add documentation**: Add function documentation comments
4. **Error handling**: Handle edge cases gracefully (missing apps, malformed configs, etc.)
5. **Export test functions**: Make sure utility functions are exported when compiled with `-DTEST`
6. **Update `-ifdef(TEST)` exports**: Add new test-only exports to the appropriate sections

## Rebar3 API Reference

Key functions from the provided rebar3 API reference:

- `rebar_state:get(State, Key, Default)` - Get configuration value from state
- `rebar_state:project_apps(State)` - Get list of project applications
- `rebar_state:all_deps(State)` - Get all dependencies (but may not include profile info)
- `rebar_app_info:name(AppInfo)` - Get application name
- `rebar_app_info:original_vsn(AppInfo)` - Get application version

## Implementation Order

Implement in this order for maximum clarity and testability:

1. Create `src/r3lfe_util.erl` with utility functions
2. Update `get_version/1` to load applications first (Fix 2)
3. Fix the plugin name from `r3lfe` to `rebar3_lfe` (Fix 3)
4. Add `rebar3_hex` to Build Tools (Fix 4)
5. Implement dependencies gathering and display (Enhancement 1)
6. Implement plugins gathering and display (Enhancement 2)
7. Update `display_versions/1` to conditionally show headers (Fix 1)
8. Add all test cases
9. Test thoroughly with various configurations

## Success Criteria

The implementation is complete when:

1. All fixes are implemented and working correctly
2. Both enhancements are implemented and working correctly
3. All new test cases pass
4. All existing test cases still pass
5. The output matches the expected format shown above
6. Empty sections do not display headers
7. LFE version is detected correctly (not "unknown")
8. Dependencies and plugins from all profiles are displayed correctly
9. Profile names are shown only for non-default profiles
10. Items are sorted alphabetically within each section
