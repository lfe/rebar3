# Rebar3 Plugin Developer API Reference

This document provides a comprehensive reference for rebar3 modules commonly used by plugin developers.

---

## rebar_api

Provides a generic API that can be used by plugin builders, wrapping `rebar.hrl` features and macros.

### rebar_api:abort/0
- **Description**: Interrupts program flow
- **Args**: None
- **Returns**: `no_return()` - throws an exception

### rebar_api:abort/2
- **Description**: Like `error/2`, except it also raises an exception to interrupt program flow
- **Args**: 
  - `Str :: string()` - format string
  - `Args :: list()` - format arguments
- **Returns**: `no_return()` - throws an exception

### rebar_api:console/2
- **Description**: Prints to the console, including a newline
- **Args**:
  - `Str :: string()` - format string
  - `Args :: list()` - format arguments
- **Returns**: `ok`

### rebar_api:debug/2
- **Description**: Logs with severity `debug`
- **Args**:
  - `Str :: string()` - format string
  - `Args :: list()` - format arguments
- **Returns**: `ok`

### rebar_api:info/2
- **Description**: Logs with severity `info`
- **Args**:
  - `Str :: string()` - format string
  - `Args :: list()` - format arguments
- **Returns**: `ok`

### rebar_api:warn/2
- **Description**: Logs with severity `warn`
- **Args**:
  - `Str :: string()` - format string
  - `Args :: list()` - format arguments
- **Returns**: `ok`

### rebar_api:error/2
- **Description**: Logs with severity `error`
- **Args**:
  - `Str :: string()` - format string
  - `Args :: list()` - format arguments
- **Returns**: `ok`

### rebar_api:expand_env_variable/3
- **Description**: Expands all references to an environment variable in a string
- **Args**:
  - `InStr :: string()` - input string
  - `VarName :: string()` - variable name
  - `RawVarValue :: term()` - variable value
- **Returns**: `string()` - expanded string

### rebar_api:get_arch/0
- **Description**: Returns the system architecture
- **Args**: None
- **Returns**: `string()` - e.g., "19.0.4-x86_64-unknown-linux-gnu-64"

### rebar_api:wordsize/0
- **Description**: Returns the size of a word on the system, as a string
- **Args**: None
- **Returns**: `string()`

### rebar_api:set_paths/2
- **Description**: Set code paths for plugins or deps
- **Args**:
  - `List :: [plugins | deps]` - targets to set paths for
  - `State :: rebar_state:t()` - rebar state
- **Returns**: `ok`

### rebar_api:unset_paths/2
- **Description**: Unsets code paths
- **Args**:
  - `List :: [plugins | deps]` - targets to unset paths for
  - `State :: rebar_state:t()` - rebar state
- **Returns**: `ok`

### rebar_api:add_deps_to_path/1
- **Description**: Add deps to the code path
- **Args**:
  - `State :: rebar_state:t()` - rebar state
- **Returns**: `ok`

### rebar_api:restore_code_path/1
- **Description**: Revert to only having the beams necessary for running rebar3 and plugins in the path
- **Args**:
  - `State :: rebar_state:t()` - rebar state
- **Returns**: `true | {error, term()}`

### rebar_api:processing_base_dir/1
- **Description**: Checks if the current working directory is the base directory for the project
- **Args**:
  - `State :: rebar_state:t()` - rebar state
- **Returns**: `boolean()`

### rebar_api:ssl_opts/1
- **Description**: Returns the SSL options adequate for the project based on its configuration
- **Args**:
  - `Url :: string() | binary()` - URL for the connection
- **Returns**: `[term()]` - SSL options proplist

---

## rebar_state

Manages rebar3 state throughout the build process.

### rebar_state:new/0
- **Description**: Creates a new, empty state
- **Args**: None
- **Returns**: `rebar_state:t()`
- **State Type**: Record with fields:
  - `dir :: file:name()`
  - `opts :: rebar_dict()`
  - `code_paths :: rebar_dict()`
  - `default :: rebar_dict()`
  - `escript_path :: undefined | file:filename_all()`
  - `lock :: list()`
  - `current_profiles :: [atom()]`
  - `namespace :: atom()`
  - `command_args :: list()`
  - `command_parsed_args :: {list(), list()}`
  - `current_app :: undefined | rebar_app_info:t()`
  - `project_apps :: [rebar_app_info:t()]`
  - `deps_to_build :: [rebar_app_info:t()]`
  - `all_plugin_deps :: [rebar_app_info:t()]`
  - `all_deps :: [rebar_app_info:t()]`
  - `compilers :: [module()]`
  - `project_builders :: [{rebar_app_info:project_type(), module()}]`
  - `resources :: list()`
  - `providers :: list()`
  - `allow_provider_overrides :: boolean()`

### rebar_state:new/1
- **Description**: Creates a new state with config
- **Args**:
  - `Config :: list()` - configuration terms
- **Returns**: `rebar_state:t()`

### rebar_state:get/2
- **Description**: Get a value from state opts
- **Args**:
  - `State :: rebar_state:t()`
  - `Key :: any()`
- **Returns**: `any()` - the value (fails if key doesn't exist)

### rebar_state:get/3
- **Description**: Get a value from state opts with a default
- **Args**:
  - `State :: rebar_state:t()`
  - `Key :: any()`
  - `Default :: any()`
- **Returns**: `any()` - the value or default

### rebar_state:set/3
- **Description**: Set a value in state opts
- **Args**:
  - `State :: rebar_state:t()`
  - `Key :: any()`
  - `Value :: any()`
- **Returns**: `rebar_state:t()`

### rebar_state:opts/1
- **Description**: Get the opts dictionary from state
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `rebar_dict()` - dictionary of options

### rebar_state:opts/2
- **Description**: Set the opts dictionary in state
- **Args**:
  - `State :: rebar_state:t()`
  - `Opts :: rebar_dict()`
- **Returns**: `rebar_state:t()`

### rebar_state:dir/1
- **Description**: Get the directory from state
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `file:name()`

### rebar_state:dir/2
- **Description**: Set the directory in state
- **Args**:
  - `State :: rebar_state:t()`
  - `Dir :: file:name()`
- **Returns**: `rebar_state:t()`

### rebar_state:project_apps/1
- **Description**: Get the list of project applications
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `[rebar_app_info:t()]`

### rebar_state:project_apps/2
- **Description**: Set the list of project applications or update a single app
- **Args**:
  - `State :: rebar_state:t()`
  - `NewApps :: [rebar_app_info:t()] | rebar_app_info:t()`
- **Returns**: `rebar_state:t()`

### rebar_state:all_deps/1
- **Description**: Get all dependencies
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `[rebar_app_info:t()]`

### rebar_state:all_deps/2
- **Description**: Set all dependencies
- **Args**:
  - `State :: rebar_state:t()`
  - `NewApps :: [rebar_app_info:t()]`
- **Returns**: `rebar_state:t()`

### rebar_state:current_profiles/1
- **Description**: Get the current profiles
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `[atom()]`

### rebar_state:apply_profiles/2
- **Description**: Apply profiles to the state
- **Args**:
  - `State :: rebar_state:t()`
  - `Profiles :: atom() | [atom()]`
- **Returns**: `rebar_state:t() | {error, term()}`

### rebar_state:command_args/1
- **Description**: Get the command arguments
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `list()`

### rebar_state:command_parsed_args/1
- **Description**: Get the parsed command arguments
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `{list(), list()}` - {parsed options, remaining args}

### rebar_state:providers/1
- **Description**: Get the list of providers
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `list()`

### rebar_state:resources/1
- **Description**: Get the list of resources
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `[{rebar_resource_v2:type(), module()}]`

### rebar_state:add_resource/2
- **Description**: Add a resource to the state
- **Args**:
  - `State :: rebar_state:t()`
  - `Resource :: {rebar_resource_v2:type(), module()}`
- **Returns**: `rebar_state:t()`

---

## rebar_app_info

Manages information about individual applications.

### rebar_app_info:new/0
- **Description**: Build a new, empty app info value
- **Args**: None
- **Returns**: `rebar_app_info:t()`

### rebar_app_info:new/1
- **Description**: Build a new app info value with only the app name set
- **Args**:
  - `AppName :: atom() | binary() | string()`
- **Returns**: `{ok, rebar_app_info:t()}`

### rebar_app_info:new/2
- **Description**: Build app info with name and version
- **Args**:
  - `AppName :: atom() | binary() | string()`
  - `Vsn :: app_vsn()` - version string, binary, or tuple like `{git, short}`
- **Returns**: `{ok, rebar_app_info:t()}`

### rebar_app_info:new/3
- **Description**: Build complete app info with name, version, and directory
- **Args**:
  - `AppName :: atom() | binary() | string()`
  - `Vsn :: app_vsn()`
  - `Dir :: file:name()`
- **Returns**: `{ok, rebar_app_info:t()}`

### rebar_app_info:name/1
- **Description**: Get the app name
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `binary()`

### rebar_app_info:name/2
- **Description**: Set the app name
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `AppName :: atom() | binary() | string()`
- **Returns**: `rebar_app_info:t()`

### rebar_app_info:opts/1
- **Description**: Get the dictionary of options for the app
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `rebar_dict()`

### rebar_app_info:opts/2
- **Description**: Set the dictionary of options for the app
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `Opts :: rebar_dict()`
- **Returns**: `rebar_app_info:t()`

### rebar_app_info:get/2
- **Description**: Look up a value in the app's options dictionary
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `Key :: term()`
- **Returns**: `term()` - fails if key doesn't exist

### rebar_app_info:get/3
- **Description**: Look up a value with a default
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `Key :: term()`
  - `Default :: term()`
- **Returns**: `term()`

### rebar_app_info:set/3
- **Description**: Set a value in the app's options dictionary
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `Key :: any()`
  - `Value :: any()`
- **Returns**: `rebar_app_info:t()`

### rebar_app_info:dir/1
- **Description**: Get the directory that contains the app
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `file:name()`

### rebar_app_info:dir/2
- **Description**: Set the directory that contains the app
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `Dir :: file:name()`
- **Returns**: `rebar_app_info:t()`

### rebar_app_info:out_dir/1
- **Description**: Get the directory where build artifacts should go
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `file:name()`

### rebar_app_info:out_dir/2
- **Description**: Set the output directory for build artifacts
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `OutDir :: file:name()`
- **Returns**: `rebar_app_info:t()`

### rebar_app_info:ebin_dir/1
- **Description**: Get the directory where ebin files should go
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `file:name()`

### rebar_app_info:vsn/1
- **Description**: Get the evaluated version of the app
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `app_vsn()`

### rebar_app_info:vsn/2
- **Description**: Set the evaluated version
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
  - `Vsn :: app_vsn()`
- **Returns**: `rebar_app_info:t()`

### rebar_app_info:applications/1
- **Description**: Get the list of applications the app depends on
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `list()`

### rebar_app_info:deps/1
- **Description**: Get the list of dependencies
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `list()`

### rebar_app_info:source/1
- **Description**: Get the source specification for the app
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `string() | tuple()`

### rebar_app_info:is_checkout/1
- **Description**: Check if the app is a checkout app
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `boolean()`

### rebar_app_info:valid/1
- **Description**: Check if the app is valid (built)
- **Args**:
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `boolean()`

### rebar_app_info:apply_overrides/2
- **Description**: Apply override rules to the app info
- **Args**:
  - `Overrides :: list()`
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `rebar_app_info:t()`

---

## rebar_app_discover

Utility functions for discovering apps and project layout.

### rebar_app_discover:do/2
- **Description**: Find all applications at the top level and their dependencies
- **Args**:
  - `State :: rebar_state:t()`
  - `LibDirs :: [file:filename()]`
- **Returns**: `rebar_state:t() | no_return()`

### rebar_app_discover:find_apps/2
- **Description**: For each directory passed, find all valid apps
- **Args**:
  - `LibDirs :: [file:filename_all()]`
  - `State :: rebar_state:t()`
- **Returns**: `[rebar_app_info:t()]`

### rebar_app_discover:find_apps/3
- **Description**: Find apps according to validity rule
- **Args**:
  - `LibDirs :: [file:filename_all()]`
  - `Validate :: valid | invalid | all`
  - `State :: rebar_state:t()`
- **Returns**: `[rebar_app_info:t()]`

### rebar_app_discover:find_app/3
- **Description**: Check if an app exists in a directory
- **Args**:
  - `AppDir :: file:filename_all()`
  - `Validate :: valid | invalid | all`
  - `State :: rebar_state:t()`
- **Returns**: `{true, rebar_app_info:t()} | false`

---

## rebar_utils

General utility functions for rebar3.

### rebar_utils:sh/2
- **Description**: Execute a shell command
- **Args**:
  - `Command :: string()`
  - `Options :: list()`
    - Options can include: `use_stdout`, `abort_on_error`, `return_on_error`, `{cd, Dir}`, `{env, Env}`
- **Returns**: `{ok, Output :: string()} | {error, {ExitCode :: integer(), Output :: string()}}`

### rebar_utils:abort/0
- **Description**: Abort execution
- **Args**: None
- **Returns**: `no_return()`

### rebar_utils:abort/2
- **Description**: Abort with formatted message
- **Args**:
  - `String :: string()`
  - `Args :: [term()]`
- **Returns**: `no_return()`

### rebar_utils:get_arch/0
- **Description**: Get system architecture string
- **Args**: None
- **Returns**: `string()`

### rebar_utils:find_files/2
- **Description**: Find files matching a regex
- **Args**:
  - `Dir :: file:filename()`
  - `Regex :: string()`
- **Returns**: `[file:filename()]`

### rebar_utils:find_files/3
- **Description**: Find files with recursion control
- **Args**:
  - `Dir :: file:filename()`
  - `Regex :: string()`
  - `Recursive :: boolean()`
- **Returns**: `[file:filename()]`

### rebar_utils:to_binary/1
- **Description**: Convert to binary
- **Args**:
  - `A :: atom() | string() | binary()`
- **Returns**: `binary()`

### rebar_utils:to_list/1
- **Description**: Convert to list
- **Args**:
  - `A :: atom() | binary() | integer() | string()`
- **Returns**: `string()`

### rebar_utils:to_atom/1
- **Description**: Convert to atom
- **Args**:
  - `B :: binary() | string() | atom()`
- **Returns**: `atom()`

### rebar_utils:expand_env_variable/3
- **Description**: Expand environment variable references in a string
- **Args**:
  - `InStr :: string()`
  - `VarName :: string()`
  - `RawVarValue :: term()`
- **Returns**: `string()`

### rebar_utils:update_code/1
- **Description**: Replace code paths and purge old modules
- **Args**:
  - `Paths :: [file:filename()]`
- **Returns**: `ok`

### rebar_utils:ssl_opts/1
- **Description**: Get SSL options for HTTPS requests
- **Args**:
  - `Url :: string() | binary()`
- **Returns**: `[term()]` - proplist of SSL options

---

## rebar_dir

Utility functions for directory and path handling.

### rebar_dir:base_dir/1
- **Description**: Get the base directory for build artifacts
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `file:filename_all()` - e.g., `_build/default/`

### rebar_dir:profile_dir/2
- **Description**: Get directory for specific profiles
- **Args**:
  - `Opts :: rebar_dict()`
  - `Profiles :: [atom()]`
- **Returns**: `file:filename_all()`

### rebar_dir:deps_dir/1
- **Description**: Get the dependencies directory
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `file:filename_all()`

### rebar_dir:root_dir/1
- **Description**: Get the project root directory
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `file:filename_all()`

### rebar_dir:checkouts_dir/1
- **Description**: Get the `_checkouts` directory location
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `file:filename_all()`

### rebar_dir:plugins_dir/1
- **Description**: Get the plugins directory
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `file:filename_all()`

### rebar_dir:lib_dirs/1
- **Description**: Get list of paths where project applications can be located
- **Args**:
  - `State :: rebar_state:t()`
- **Returns**: `[file:filename_all()]`

### rebar_dir:src_dirs/1
- **Description**: Get source directories
- **Args**:
  - `Opts :: rebar_dict()`
- **Returns**: `[file:filename_all()]`

### rebar_dir:src_dirs/2
- **Description**: Get source directories with defaults
- **Args**:
  - `Opts :: rebar_dict()`
  - `Default :: [file:filename_all()]`
- **Returns**: `[file:filename_all()]`

### rebar_dir:extra_src_dirs/1
- **Description**: Get extra source directories
- **Args**:
  - `Opts :: rebar_dict()`
- **Returns**: `[file:filename_all()]`

### rebar_dir:all_src_dirs/1
- **Description**: Get all source directories (src_dirs + extra_src_dirs)
- **Args**:
  - `Opts :: rebar_dict()`
- **Returns**: `[file:filename_all()]`

### rebar_dir:get_cwd/0
- **Description**: Get current working directory (cross-platform)
- **Args**: None
- **Returns**: `file:filename_all()`

### rebar_dir:make_relative_path/2
- **Description**: Make a target path relative to a source path
- **Args**:
  - `Source :: file:filename()`
  - `Target :: file:filename()`
- **Returns**: `file:filename()`

---

## rebar_file_utils

File and directory manipulation utilities.

### rebar_file_utils:rm_rf/1
- **Description**: Remove files and directories recursively
- **Args**:
  - `Target :: string()` - filename, directory, or wildcard
- **Returns**: `ok`

### rebar_file_utils:cp_r/2
- **Description**: Copy files and directories recursively
- **Args**:
  - `Sources :: [string()]`
  - `Dest :: file:filename()`
- **Returns**: `ok`

### rebar_file_utils:cp_r/3
- **Description**: Copy with options
- **Args**:
  - `Sources :: [string()]`
  - `Dest :: file:filename()`
  - `Options :: proplists:proplist()`
    - `{dereference, true|false}` - dereference symbolic links
- **Returns**: `ok`

### rebar_file_utils:mv/2
- **Description**: Move a file or directory
- **Args**:
  - `Source :: string()`
  - `Dest :: file:filename()`
- **Returns**: `ok | {error, term()}`

### rebar_file_utils:symlink_or_copy/2
- **Description**: Create symlink or copy if symlink fails
- **Args**:
  - `Source :: file:filename()`
  - `Target :: file:filename()`
- **Returns**: `ok | exists | {error, term()}`

### rebar_file_utils:canonical_path/1
- **Description**: Reduce filepath by removing `.` and `..`
- **Args**:
  - `Dir :: string()`
- **Returns**: `string()`

### rebar_file_utils:ensure_dir/1
- **Description**: Ensure a directory exists
- **Args**:
  - `Path :: file:name_all()`
- **Returns**: `ok | {error, file:posix()}`

### rebar_file_utils:try_consult/1
- **Description**: Try to consult a file, return empty list on error
- **Args**:
  - `File :: file:filename()`
- **Returns**: `[term()]`

---

## rebar_config

Configuration file handling.

### rebar_config:consult_root/0
- **Description**: Read the default config file at the top of a project
- **Args**: None
- **Returns**: `[any()]`

### rebar_config:consult/1
- **Description**: Read the default config file in a given directory
- **Args**:
  - `Dir :: file:name()`
- **Returns**: `[any()]`

### rebar_config:consult_file/1
- **Description**: Read a config file and verify format
- **Args**:
  - `File :: file:filename()`
- **Returns**: `[{_,_}]` - key-value list

### rebar_config:consult_lock_file/1
- **Description**: Read and parse the lock file
- **Args**:
  - `File :: file:filename()`
- **Returns**: `[any()]` - lock file entries

### rebar_config:write_lock_file/2
- **Description**: Write locks to a lock file
- **Args**:
  - `LockFile :: file:filename()`
  - `Locks :: [any()]`
- **Returns**: `ok | {error, term()}`

---

## rebar_opts

Options manipulation and merging.

### rebar_opts:get/2
- **Description**: Get an option value
- **Args**:
  - `Opts :: rebar_dict()`
  - `Key :: term()`
- **Returns**: `term()` - fails if key doesn't exist

### rebar_opts:get/3
- **Description**: Get an option value with default
- **Args**:
  - `Opts :: rebar_dict()`
  - `Key :: term()`
  - `Default :: term()`
- **Returns**: `term()`

### rebar_opts:set/3
- **Description**: Set an option value
- **Args**:
  - `Opts :: rebar_dict()`
  - `Key :: any()`
  - `Value :: any()`
- **Returns**: `rebar_dict()`

### rebar_opts:erl_opts/1
- **Description**: Get erlc options with defines applied
- **Args**:
  - `Opts :: rebar_dict()`
- **Returns**: `list()` - erlc options

### rebar_opts:apply_overrides/3
- **Description**: Apply overrides to options
- **Args**:
  - `Opts :: rebar_dict()`
  - `Name :: atom()` - app name
  - `Overrides :: list()`
- **Returns**: `rebar_dict()`

### rebar_opts:merge_opts/2
- **Description**: Merge two option dictionaries
- **Args**:
  - `NewOpts :: rebar_dict()`
  - `OldOpts :: rebar_dict()`
- **Returns**: `rebar_dict()`

---

## rebar_log

Logging functionality.

### rebar_log:init/2
- **Description**: Initialize the logging system
- **Args**:
  - `Caller :: command_line | api`
  - `Verbosity :: integer()`
- **Returns**: `ok`

### rebar_log:log/3
- **Description**: Log a message at a specific level
- **Args**:
  - `Level :: error | warn | info | debug | diagnostic`
  - `Str :: string()`
  - `Args :: [term()]`
- **Returns**: `ok`

### rebar_log:set_level/1
- **Description**: Set the log level
- **Args**:
  - `Level :: integer()`
- **Returns**: `ok`

### rebar_log:get_level/0
- **Description**: Get the current log level
- **Args**: None
- **Returns**: `integer()`

---

## rebar_compiler

Compiler abstraction and coordination.

### rebar_compiler:compile_all/2
- **Description**: Compile all source files using specified compilers
- **Args**:
  - `Compilers :: [module()]` - list of compiler modules
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `ok`

### rebar_compiler:needs_compile/3
- **Description**: Check if a source file needs recompilation
- **Args**:
  - `Source :: file:name_all()`
  - `OutExt :: string()` - output extension
  - `Mappings :: [{extension(), DirName}]`
- **Returns**: `boolean()`

### rebar_compiler:ok_tuple/2
- **Description**: Format successful compilation result
- **Args**:
  - `Source :: file:filename()`
  - `Ws :: [string()]` - warnings
- **Returns**: `{ok, [string()]}`

### rebar_compiler:error_tuple/4
- **Description**: Format error compilation result
- **Args**:
  - `Source :: file:filename()`
  - `Es :: [string()]` - errors
  - `Ws :: [string()]` - warnings
  - `Opts :: rebar_dict() | [{_,_}]`
- **Returns**: `{error, [string()], [string()]}`

---

## rebar_hooks

Hook execution for build phases.

### rebar_hooks:run_all_hooks/5
- **Description**: Run hooks for a specific command phase
- **Args**:
  - `Dir :: file:filename_all()`
  - `Type :: pre | post`
  - `Command :: atom() | {atom(), atom()} | string()`
  - `Providers :: [providers:t()]`
  - `State :: rebar_state:t()`
- **Returns**: `ok`

### rebar_hooks:run_all_hooks/6
- **Description**: Run all hooks including app-specific
- **Args**:
  - `Dir :: file:filename_all()`
  - `Type :: pre | post`
  - `Command :: atom() | {atom(), atom()} | string()`
  - `Providers :: [providers:t()]`
  - `AppInfo :: rebar_app_info:t()`
  - `State :: rebar_state:t()`
- **Returns**: `rebar_app_info:t()`

---

## rebar_plugins

Plugin management and loading.

### rebar_plugins:install/2
- **Description**: Install plugins for an app
- **Args**:
  - `State :: rebar_state:t()`
  - `AppInfo :: rebar_app_info:t()`
- **Returns**: `rebar_state:t()`

### rebar_plugins:handle_plugins/3
- **Description**: Handle plugins for a profile
- **Args**:
  - `Profile :: atom()`
  - `Plugins :: list()`
  - `State :: rebar_state:t()`
- **Returns**: `rebar_state:t()`

---

## rebar_paths

Code path manipulation.

### rebar_paths:set_paths/2
- **Description**: Set code paths for targets
- **Args**:
  - `Targets :: [deps | plugins | runtime]`
  - `State :: rebar_state:t()`
- **Returns**: `ok`

### rebar_paths:unset_paths/2
- **Description**: Remove code paths for targets
- **Args**:
  - `Targets :: [deps | plugins | runtime]`
  - `State :: rebar_state:t()`
- **Returns**: `ok`

---

## rebar_string

String compatibility and utility functions.

### rebar_string:join/2
- **Description**: Join strings with separator
- **Args**:
  - `List :: [string()]`
  - `Sep :: string()`
- **Returns**: `string()`

### rebar_string:split/2
- **Description**: Split string by pattern
- **Args**:
  - `Str :: string()`
  - `SearchPattern :: string()`
- **Returns**: `[string()]`

### rebar_string:trim/1
- **Description**: Trim whitespace from string
- **Args**:
  - `Str :: string()`
- **Returns**: `string()`

### rebar_string:uppercase/1
- **Description**: Convert to uppercase
- **Args**:
  - `Str :: string()`
- **Returns**: `string()`

### rebar_string:lowercase/1
- **Description**: Convert to lowercase
- **Args**:
  - `Str :: string()`
- **Returns**: `string()`

### rebar_string:consult/1
- **Description**: Parse string into list of terms
- **Args**:
  - `Str :: unicode:chardata()`
- **Returns**: `{error, term()} | [term()]`

---

## Custom Types Reference

### rebar_dict()
A dictionary type used throughout rebar3, implemented as `dict:dict()`.

### rebar_app_info:t()
An opaque type representing application information. Access via `rebar_app_info` module functions.

### rebar_state:t()
An opaque type representing rebar3's state. Access via `rebar_state` module functions.

### app_vsn()
Application version, can be:
- `binary()`
- `string()`
- `{git, short}` - short git hash
- `{git, long}` - long git hash

### project_type()
- `rebar3` - standard rebar3 project
- `mix` - Elixir Mix project
- `undefined`

---

## Common Patterns

### Getting Configuration
```erlang
%% Get from state
Config = rebar_state:get(State, my_key, DefaultValue),

%% Get from app info
AppConfig = rebar_app_info:get(AppInfo, my_key, DefaultValue),

%% Get from opts dict
Opts = rebar_state:opts(State),
Value = rebar_opts:get(Opts, my_key, DefaultValue).
```

### Working with Apps
```erlang
%% Get all project apps
Apps = rebar_state:project_apps(State),

%% Get all dependencies
Deps = rebar_state:all_deps(State),

%% Find specific app
{ok, AppInfo} = rebar_app_utils:find(<<"myapp">>, Apps).
```

### File Operations
```erlang
%% Get directories
BaseDir = rebar_dir:base_dir(State),
DepsDir = rebar_dir:deps_dir(State),
RootDir = rebar_dir:root_dir(State),

%% Copy files
ok = rebar_file_utils:cp_r(Sources, Dest),

%% Remove files
ok = rebar_file_utils:rm_rf(Path).
```

### Logging
```erlang
%% Log at different levels
rebar_api:debug("Debug message: ~p", [Data]),
rebar_api:info("Info message: ~p", [Data]),
rebar_api:warn("Warning: ~ts", [Message]),
rebar_api:error("Error: ~ts", [Message]).
```

### Running Shell Commands
```erlang
%% Execute command
{ok, Output} = rebar_utils:sh("ls -la", 
    [{cd, Dir}, {use_stdout, false}]).
```
