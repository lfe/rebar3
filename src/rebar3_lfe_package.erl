%%% This module contains the logic for working with faux packages in LFE.
%%%
%%% Essentially, this boils down to creating modules with dotted-notation
%%% prefixes in their names. This can be done in the same `src` directory
%%% as regular Erlang and LFE projects, or, if a developer prefers, it can
%%% be done using subdirectories. In the latter case a faux package name will
%%% be created using a combination of sub-directory names under the `src`
%%% directory and the file name itself. To be clear, in the end what is
%%% created is simply a module name, as there is no such thing in Erlang as
%%% a package.
%%%
%%% Some examples of how files and directories will be transformed in their
%%% final form as `.beam` files:
%%%
%%% ./src/my.package1.lfe             -> ebin/my.package1.beam
%%% ./src/my/package2.lfe             -> ebin/my.package2.beam
%%% ./src/my/other/package.lfe        -> ebin/my.other.package.beam
%%% ./src/my/really/deep/package1.lfe -> ebin/my.really.deep.package1.beam
%%% ./src/my/really/deep.package2.lfe -> ebin/my.really.deep.package2.beam
%%% ./src/my/really.deep.package3.lfe -> ebin/my.really.deep.package3.beam
%%% ./src/my.really.deep.package4.lfe -> ebin/my.really.deep.package4.beam
%%%
%%% So, in essence, this module is responsible for transforming a nested
%%% directory structure into a single file, as well as providing the functions
%%% necessary to perform any cleanup of intermediary, temporary files.
%%%
-module(rebar3_lfe_package).

-export([generate_sources/1,
         generate_source/1,
         files/1,
         copy/1,
         clean_sources/1,
         clean_source/1]).

-define(FILE_REGEX, ".*[le][fr][el]$").
-define(RECURSE, true).
-define(DONT_RECURSE, false).
-define(DEFAULT_FILE_LIST, []).

generate_sources(SourceDirs) ->
    AllFiles = [generate_source(Path) || Path <- SourceDirs],
    rebar_api:debug("All package files: ~p", [AllFiles]).

generate_source(Path) ->
    Files = files(filename:absname(Path)),
    rebar_api:debug("Package files for source dir ~p: ~p", [Path, Files]),
    copy(Files),
    Files.

package_to_source(SourcePath, Filepath) ->
    rebar_api:debug("SourcePath, Filepath: ~p, ~p", [SourcePath, Filepath]),
    [_ | Tail] = re:split(Filepath, "^" ++ SourcePath ++ "/"),
    rebar_api:debug("Tail: ~p", [Tail]),
    Replaced = re:replace(Tail, "/", ".", [global, {return, list}]),
    rebar_api:debug("Replaced: ~p", [Replaced]),
    filename:join([SourcePath, Replaced]).

files(SourcePath) ->
    %% This returns a list of maps for file names, where each map has
    %% 'package_file' and 'source_file' keys. The 'source_file' is the
    %% "flattened" module file copied from the 'package_file' (found in a
    %% subdirectory) where the filename has been assembled from the full path
    %% to the 'package_file'.
    AddFile = fun(Filepath, Acc) -> lists:append(Acc, [Filepath]) end,
    ModsOnly = filelib:fold_files(SourcePath, ?FILE_REGEX, ?DONT_RECURSE, AddFile, ?DEFAULT_FILE_LIST),
    AllFiles = filelib:fold_files(SourcePath, ?FILE_REGEX, ?RECURSE, AddFile, ?DEFAULT_FILE_LIST),
    PackageFiles = rebar3_lfe_utils:set_diff(AllFiles, ModsOnly),
    FileData = fun(Filepath) -> #{source_path => SourcePath,
                                  package_file => Filepath,
                                  source_file => package_to_source(SourcePath, Filepath)} end,
    PackageFileData = fun(Filepath, Acc) -> lists:append(Acc, [FileData(Filepath)]) end,
    lists:foldl(PackageFileData, ?DEFAULT_FILE_LIST, PackageFiles).

copy(Files) ->
    [file:copy(maps:get(package_file, File), maps:get(source_file, File)) || File <- Files].

clean_sources(SourceDirs) ->
    [clean_source(Path) || Path <- SourceDirs].

clean_source(SourcePath) ->
    %% This removes the temporary dotted-name LFE files copied to ./src from
    %% the subdirectories.
    rebar_api:debug("Cleaning up generated package source files for ~p...", [SourcePath]),
    Files = files(SourcePath),
    [file:delete(maps:get(source_file, File)) || File <- Files].
