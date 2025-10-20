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
    %% (except rebar3_lfe which is the app, not a module)
    Loaded = [M || {M, _} <- code:all_loaded()],

    OldModules = lists:filter(
        fun(M) ->
            ModStr = atom_to_list(M),
            %% Find rebar3_lfe_ prefix (but exclude rebar3_lfe app itself)
            case string:find(ModStr, "rebar3_lfe_") of
                nomatch -> false;
                _ -> true
            end
        end,
        Loaded
    ),

    ?assertEqual([], OldModules, "Old rebar3_lfe_* modules should not be loaded"),

    ok.

no_old_source_files(_Config) ->
    %% Check that no old source files exist
    %% Only check for .erl files with rebar3_lfe_ prefix
    OldSrcFiles = filelib:wildcard("src/rebar3_lfe_*.erl"),

    ?assertEqual([], OldSrcFiles, "Old source files should be removed"),

    %% Also check for old header files
    OldHrlFiles = filelib:wildcard("src/rebar3_lfe*.hrl") ++
                  filelib:wildcard("include/rebar3_lfe*.hrl"),

    ?assertEqual([], OldHrlFiles, "Old header files should be removed"),

    ok.

no_old_beam_files(_Config) ->
    %% Check that no old beam files exist in _build
    %% Look for rebar3_lfe_* modules (but not rebar3_lfe.beam which is preserved)
    OldBeamFiles = filelib:wildcard("_build/**/rebar3_lfe_*.beam"),

    ?assertEqual([], OldBeamFiles, "Old beam files should not exist"),

    ok.

no_old_references(_Config) ->
    %% Check that src files don't reference old modules
    SrcFiles = filelib:wildcard("src/*.erl"),

    BadRefs = lists:flatmap(
        fun(File) ->
            {ok, Content} = file:read_file(File),
            ContentStr = binary_to_list(Content),

            %% Look for rebar3_lfe_ references (module calls)
            %% But exclude comments and string literals where possible
            Lines = string:split(ContentStr, "\n", all),

            BadLines = lists:filter(
                fun(Line) ->
                    %% Skip comment lines
                    case string:trim(Line, leading, " \t") of
                        "%" ++ _ -> false;
                        _ ->
                            %% Check for rebar3_lfe_ pattern
                            case string:find(Line, "rebar3_lfe_") of
                                nomatch -> false;
                                _ -> true
                            end
                    end
                end,
                Lines
            ),

            case BadLines of
                [] -> [];
                _ -> [{File, "references old modules"}]
            end
        end,
        SrcFiles
    ),

    ?assertEqual([], BadRefs, "Source files should not reference old modules"),

    ok.
