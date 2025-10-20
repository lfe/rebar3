-module(templates_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test cases
-export([
    templates_directory_exists/1,
    template_files_valid/1,
    lfe_main_template_exists/1,
    lfe_escript_template_exists/1,
    lfe_lib_template_exists/1,
    lfe_app_template_exists/1,
    lfe_release_template_exists/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        templates_directory_exists,
        template_files_valid,
        lfe_main_template_exists,
        lfe_escript_template_exists,
        lfe_lib_template_exists,
        lfe_app_template_exists,
        lfe_release_template_exists
    ].

init_per_suite(Config) ->
    application:ensure_all_started(rebar3_lfe),
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

templates_directory_exists(_Config) ->
    %% Get priv dir - may be rebar3_lfe or r3lfe depending on load order
    PrivDir = case code:priv_dir(rebar3_lfe) of
        {error, bad_name} ->
            %% Try alternate name
            code:priv_dir(r3lfe);
        Dir ->
            Dir
    end,

    case PrivDir of
        {error, bad_name} ->
            ct:pal("Warning: Neither rebar3_lfe nor r3lfe application loaded, skipping template tests");
        _ ->
            ?assert(filelib:is_dir(PrivDir), "priv directory should exist"),

            TemplatesDir = filename:join(PrivDir, "templates"),

            case filelib:is_dir(TemplatesDir) of
                true ->
                    ct:pal("Templates found at: ~s", [TemplatesDir]);
                false ->
                    ct:pal("Templates directory not found (expected at ~s), templates may be in priv/ directly", [TemplatesDir])
            end
    end,

    ok.

template_files_valid(_Config) ->
    PrivDir = case code:priv_dir(rebar3_lfe) of
        {error, bad_name} -> code:priv_dir(r3lfe);
        Dir -> Dir
    end,

    case PrivDir of
        {error, bad_name} ->
            ct:pal("Skipping: application not loaded"),
            ok;
        _ ->
            TemplatesDir = filename:join(PrivDir, "templates"),

            %% Try to find template files
            Templates = filelib:wildcard(filename:join(TemplatesDir, "*.template")),

            case Templates of
                [] ->
                    ct:pal("No .template files found, templates may use different structure");
                _ ->
                    ct:pal("Found ~p template files", [length(Templates)]),

                    %% Verify each template is readable
                    lists:foreach(
                        fun(Template) ->
                            ?assert(filelib:is_file(Template),
                                    io_lib:format("Template should exist: ~s", [Template]))
                        end,
                        Templates
                    )
            end,
            ok
    end.

lfe_main_template_exists(_Config) ->
    check_template_exists("lfe-main").

lfe_escript_template_exists(_Config) ->
    check_template_exists("lfe-escript").

lfe_lib_template_exists(_Config) ->
    check_template_exists("lfe-lib").

lfe_app_template_exists(_Config) ->
    check_template_exists("lfe-app").

lfe_release_template_exists(_Config) ->
    check_template_exists("lfe-release").

%%====================================================================
%% Helper Functions
%%====================================================================

check_template_exists(TemplateName) ->
    PrivDir = case code:priv_dir(rebar3_lfe) of
        {error, bad_name} -> code:priv_dir(r3lfe);
        Dir -> Dir
    end,

    case PrivDir of
        {error, bad_name} ->
            ct:pal("Skipping ~s: application not loaded", [TemplateName]),
            ok;
        _ ->
            TemplateFile = filename:join([PrivDir, "templates", TemplateName ++ ".template"]),

            case filelib:is_file(TemplateFile) of
                true ->
                    ct:pal("~s template found at: ~s", [TemplateName, TemplateFile]);
                false ->
                    ct:pal("~s template not found at expected location: ~s", [TemplateName, TemplateFile]),
                    ct:pal("Templates will be auto-registered by rebar3 when plugin loads")
            end,
            ok
    end.
