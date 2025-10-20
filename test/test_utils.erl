-module(test_utils).

%% Test utility exports
-export([
    create_temp_dir/0,
    create_temp_dir/1,
    cleanup_temp_dir/1,
    create_test_app/1,
    create_test_app/2,
    write_file/2,
    mock_app_info/1,
    mock_state/0
]).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Test Utilities
%%====================================================================

%% @doc Create a temporary directory for testing
create_temp_dir() ->
    create_temp_dir("rb3lfe_test").

create_temp_dir(Prefix) ->
    Rand = integer_to_list(erlang:unique_integer([positive])),
    TmpDir = case os:getenv("TMPDIR") of
        false -> "/tmp";
        TmpPath -> TmpPath
    end,
    Dir = filename:join([TmpDir, Prefix ++ "_" ++ Rand]),
    %% filelib:ensure_dir creates parent dirs, file:make_dir creates the dir itself
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok  %% Already exists from parallel test
    end,
    Dir.

%% @doc Clean up a temporary directory
cleanup_temp_dir(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} ->
            ct:pal("Warning: Failed to cleanup ~s: ~p", [Dir, Reason]),
            ok
    end.

%% @doc Create a minimal test application structure
create_test_app(Dir) ->
    create_test_app(Dir, "test_app").

create_test_app(Dir, AppName) ->
    %% Create directory structure
    SrcDir = filename:join(Dir, "src"),
    IncludeDir = filename:join(Dir, "include"),
    EbinDir = filename:join(Dir, "ebin"),

    %% filelib:ensure_dir creates parent dirs (in this case, SrcDir, IncludeDir, EbinDir)
    %% when given a path like "SrcDir/dummy", it creates SrcDir
    ok = filelib:ensure_dir(filename:join(SrcDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(IncludeDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),

    %% Create .app.src file
    AppSrc = io_lib:format(
        "{application, ~s, [~n"
        "  {description, \"Test Application\"},~n"
        "  {vsn, \"0.1.0\"},~n"
        "  {modules, []},~n"
        "  {registered, []},~n"
        "  {applications, [kernel, stdlib]}~n"
        "]}.~n",
        [AppName]
    ),
    write_file(filename:join(SrcDir, AppName ++ ".app.src"), AppSrc),

    %% Create rebar.config
    RebarConfig =
        "{erl_opts, [debug_info]}.\n"
        "{deps, [{lfe, \"2.2.0\"}]}.\n",
    write_file(filename:join(Dir, "rebar.config"), RebarConfig),

    #{
        dir => Dir,
        app_name => AppName,
        src_dir => SrcDir,
        include_dir => IncludeDir,
        ebin_dir => EbinDir
    }.

%% @doc Write content to a file
write_file(Path, Content) when is_list(Content) ->
    write_file(Path, list_to_binary(Content));
write_file(Path, Content) when is_binary(Content) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Content).

%% @doc Create a mock rebar_app_info structure for testing
mock_app_info(AppDir) ->
    %% This is a simplified mock - real tests should use rebar_app_info:new()
    #{
        dir => AppDir,
        ebin_dir => filename:join(AppDir, "ebin"),
        opts => rebar_opts:new()
    }.

%% @doc Create a mock rebar_state for testing
mock_state() ->
    rebar_state:new().
