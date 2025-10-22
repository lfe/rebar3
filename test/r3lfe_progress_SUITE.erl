-module(r3lfe_progress_SUITE).

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
    init_creates_progress_record/1,
    report_start_logs_file_count/1,
    report_file_increments_counter/1,
    report_file_calculates_percentage/1,
    report_complete_shows_duration/1,
    progress_with_zero_files/1,
    progress_with_large_file_count/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        init_creates_progress_record,
        report_start_logs_file_count,
        report_file_increments_counter,
        report_file_calculates_percentage,
        report_complete_shows_duration,
        progress_with_zero_files,
        progress_with_large_file_count
    ].

init_per_suite(Config) ->
    application:ensure_all_started(lfe),
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

init_creates_progress_record(_Config) ->
    Progress = r3lfe_progress:init(100),

    %% Verify record structure (we need to check fields are set)
    %% Progress is a record, check it's a tuple with expected size
    ?assert(is_tuple(Progress)),
    ?assertEqual(4, tuple_size(Progress)), % #progress{} has 4 elements
    ok.

report_start_logs_file_count(_Config) ->
    %% Create mock app info
    {ok, AppInfo} = rebar_app_info:new(test_app, "0.1.0", "/tmp"),

    %% Should not crash
    ok = r3lfe_progress:report_start(10, AppInfo),
    ok = r3lfe_progress:report_start(0, AppInfo),
    ok.

report_file_increments_counter(_Config) ->
    Progress1 = r3lfe_progress:init(10),

    %% Report first file
    Progress2 = r3lfe_progress:report_file(Progress1),

    %% Progress should be different (counter incremented)
    ?assertNotEqual(Progress1, Progress2),

    %% Report more files
    Progress3 = r3lfe_progress:report_file(Progress2),
    Progress4 = r3lfe_progress:report_file(Progress3),

    ?assertNotEqual(Progress2, Progress3),
    ?assertNotEqual(Progress3, Progress4),
    ok.

report_file_calculates_percentage(_Config) ->
    %% Test with small file count to verify percentage calculation
    Progress = r3lfe_progress:init(10),

    %% Report 5 files (50%)
    Progress1 = lists:foldl(
        fun(_, Acc) -> r3lfe_progress:report_file(Acc) end,
        Progress,
        lists:seq(1, 5)
    ),

    %% We can't directly inspect the record, but we verify no crashes
    %% and the function returns a valid progress record
    ?assert(is_tuple(Progress1)),
    ok.

report_complete_shows_duration(_Config) ->
    Progress = r3lfe_progress:init(5),

    %% Compile some files
    Progress1 = lists:foldl(
        fun(_, Acc) -> r3lfe_progress:report_file(Acc) end,
        Progress,
        lists:seq(1, 5)
    ),

    %% Report complete should not crash
    ok = r3lfe_progress:report_complete(Progress1),
    ok.

progress_with_zero_files(_Config) ->
    Progress = r3lfe_progress:init(0),

    %% Should handle edge case
    ok = r3lfe_progress:report_complete(Progress),
    ok.

progress_with_large_file_count(_Config) ->
    %% Test with large number
    Progress = r3lfe_progress:init(10000),

    %% Report some files
    Progress1 = lists:foldl(
        fun(_, Acc) -> r3lfe_progress:report_file(Acc) end,
        Progress,
        lists:seq(1, 100)
    ),

    %% Should handle without overflow
    ok = r3lfe_progress:report_complete(Progress1),
    ok.
