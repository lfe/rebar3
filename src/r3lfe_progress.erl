-module(r3lfe_progress).

%% API exports
-export([
    init/1,
    report_start/2,
    report_file/1,
    report_complete/1
]).

-include_lib("rebar3_lfe/include/r3lfe.hrl").

-record(progress, {
    total :: integer(),
    compiled :: integer(),
    start_time :: integer()
}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize progress reporting
-spec init(integer()) -> #progress{}.
init(TotalFiles) ->
    #progress{
        total = TotalFiles,
        compiled = 0,
        start_time = erlang:monotonic_time(millisecond)
    }.

%% @doc Report start of compilation
-spec report_start(integer(), rebar_app_info:t()) -> ok.
report_start(FileCount, AppInfo) ->
    AppName = rebar_app_info:name(AppInfo),

    if
        FileCount > 0 ->
            ?INFO("Compiling ~p LFE files in ~s", [FileCount, AppName]);
        true ->
            ?DEBUG("No LFE files to compile in ~s", [AppName])
    end,

    ok.

%% @doc Report compilation of a single file
-spec report_file(#progress{}) -> #progress{}.
report_file(Progress = #progress{total = Total, compiled = Compiled}) ->
    NewCompiled = Compiled + 1,

    %% Report every 10% or for small projects, every file
    ShouldReport = (Total =< 10) orelse (NewCompiled rem max(1, Total div 10) == 0),

    if
        ShouldReport ->
            Percent = (NewCompiled * 100) div Total,
            ?DEBUG("Progress: ~p/~p (~p%)", [NewCompiled, Total, Percent]);
        true ->
            ok
    end,

    Progress#progress{compiled = NewCompiled}.

%% @doc Report completion
-spec report_complete(#progress{}) -> ok.
report_complete(#progress{total = Total, compiled = Compiled, start_time = StartTime}) ->
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000,

    if
        Compiled > 0 ->
            ?INFO("Compiled ~p files in ~.2fs", [Compiled, Duration]);
        Total > 0 ->
            ?DEBUG("All ~p files up to date", [Total]);
        true ->
            ok
    end,

    ok.
