-ifndef(R3LFE_HRL).
-define(R3LFE_HRL, true).

%%% Plugin namespace
-define(NAMESPACE, lfe).

%%% File patterns
-define(LFE_SRC_EXTENSION, ".lfe").
-define(BEAM_EXTENSION, ".beam").
-define(LFE_FILE_REGEX, ".*\\.lfe$").

%%% Default directories
-define(DEFAULT_SRC_DIR, "src").
-define(DEFAULT_INCLUDE_DIR, "include").
-define(DEFAULT_OUT_DIR, "ebin").
-define(DEFAULT_TEST_DIR, "test").

%%% Compiler options
-define(DEFAULT_LFE_OPTS, [
    return,
    report_errors,
    report_warnings
]).

%%% Error macros
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

%%% Logging helpers (use rebar_api, not io:format)
-define(DEBUG(Fmt, Args), rebar_api:debug(Fmt, Args)).
-define(INFO(Fmt, Args), rebar_api:info(Fmt, Args)).
-define(WARN(Fmt, Args), rebar_api:warn(Fmt, Args)).
-define(ERROR(Fmt, Args), rebar_api:error(Fmt, Args)).

%%% Type specifications for better dialyzer checks
-type compile_result() :: ok | {ok, [warning()]} | {error, [error()], [warning()]}.
-type error() :: {file:filename(), [{integer(), module(), term()}]}.
-type warning() :: {file:filename(), [{integer(), module(), term()}]}.

-endif.
