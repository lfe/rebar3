-module(rb3lfe_prv_confabulate).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include_lib("rebar3_lfe/include/rb3lfe.hrl").

-define(PROVIDER, confabulate).
-define(DEPS, []).

%%====================================================================
%% Provider API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Description = "Convert LFE data files to Erlang data files",

    Opts = [
        {input, $i, "input", string,
         "Input LFE file to convert"},
        {output, $o, "output", string,
         "Output Erlang file (defaults to input.erl)"},
        {force, $f, "force", boolean,
         "Overwrite output file if it exists"}
    ],

    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 lfe confabulate --input data.lfe"},
        {opts, Opts},
        {short_desc, Description},
        {desc, info(Description)}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?DEBUG("LFE confabulate provider starting", []),

    rebar_paths:set_paths([deps, plugins], State),

    try
        %% Get options
        {Opts, _} = rebar_state:command_parsed_args(State),

        InputFile = proplists:get_value(input, Opts),
        OutputFile = determine_output_file(InputFile, Opts),
        Force = proplists:get_value(force, Opts, false),

        case InputFile of
            undefined ->
                {error, format_error(no_input_file)};
            _ ->
                %% Convert file
                ?INFO("Converting ~s to ~s", [InputFile, OutputFile]),

                case convert_file(InputFile, OutputFile, Force) of
                    ok ->
                        ?INFO("Conversion successful!", []),
                        {ok, State};
                    {error, Reason} ->
                        {error, format_error(Reason)}
                end
        end
    catch
        throw:{error, ErrorReason} ->
            {error, format_error(ErrorReason)};
        error:ErrorReason:Stack ->
            ?ERROR("Confabulate failed: ~p", [ErrorReason]),
            ?DEBUG("Stack trace: ~p", [Stack]),
            {error, format_error({confabulate_error, ErrorReason})}
    end.

-spec format_error(term()) -> iolist().
format_error(no_input_file) ->
    "No input file specified. Use --input option:\n"
    "  rebar3 lfe confabulate --input data.lfe";
format_error({input_not_found, File}) ->
    io_lib:format("Input file not found: ~s", [File]);
format_error({output_exists, File}) ->
    io_lib:format(
        "Output file already exists: ~s~n"
        "Use --force to overwrite",
        [File]
    );
format_error({parse_error, File, Reason}) ->
    io_lib:format("Failed to parse ~s: ~p", [File, Reason]);
format_error({write_error, File, Reason}) ->
    io_lib:format("Failed to write ~s: ~p", [File, Reason]);
format_error({confabulate_error, Reason}) ->
    io_lib:format("Conversion failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Determine output file path
-spec determine_output_file(file:filename() | undefined, proplists:proplist()) ->
    file:filename() | undefined.
determine_output_file(undefined, _Opts) ->
    undefined;
determine_output_file(InputFile, Opts) ->
    case proplists:get_value(output, Opts) of
        undefined ->
            %% Default: replace .lfe with .erl
            filename:rootname(InputFile, ".lfe") ++ ".erl";
        OutputFile ->
            OutputFile
    end.

%% @doc Convert a single LFE file to Erlang format
-spec convert_file(file:filename(), file:filename(), boolean()) ->
    ok | {error, term()}.
convert_file(InputFile, OutputFile, Force) ->
    %% Validate input exists
    case filelib:is_file(InputFile) of
        false ->
            {error, {input_not_found, InputFile}};
        true ->
            %% Check output doesn't exist (unless force)
            case filelib:is_file(OutputFile) andalso not Force of
                true ->
                    {error, {output_exists, OutputFile}};
                false ->
                    do_conversion(InputFile, OutputFile)
            end
    end.

%% @doc Perform the actual conversion
-spec do_conversion(file:filename(), file:filename()) -> ok | {error, term()}.
do_conversion(InputFile, OutputFile) ->
    ?DEBUG("Parsing LFE file: ~s", [InputFile]),

    %% Parse LFE file
    case lfe_io:parse_file(InputFile) of
        {ok, Forms} ->
            ?DEBUG("Parsed ~p forms", [length(Forms)]),

            %% Convert and write
            write_erlang_file(OutputFile, Forms);

        {error, Reason} ->
            {error, {parse_error, InputFile, Reason}}
    end.

%% @doc Write forms as Erlang data
-spec write_erlang_file(file:filename(), [term()]) -> ok | {error, term()}.
write_erlang_file(OutputFile, Forms) ->
    try
        %% Delete existing file if present
        file:delete(OutputFile),

        %% Flatten and write each form
        FlatForms = flatten_forms(Forms),

        lists:foreach(
            fun(Form) ->
                ok = append_form(OutputFile, Form)
            end,
            FlatForms
        ),

        ok
    catch
        error:Reason ->
            {error, {write_error, OutputFile, Reason}}
    end.

%% @doc Flatten forms - if the parsed result is a single list of forms,
%% unwrap it so each element is written as a separate term
-spec flatten_forms([term()]) -> [term()].
flatten_forms([{Form, _Line}]) when is_list(Form) ->
    %% Single form that is a list - unwrap it
    Form;
flatten_forms([{Form, _Line}]) ->
    %% Single form that is not a list - keep as is
    [Form];
flatten_forms([Form]) when is_list(Form) ->
    %% Single form without line info that is a list - unwrap it
    Form;
flatten_forms([Form]) ->
    %% Single form without line info - keep as is
    [Form];
flatten_forms(Forms) when is_list(Forms) ->
    %% Multiple forms - strip line info
    [case F of
        {Form, _Line} -> Form;
        Form -> Form
     end || F <- Forms];
flatten_forms(Other) ->
    [Other].

%% @doc Append a single form to the output file
-spec append_form(file:filename(), term()) -> ok.
append_form(OutputFile, {Form, _Line}) ->
    append_form(OutputFile, Form);
append_form(OutputFile, Form) ->
    %% Format as Erlang term
    FormattedTerm = io_lib:format("~p.~n", [Form]),

    %% Append to file
    case file:write_file(OutputFile, FormattedTerm, [append]) of
        ok ->
            ok;
        {error, Reason} ->
            throw({error, {write_error, OutputFile, Reason}})
    end.

-spec info(string()) -> iolist().
info(Description) ->
    io_lib:format(
        "~n~s~n"
        "~n"
        "Converts LFE data files to Erlang data files. This is useful for:~n"
        "  - Converting LFE configuration to Erlang format~n"
        "  - Generating Erlang test data from LFE~n"
        "  - Data exchange between LFE and Erlang codebases~n"
        "  - Migration between languages~n"
        "~n"
        "The conversion parses LFE data structures and writes them as~n"
        "Erlang terms, one per line.~n"
        "~n"
        "Input Format (LFE):~n"
        "  ;; data.lfe~n"
        "  ((tuple 'person \"Alice\" 30)~n"
        "   (tuple 'person \"Bob\" 25))~n"
        "~n"
        "Output Format (Erlang):~n"
        "  %% data.erl~n"
        "  {person,\"Alice\",30}.~n"
        "  {person,\"Bob\",25}.~n"
        "~n"
        "Options:~n"
        "  --input FILE    Input LFE file (required)~n"
        "  --output FILE   Output Erlang file (default: input.erl)~n"
        "  --force         Overwrite existing output file~n"
        "~n"
        "Examples:~n"
        "  rebar3 lfe confabulate --input data.lfe~n"
        "  rebar3 lfe confabulate --input data.lfe --output config.erl~n"
        "  rebar3 lfe confabulate -i data.lfe -o config.erl --force~n"
        "~n"
        "Note: Only data files are supported, not code modules.~n"
        "The input should contain LFE data structures, not function~n"
        "definitions or module declarations.~n",
        [Description]
    ).
