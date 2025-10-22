{erl_opts, [debug_info]}.

{deps, [
    {lfe, "~> 2.2"}
]}.

{project_plugins, [
    {rebar3_lfe, "~> 0.5"},
    rebar3_hex
]}.

{provider_hooks, [
    {pre, [{compile, {lfe, compile}}]}
]}.

{xref_checks,[
    undefined_function_calls,undefined_functions,locals_not_used,
    deprecated_function_calls,deprecated_functions
]}.

{profiles, [
    {test, [
        {deps, [
            {proper, "1.4.0"},
            {ltest, "~> 0.13"}
        ]},
        {plugins, [
            {rebar3_proper, "0.12.1"}
        ]},
        {eunit_opts, [verbose]},
        {erl_opts, [{src_dirs, ["src", "test"]}]}
    ]}
]}.

{alias, [
    %% should be run with `rebar3 as test coverge`
    {coverage, [
        {proper, "-c"},
        {cover, "-v --min_coverage=0"}
    ]},
    %% should be run with `rebar3 as test ltest`
    {ltest, [
        compile,
        {lfe, ltest}
    ]},
    %% should be run with `rebar3 as test check`
    {check, [
        compile,
        %%xref,
        %%dialyzer,
        %%eunit,
        {lfe, ltest},
        coverage
    ]},
    {publish, [
        compile,
        {hex, "publish package"}
    ]}
]}.
