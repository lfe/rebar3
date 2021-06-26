{erl_opts, [debug_info]}.

{deps, [
    {lfe, "2.0.1"}
]}.

{plugins, [
    {rebar3_lfe, {git, "https://github.com/lfe-rebar3/rebar3_lfe.git", {branch, "release/0.3.x"}}}
]}.

{lfe, [
    {main, "scripts/main.lfe"}
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
            {proper, "1.3.0"}
        ]},
        {plugins, [
            {rebar3_proper, "0.12.0"}
        ]},
        {eunit_opts, [verbose]},
        {erl_opts, [{src_dirs, ["src", "test"]}]}
    ]}
]}.

{alias, [
    {coverage, [
        {proper, "-c"},
        {cover, "-v --min_coverage=0"}
    ]},
    {check, [
        compile,
        xref,
        %%dialyzer,
        eunit,
        coverage
    ]}
]}.
