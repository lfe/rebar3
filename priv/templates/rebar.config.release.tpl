{erl_opts, [debug_info]}.

{deps, [
    {lfe, "2.1.2"},
    {ltest, "0.13.5"}
]}.

{plugins, [
    {rebar3_lfe, "0.4.6"}
]}.

{provider_hooks, [
    {pre, [{compile, {lfe, compile}}]}
]}.

{xref_checks,[
    undefined_function_calls,undefined_functions,locals_not_used,
    deprecated_function_calls,deprecated_functions
]}.

{relx, [
    {release, {'{{name}}', "0.1.0"}, [
        '{{name}}',
        lfe,
        sasl
    ]},

    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"},

    {dev_mode, true},
    {include_erts, false},

    {extended_start_script, true}
]}.

{profiles, [
    {prod, [
        {relx, [
            {dev_mode, false},
            {include_erts, true}
       ]}
    ]},
    {test, [
        {deps, [
            {proper, "1.4.0"}
        ]},
        {plugins, [
            {rebar3_proper, "0.12.1"}
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
