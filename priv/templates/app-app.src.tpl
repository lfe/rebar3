{application, '{{name}}', [
    {description, "{{description}}"},
    {vsn, "0.1.0"},
    {registered, []},
    {mod, {'{{name}}-app', []}},
    {applications, [
        kernel,
        stdlib
    ]},
    {env,[]},
    {modules, []},

    {licenses, ["Apache 2.0"]},
    {links, []}
 ]}.
