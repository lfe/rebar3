{application, '{{name}}',
 [{description, "{{description}}"},
  {vsn, git},
  {registered, []},
  {mod, {'{{name}}-app', []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
