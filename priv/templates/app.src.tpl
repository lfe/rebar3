{application, '{{name}}', 
  [{vsn, git},
   {description, "{{description}}"},
   {modules, []},
   {applications,
     [stdlib,
      kernel
      ]
    },
  {mod, {'{{name}}.app', []}}
  ]
}.
