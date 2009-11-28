{application, exb,
 [{description, "exb"},
  {vsn, "0.1"},
  {modules, [
    exb_app,
    exb_sup
  ]},
  {registered, []},
  {mod, {exb_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
