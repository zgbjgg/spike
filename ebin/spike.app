{application, spike,
 [
  {description, "SPIKE - Server for monitoring alive devices"},
  {vsn, "1.9"},
  {modules, [spike_app, spike_sup, spike]},
  {registered, [spike_app, spike_sup, spike]},
  {applications,[ kernel, stdlib]},
  {mod, { spike_app, []}} 
]}.
