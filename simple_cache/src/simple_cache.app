{application, simple_cache,
 [{description, "Simple Caching System"},
  {vsn, "0.3.0"},
  {modules, [simple_cache,
             sc_app, 
             sc_sup,
             sc_element_sup,
             sc_store,
             sc_element,
             sc_event,
             sc_event_logger
            ]},
  {registered, [sc_sup]},
  {applications, [kernel, stdlib, sasl, mnesia, resource]},
  {mod, {sc_app, []}}
 ]
}.
