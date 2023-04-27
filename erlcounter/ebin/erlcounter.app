{application, erlcounter,
    [{vsn, "1.0.0"},
    {modules, [erlcounter, erlcounter_dispatcher, library_counter, counter_worker]},
    {applications, [ppool]},
    {registered, [erlcounter]},
    {mod, {erlcounter, []}},
    {env, [{directory, "."}, {maxfiles, 10}, {regex, ["case\\s.+\\sof", "if\\s.+\\s->"]}]}
]}.