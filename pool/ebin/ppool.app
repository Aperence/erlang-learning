{application, ppool,
    [{vsn, "1.0.0"},
    {modules, [ppool, pool_server, ppool_supervisor, supervisor_workers]},
    {registered, [ppool]},
    {mod, {ppool, []}}
]}.