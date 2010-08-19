{application, romacd,
  [
    {description, "ROMA client proxy daemon"},
    {vsn, "0.8"},
    {id, "romacd"},
    {modules, [
      romacd_app,
      tcp_server_sup,
      tcp_acceptor,
      romacd_sup,
      romacd_request,
      romacd_proxy,
      romacd_connection,
      romacd_routingtable,
      romacd_common
      ]
		},
    {registered,   []},
    {applications, [kernel, stdlib]},
    %%
    %% mod: Specify the module name to start the application, plus args
    %%
    {mod, {romacd_app, []}},
    {env, []}
  ]
}.

