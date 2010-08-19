%%======================================================================
%% ROMA Client Proxy Daemon
%% @author yosuke hara
%% @doc
%% <pre>
%%                 +----------------+
%%                 |   romacd_sup   |
%%                 +--------+-------+
%%                          | 
%%                          +--------------------+--------------+  
%%                          | (one_for_one)      |              |   (one_for_one)
%%                 +----------------+            |      +-------+-------------+
%%                 | tcp_server_sup |            |      | romacd_routingtable |
%%                 +--------+-------+            |      +-------+-------------+
%%                          |                    | (one_for_one)          (gen_server)
%%                          | (one_for_one)   +--+---------------+             
%%         +----------------+---------+       |    romcd_pool    |
%%         |                          |       +--+---------------+
%% +-------+---------+          +-----|---------+            (gen_server)
%% |   tcp_acceptor  |        +-------|--------+|---->+-------------------+
%% +-----------------+       +--------+-------+|+---->|  romacd_request   |
%%                           |  romacd_proxy  |+----->+-------------------+
%%                           +----------------+        parameterized_module
%% </pre>
%% @end
%%======================================================================
-module(romacd).
-author('yosuke hara').
-vsn('0.8.0').
-include("romacd.hrl").

%% Application and Supervisor callbacks
-export([start/0, stop/0]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start() ->
    application:start(mnesia),
    mnesia:change_table_copy_type(schema, node(),disc_copies),
    mnesia:create_table(routing_table, [{disc_copies, [node()]}, {attributes, record_info(fields, routing_table)}]),
    mnesia:create_table(mklhash,       [{disc_copies, [node()]}, {attributes, record_info(fields, mklhash)}]),
    mnesia:create_table(client_config, [{disc_copies, [node()]}, {attributes, record_info(fields, client_config)}]),

    application:start(log4erl),
    log4erl:conf(filename:absname("") ++ "/ebin/log4erl.conf"),
    application:start(romacd).

stop() ->
    application:stop(log4erl),
    application:stop(mnesia),
    init:stop().

