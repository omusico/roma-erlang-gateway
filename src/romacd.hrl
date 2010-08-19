%%=============================================================================
%% ROMA Client Proxy Daemon - constants.
%% 
%% @author yosuke hara
-author('yosuke hara').
-vsn('0.8.0').
-include_lib("eunit/include/eunit.hrl").

-define(APP_NAME,'romacd').
-define(MAX_TIME,     60).
-define(MAX_ACCEPT,  100).
-define(MAX_RESTART,   5).
-define(SHUTDOWN_WAITING_TIME, 2000).

-define(DEFAULT_CONF_FILE,'romacd.config').
-define(RETRY_TIMES,             3).
-define(TIMEOUT,              3000).
-define(DEFAULT_PORT,        12345).
-define(CHECK_INTERVAL,      10000).
-define(DEF_ROMA_NODES, [
    {"127.0.0.1",11211},
    {"127.0.0.1",11212}
  ]).

%% return values.
-define(CRLF,        "\r\n").
-define(END,         "END\r\n").
-define(STORED,      "STORED\r\n").
-define(NOT_STORED,  "NOT_STORED\r\n").
-define(DELETED,     "DELETED\r\n").
-define(NOT_DELETED, "NOT_DELETED\r\n").
-define(NOT_FOUND,   "NOT_FOUND\r\n").
-define(CLEARED,     "CLEARED\r\n").
-define(NOT_CLEARED, "NOT_CLEARED\r\n").
-define(TRUE,        "true\r\n").
-define(FALSE,       "false\r\n").
-define(SERVER_ERROR,"SERVER_ERROR").
-define(SERVER_ERROR_CAUSE_CONN,"SERVER_ERROR - did not got connection.\r\n").
-define(TCP_SEND_ERROR,         "SERVER_ERROR - tcp send exception.\r\n").
-define(ZERO_CONNECTION,        "ZERO-CONNECTION - could not got connection.").


%% roma-commands.
-define(MKLHASH,          "mklhash 0\r\n").
-define(QUIT,             "quit\r\n").
-define(ROUTINGDUMP_JSON, "routingdump json\r\n").

%% routing-table related.
-define(DGST_BITS,   32).
-define(DIV_BITS,     9).

%% TCP-related.
-define(TCP_OPTS_ROMA,   [binary, {packet, line}, {active, true }, {reuseaddr, true}, {nodelay, true}]).
-define(TCP_OPTS_LISTEN, [binary, {packet, line}, {active, false}, {reuseaddr, true}]).

%% registered process name.
-define(PROXY_WORKER, "proxy_worker-").
-define(CONN_HANDLER, "conn_handler-").

%% Records.
-record(roma_node,     {host,
                        port,
                        bin}).
-record(routing_table, {vnode_id,
                        vnode_clock,
                        redundants}).
-record(mklhash,       {version,
                        mklhash}).
-record(client_config, {version,
                        num_of_use_head_bits,
                        bit_of_virtual_nodes,
                        timeout,
                        retry_times,
                        listen_port,
                        num_of_acceptors,
                        num_of_connections,
                        roma_nodes}).

%% Error Messages.
-define(ERROR_INVALID_FORMAT_JSON, "invalid format json.").
-define(ERROR_COULD_NOT_GOT_NODES, "could not got nodes from routing table.").

-define(ERROR_LOG_FORMAT, "~p:~p  - reqid:[~p] - cause:[~p]").
-define(ERROR_LOG_FORMAT_WITH_NODEPORT, "~p:~p  - reqid:[~p] - cause:[~p] - nodeport:[~p]").
-define(INFO_LOG_FORMAT,  "~p:~p  - reqid:[~p] - [~p]").

