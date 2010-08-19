%%======================================================================
%% Roma Client Proxy Daemon - Supervisor
%%
%% @author yosuke hara
%% @doc
%% @end
%%======================================================================
-module(romacd_sup).
-author('yosuke hara').
-vsn('0.8.0').
-behaviour(supervisor).

-include("romacd.hrl").

%% External API
-export([start_link/0, stop/0]). 

%% Callbacks
-export([init/1]). 

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @spec () -> ok
%% @doc start link... 
%% @end
start_link() ->
    ConfFileName =
        romacd_common:get_app_env(?APP_NAME, config_file, ?DEFAULT_CONF_FILE),
    case file:consult(ConfFileName) of
        {ok, [{romacd, [{num_of_use_head_bits,  HBits},
                        {bit_of_virtual_nodes,  BitOfVNodes},
                        {timeout,               Timeout},
                        {retry_times,           RetryTimes},
                        {listen_port,           ListenPort},
                        {num_of_acceptors,      NumOfAcceptors},
                        {num_of_connections,    NumOfConns},
                        {roma_nodes,            RomaNodes}]}]} ->
            
            ClientConfig = #client_config{version=1,
                                          num_of_use_head_bits=HBits,
                                          bit_of_virtual_nodes=BitOfVNodes,
                                          timeout=Timeout,
                                          retry_times=RetryTimes,
                                          listen_port=ListenPort,
                                          num_of_acceptors=NumOfAcceptors,
                                          num_of_connections=NumOfConns,
                                          roma_nodes=RomaNodes},
            mnesia:transaction(fun() -> mnesia:write(ClientConfig) end),

            Ret = supervisor:start_link(
                    {local, ?MODULE}, ?MODULE,
                    [ListenPort, NumOfAcceptors, {romacd_proxy, main}]
                   ),
            
            check_mklhash(),
            Ret;
        {error, _} ->
            log4erl:error("~p:~p - [~p]", [?MODULE, "start_link - error", "system config load error."]),
            {error, "system config load error."}
    end.

%% @spec () -> ok | 
%%             not_started
%% @doc stop process.
%% @end
stop() ->
    case whereis(?MODULE) of
        Pid when pid(Pid) ->
            exit(Pid, shutdown),
            ok;
        _ -> not_started
    end.

%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @doc stop process.
%% @end
%% @private
init([Port, MaxWorker, Callback]) ->
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
          [%% 1.
           {tcp_server, {tcp_server_sup, start_link, [Port, MaxWorker, Callback, {}]},
            permanent,
            ?SHUTDOWN_WAITING_TIME,
            supervisor,
            [tcp_server_sup]
           },
           %% 2.
           {romacd_sender, {romacd_sender, start_link, []},
            permanent,
            ?SHUTDOWN_WAITING_TIME,
            worker,
            [romacd_sender]
           },
           %% 3.
           {romacd_routingtable, {romacd_routingtable, start_link, []},
            permanent,
            ?SHUTDOWN_WAITING_TIME,
            worker,
            [romacd_routingtable]
           }
          ]}
    }.

%% ---------------------------------------------------------------------
%% Internal Functions
%% ---------------------------------------------------------------------
%% @spec () -> ok
%% @doc check mklhash w/routing-table.
%% @end
%% @private
check_mklhash() ->
    timer:sleep(?CHECK_INTERVAL),
    try
        romacd_routingtable:reflesh_mklhash()
    catch
        _:Reason ->
            log4erl:error("~p:~p - [~p]", [?MODULE, "start_link - check_mklhash", Reason])
    end,
    check_mklhash().

