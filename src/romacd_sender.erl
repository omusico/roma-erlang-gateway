%%======================================================================
%% ROMA Client Proxy Daemon - command sender.
%% @author yosuke hara
%% @doc
%% @end
%%======================================================================
-module(romacd_sender).
-author('yosuke hara').
-vsn('0.8.0').
-behaviour(gen_server).
-export([start_link/0,
         stop/0,
         recv_and_reply/5]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
	       terminate/2,
         code_change/3]).

-include("romacd.hrl").
-include("romacd_error.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

recv_and_reply(RequestId, NodePorts, Command, CType, RType) ->
    gen_server:call(?MODULE, {recv_and_reply, RequestId, NodePorts, Command, CType, RType}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([]) ->
    case get_connections() of
        {ok, Conns, NumOfMaxConns} ->
            register(
              pool_pid,
              spawn_link(fun() -> loop(Conns, NumOfMaxConns) end)),
            {ok, null};

        {error, Reason} ->
            {error, Reason}
    end.

handle_call({recv_and_reply, RequestId, NodePorts, Command, CType, RType}, From, State) ->
    recv_and_reply_sub(From, RequestId, NodePorts, Command, CType, RType),
    {noreply, State}.

%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions.
%%--------------------------------------------------------------------
recv_and_reply_sub(From,_RequestId, [],       _Command,_CType,_RType) ->
    gen_server:reply(From, {error, ?DID_NOT_GOT_CONN});
recv_and_reply_sub(From, RequestId, [NodePort|T], Command, CType, RType) ->    
    Pid = whereis(pool_pid),
    Pid ! {borrow, self(), RequestId, NodePort},

    receive
        {ok, Conn} ->
            case send_command(RequestId, NodePort, Conn, Command, CType, RType) of
                {ok, Ret} ->
                    gen_server:reply(From, {ok, Ret});
                {error, Cause} ->
                    log4erl:error(?ERROR_LOG_FORMAT_WITH_NODEPORT,
                                  [?MODULE, "recv_and_reply_sub/6a", RequestId, Cause, NodePort]),
                    recv_and_reply_sub(From, RequestId, T, Command, CType, RType)
            end;

        {error, {Host, Port, _Why}} ->
            case gen_tcp:connect(Host, Port, ?TCP_OPTS_ROMA) of
                {ok, Socket} ->
                    case send_command(RequestId, NodePort, Socket, Command, CType, RType) of
                        {ok, Ret} ->
                            log4erl:debug("~p:~p - ~p,~p",
                                          [?MODULE, "recv_and_reply_sub/6b", RequestId, recovered]),
                            gen_server:reply(From, {ok, Ret});

                        {error, Cause} ->
                            log4erl:error(?ERROR_LOG_FORMAT_WITH_NODEPORT,
                                          [?MODULE, "recv_and_reply_sub/6b", RequestId, Cause, NodePort]),

                            recv_and_reply_sub(From, RequestId, T, Command, CType, RType)
                    end;

                {error, Reason} ->
                    log4erl:error(?ERROR_LOG_FORMAT_WITH_NODEPORT,
                                  [?MODULE, "recv_and_reply_sub/6c", RequestId, Reason, NodePort]),
                    recv_and_reply_sub(From, RequestId, T, Command, CType, RType)
            end
    end.

send_command(RequestId, NodePort, Conn, Command, CType, RType) ->
    Reply =
        case gen_tcp:send(Conn, Command) of
            ok ->
                case recv_get_reply_sub(Conn, CType, RType) of
                    {error, Cause} ->
                        log4erl:error(?ERROR_LOG_FORMAT_WITH_NODEPORT,
                                      [?MODULE, "send_command/6a", RequestId, Cause, NodePort]),
                        {error, Cause};
                    {timeout, _} ->
                        log4erl:error(?ERROR_LOG_FORMAT_WITH_NODEPORT,
                                      [?MODULE, "send_command/6b", RequestId, timeout, NodePort]),
                        {error, timeout};
                    {Ret, _} ->
                        {ok, Ret}
                end;
            {error, Cause} ->
                log4erl:error(?ERROR_LOG_FORMAT_WITH_NODEPORT,
                              [?MODULE, "send_command/6c", RequestId, Cause, NodePort]),
                {error, Cause}
            end,

    case Reply of
        {ok, _} ->            
            return_connection(NodePort, Conn);
        {error, _} ->
            gen_tcp:close(Conn)
    end,
    Reply.

get_connections() ->
    case mnesia:dirty_read(client_config, 1) of
        [ClientConfig] ->
            Nodes = ClientConfig#client_config.roma_nodes,
            MaxConns = ClientConfig#client_config.num_of_connections,
            ConnPool = gen_connections(Nodes, MaxConns, []),
            {ok, ConnPool, MaxConns};

        {aborted, Cause} ->
            {error, Cause}
    end.

gen_connections([]              , _MaxConn, List) -> List;
gen_connections([{Host, Port}|T],  MaxConn, List) ->
    {ok, Tuple} =
        gen_conn_proc(#roma_node{host=Host, port=Port}, MaxConn, []),

    NewList = List ++ [Tuple],
    gen_connections(T, MaxConn, NewList).

gen_conn_proc(RomaNode, 0, List) ->
    NodePort =
        RomaNode#roma_node.host ++ "_" ++ integer_to_list(RomaNode#roma_node.port),
    Tuple = {list_to_binary(NodePort), List},
    {ok, Tuple};
gen_conn_proc(RomaNode, MaxConn, List) ->
    Host = RomaNode#roma_node.host,
    Port = RomaNode#roma_node.port,
    NewList = 
        case gen_tcp:connect(Host, Port, ?TCP_OPTS_ROMA) of
            {ok, Socket} ->
                List ++ [Socket];
            {error, _Why} ->
                List
        end,
    gen_conn_proc(RomaNode, MaxConn-1, NewList).

loop(Connections, NumOfMaxConns)->
    receive
        {borrow, From, _RequestId, NodePort} ->
            Token = string:tokens(binary_to_list(NodePort), "_"),
            Host  = lists:nth(1, Token),
            Port  = list_to_integer(lists:nth(2, Token)),
            NewConn = 
                case lists:keyfind(NodePort, 1, Connections) of
                    {NodePort, Pool} ->
                        case length(Pool) of
                            0 ->
                                From ! {error, {Host, Port, ?ZERO_CONNECTION}},
                                Connections;
                            _ ->
                                Conn = lists:nth(1, Pool),
                                case inet:getstat(Conn) of
                                    {ok, _Status} ->
                                        From ! {ok, Conn};
                                    {error, Why} ->
                                        gen_tcp:close(Conn),
                                        From ! {error, {Host, Port, Why}}
                                end,

                                NewPool = lists:delete(Conn, Pool),
                                _Connections =
                                    lists:delete({NodePort, Pool}, Connections) ++ [{NodePort, NewPool}]
                        end;

                    false ->
                        From ! {error, {Host, Port, ?ZERO_CONNECTION}},
                        Connections
                end,
            loop(NewConn, NumOfMaxConns);

        {return, From, NodePort, Conn} ->            
            From ! ok,
            NewConn =
                case lists:keyfind(NodePort, 1, Connections) of
                    {NodePort, Pool} ->
                        if (erlang:length(Pool) >= NumOfMaxConns) ->
                                gen_tcp:close(Conn),
                                Connections;
                           true ->
                                _NewConnections =
                                    lists:delete({NodePort, Pool}, Connections) ++ [{NodePort, Pool ++ [Conn]}]
                        end;
                    false ->
                        _NewConnections = Connections ++ [{NodePort, [Conn]}]
                end,
            log4erl:debug("~p:~p - ~p", [?MODULE, "loop/2b", NewConn]),
            loop(NewConn, NumOfMaxConns)
    end.

return_connection(NodePort, Conn) ->
    Pid = whereis(pool_pid),
    Pid ! {return, self(), NodePort, Conn}.

recv_get_reply_sub(Socket, CType, RType) ->
    recv_get_reply_sub(Socket, CType, RType, []).

recv_get_reply_sub(Socket, CType, RType, Data) when CType == read ->
    receive
        {tcp, Socket, <<?END>>} -> {string:concat(Data, ?END), Socket};        
        {tcp, Socket, Bin} ->
            List = binary_to_list(Bin),
            Token    = string:tokens(List, " \r\n"),
            case Token of
                ["VALUE",_,_,Bytes] ->                    
                    {NewData, _} = list_reverse(Data, lists:reverse(List), 0),
                    DataSize     = list_to_integer(Bytes) + 2,
                    recv_get_reply_sub(Socket, CType, RType, NewData, DataSize, 0);

                _Other ->
                    Err = string:rstr(List, ?SERVER_ERROR),
                    if
                        Err =:= 1 orelse RType >  0 -> {List, Socket};
                        true ->
                            Parse = string:concat(Data, List),
                            recv_get_reply_sub(Socket, CType, 0, Parse)
                    end
            end;
        {error, closed} -> {error, connection_closed};
        {error, Cause } -> {error, Cause}
    after ?TIMEOUT ->
            {timeout, Socket}
    end;

recv_get_reply_sub(Socket, CType, RType, Data)
  when CType == command;
       CType == other;
       CType == write ->
    receive
        {tcp, Socket, <<?END>>         } -> {string:concat(Data, ?END), Socket};
        {tcp, Socket, <<?STORED>>      } -> {?STORED,                   Socket};
        {tcp, Socket, <<?NOT_STORED>>  } -> {?NOT_STORED,               Socket};
        {tcp, Socket, <<?DELETED>>     } -> {?DELETED,                  Socket};
        {tcp, Socket, <<?NOT_DELETED>> } -> {?NOT_DELETED,              Socket};
        {tcp, Socket, <<?NOT_FOUND>>   } -> {?NOT_FOUND,                Socket};
        {tcp, Socket, <<?CLEARED>>     } -> {?CLEARED,                  Socket};
        {tcp, Socket, <<?NOT_CLEARED>> } -> {?NOT_CLEARED,              Socket};
        {tcp, Socket, <<?TRUE>>        } -> {?TRUE,                     Socket};
        {tcp, Socket, <<?FALSE>>       } -> {?FALSE,                    Socket};
        {tcp, Socket, Bin} ->
            List = binary_to_list(Bin),
            Ret = string:rstr(List, ?SERVER_ERROR),
            if
                Ret =:= 1 orelse RType >  0 -> {List, Socket};
                true ->
                    Parse = string:concat(Data, List),
                    recv_get_reply_sub(Socket, CType, 0, Parse)
            end;
        {error, closed} -> {error, connection_closed};
        {error, Cause } -> {error, Cause}
    after ?TIMEOUT ->
            {timeout, Socket}
    end.

recv_get_reply_sub(Socket, CType, RType, Data, BodySize, ProgressLen) when CType == read ->
    receive
        {tcp, Socket, Bin} ->
            List = binary_to_list(Bin),
            {Parse, NewLength} = list_reverse(List, Data, ProgressLen),
            if
                NewLength < BodySize ->
                    recv_get_reply_sub(Socket, CType, RType, Parse, BodySize, NewLength);
                true ->
                    recv_get_reply_sub(Socket, CType, RType, lists:reverse(Parse))
            end;

        {error, closed} -> {error, connection_closed};
        {error, Cause } -> {error, Cause}
    end.

list_reverse([H|T], Acc, Len) ->
    list_reverse(T, [H|Acc], Len+1);
list_reverse([], Acc, Len) ->
    {Acc, Len}.

