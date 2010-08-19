%%======================================================================
%% Roma Client Proxy Daemon -  Request Handler.
%% @author yosuke hara
%% @doc
%% This module is parameterized module.
%% @end
%%======================================================================
-module(romacd_request, [RequestId, RomaConns]).
-author('yosuke hara').
-vsn('0.8.0').
-include("romacd.hrl").
-include("romacd_error.hrl").

-export([init_command/2,
         invoke/2,
         invoke/3,
         invoke/4,
         invoke/5]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%%
%% do execute.
%%
invoke(do_exec, Command) ->
    invoke(do_exec, Command, 0).
invoke(do_exec, Command, RType) ->
    invoke(do_exec, Command, RType, 0, []).
invoke(do_exec, Command, RType, Times, Error) ->
    case Times of
        ?RETRY_TIMES ->
            {error, Error};
        _ ->
            [ClientConfig] =  mnesia:dirty_read(client_config, 1),
            VirtualNodeId =
                case ClientConfig of
                    {aborted, _} -> 0;
                    _ ->
                        BaseNum = 
                            random:uniform(
                              romacd_common:power(
                                2, ClientConfig#client_config.bit_of_virtual_nodes)-1),

                        BaseNum bsl (ClientConfig#client_config.num_of_use_head_bits -
                                     ClientConfig#client_config.bit_of_virtual_nodes)
                end,

            ConnInfo =
                case  mnesia:dirty_read(routing_table, VirtualNodeId) of
                    [{routing_table, _VirtualNodeId, _VClock, Nodes}] ->
                        TargetConns = lists:map(fun(N) -> N#roma_node.bin end, Nodes),
                        {ok, TargetConns};
                    _ ->
                        log4erl:error("~p:~p - [~p]",
                                      [?MODULE, "invoke - do_exec - error(a)", ?ERROR_COULD_NOT_GOT_NODES]),
                        {error, ?ERROR_COULD_NOT_GOT_NODES}
                end,

            case ConnInfo of
                {ok, NodePorts} ->
                    case romacd_sender:recv_and_reply(RequestId, NodePorts, Command, command, RType) of
                        {ok, Ret} ->
                            {ok, Ret};
                        {error, Why} ->
                            log4erl:error("~p:~p  - reqid:[~p] - cause:[~p]",
                                          [?MODULE, "invoke(do_exec)/5a", RequestId, Why]),
                            invoke(do_exec, Command, RType, Times+1, Error++[Why])
                    end;
                {error, Reason} ->
                    log4erl:error("~p:~p  - reqid:[~p] - cause:[~p]",
                                  [?MODULE, "invoke(do_exec)/5c", RequestId, Reason]),
                    invoke(do_exec, Command, RType, Times+1, Error++[Reason])
            end
    end.

init_command(Command, RType) ->
    [ConnInfo] = mnesia:dirty_read(client_config, 1),
    Nodes = ConnInfo#client_config.roma_nodes,
    NodePorts =
        lists:map(fun(N) ->
                          {Host, Port} = N,
                          list_to_binary(Host ++ "_" ++ integer_to_list(Port))
                  end, Nodes),
    romacd_sender:recv_and_reply(RequestId, NodePorts, Command, command, RType).

%%
%% do query.
%%
invoke(do_query, Command, CType, RType) ->
    romacd_sender:recv_and_reply(RequestId, RomaConns, Command, CType, RType).

