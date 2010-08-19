%%======================================================================
%% Roma Client Proxy Daemon -  Routing Table.
%% @author yosuke hara
%% @doc
%% @end
%%======================================================================
-module(romacd_routingtable).
-author('yosuke hara').
-vsn('0.8.0').
-behaviour(gen_server).

-include("romacd.hrl").
-include("romacd_error.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0,
         stop/0,
         reflesh_mklhash/0,
         get_nodes_from_key/2,
         get_nodes_from_key/3]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
	       terminate/2,
         code_change/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec () -> ok
%% @doc stop server.
%% @end
stop() ->
    gen_server:cast(?MODULE, stop).

reflesh_mklhash() ->
    gen_server:call(?MODULE, {reflesh_mklhash}).

get_nodes_from_key(Config, Key) ->
    gen_server:call(?MODULE, {get_nodes_from_key, Config, Key}).

get_nodes_from_key(From, Config, Key) ->
    Reply = 
        case Config of
            {ok, C} ->
                SearchMask =
                    (romacd_common:power(2, C#client_config.num_of_use_head_bits) - 1) bsl
                    (C#client_config.num_of_use_head_bits - C#client_config.bit_of_virtual_nodes),
                SHA1Input =
                    fun(Str) ->
                            {St, Hex, _} = io_lib:fread("~16u",sha1:hexstring(Str)),
                            case St of
                                ok ->
                                    {true, lists:nth(1, Hex) band 16#ffffffff}; %% 32bit
                                _  ->
                                    {false, 0}
                            end
                    end,

                {_, Digest} = SHA1Input(Key),
                VirtualNodeId = (Digest band SearchMask),
                case  mnesia:dirty_read(routing_table, VirtualNodeId) of
                    [{routing_table, _VirtualNodeId, _VClock, Nodes}] ->
                        {ok, Nodes};
                    _ ->
                        log4erl:error("~p:~p - [~p]", [?MODULE, "get_nodes_from_key - error", ?ERROR_COULD_NOT_GOT_NODES]),
                        {error, ?ERROR_COULD_NOT_GOT_NODES}
                end;
            {error, Why} ->
                {error, Why}
        end,
    gen_server:reply(From, Reply).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([]) ->
    try
        RequestHandler = romacd_request:new({}, []),
        {_, Json} = RequestHandler:init_command(?ROUTINGDUMP_JSON, 0),
        {_, RoutingTable, _} = json_parser:dvm_parser(list_to_binary(Json)),
        write_routing_table(RoutingTable),
        {_, Mklhash} = RequestHandler:init_command(?MKLHASH, 1),

        mnesia:transaction(
          fun() -> mnesia:write(#mklhash{version=1, mklhash=Mklhash}) end),
        {ok, started}

    catch _:Cause ->
            log4erl:error("~p:~p - [~p]", [?MODULE, "init - error", Cause]),
            {ok, not_started}                
    end.

handle_call({reflesh_mklhash}, From, State) ->
    case State of
        started ->
            RequestHandler = romacd_request:new({}, []),
            {_, Mklhash} = RequestHandler:invoke(do_exec, ?MKLHASH, 1),

            Reply =
                case mnesia:dirty_read(mklhash, 1) of
                    [DestMklhash] ->
                        case DestMklhash#mklhash.mklhash of
                            Mklhash ->
                                ok;
                            _ ->
                                case RequestHandler:invoke(do_exec, ?ROUTINGDUMP_JSON) of
                                    {ok, Json} ->
                                        {_, NewRoutingTable, _} = json_parser:dvm_parser(list_to_binary(Json)),
                                        write_routing_table(NewRoutingTable),
                                        mnesia:transaction(
                                          fun() -> mnesia:write(#mklhash{version=1, mklhash=Mklhash}) end),
                                        ok;
                                    {error, Why} ->
                                        {error, Why}
                                end                                        
                        end;
                    {aborted, Cause} ->
                        log4erl:error("~p:~p - [~p]", [?MODULE, "reflesh_mklhash - error(b)", Cause]),
                        {error, Cause}
                end,
            gen_server:reply(From, Reply);

        _ ->
            log4erl:debug("~p:~p - [~p]", [?MODULE, "handle_call - reflesh_mklhash - state", State]),
            gen_server:reply(From, {error, not_started})
    end,            
    {noreply, State};

handle_call({get_nodes_from_key, Config, Key}, From, State) ->
    spawn_link(?MODULE, get_nodes_from_key, [From, Config, Key]),
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
%%% Internal functions
%%--------------------------------------------------------------------
%% @spec write_routing_table(RoutingTableJSON) -> {ok,null} | {error, Cause}
%% @doc
%% @end
%% @private
write_routing_table(RoutingTableJSON) ->
    case RoutingTableJSON of
        [{struct, _RomaBaisInfo}, _Nodes, {struct,RoutingTable}] ->
            lists:foreach(
              fun(N)->
                      {VNodeId, Redundant} = N,
                      NewRedundant =
                          lists:map(fun(R) ->
                                            Token = string:tokens(binary_to_list(R), "_"),
                                            #roma_node{host=lists:nth(1,Token),
                                                       port=list_to_integer(lists:nth(2,Token)),
                                                       bin=R}
                                    end, Redundant),

                      mnesia:transaction(
                        fun() -> mnesia:write(
                                   #routing_table{vnode_id= list_to_integer(binary_to_list(VNodeId)),
                                                  vnode_clock=0,
                                                  redundants=NewRedundant})
                        end)
              end, RoutingTable),
            {ok, null};
        _ ->
            log4erl:error("~p:~p - [~p]",
                          [?MODULE, "write_routing_table - error", ?ERROR_INVALID_FORMAT_JSON]),
            {error, ?ERROR_INVALID_FORMAT_JSON}
    end.

