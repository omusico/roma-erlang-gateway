%%======================================================================
%% TCP Server  - Acceptor.
%% @author yosuke hara
%%
%%======================================================================
-module(tcp_acceptor).
-author('yosuke hara').

%% External API
-export([start_link/4]).

%% Callbacks
-export([init/5, accept/4]).

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @spec (Id, Callback, Option, ListenSocket) -> ok
%% @doc start link... 
%% @end
start_link(Id, Callback, Option, ListenSocket) ->
    proc_lib:start_link(?MODULE, init, [self(), Id, Callback, Option, ListenSocket]).

%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @spec (Parent, Id, Callback, Option, ListenSocket) -> ok
%% @doc initialize. 
%% @end
%% @private
init(Parent, Id, Callback, Option, ListenSocket) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    accept(Id, Callback, Option, ListenSocket).

%% @spec (Id, Callback, Option, ListenSocket) -> ok
%% @doc accept request(s).
%% @end
%% @private
accept(Id, Callback, Option, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    case Callback of
        {Module, Function} -> Module:Function(Id, Socket);
                         _ -> Callback(Id, Socket)
    end,
    accept(Id, Callback, Option, ListenSocket).

