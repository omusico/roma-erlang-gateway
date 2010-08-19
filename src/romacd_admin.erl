%%======================================================================
%% ROMA Client Proxy Daemon - for admin.
%% @author yosuke hara
%% @doc
%% @end
%%======================================================================
-module(romacd_admin).
-author('yosuke hara').
-vsn('0.8.0').

-export([stop/0]).

stop() ->
    {ok, [[Node]]} = init:get_argument(tnode),
    Ret = rpc:call(list_to_atom(Node), romacd, stop, []),
    io:format("*** roma-client-proxy daemon - [~p] stopped:~p ***~n", [Node, Ret]),
    init:stop().
