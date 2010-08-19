%%======================================================================
%% ROMA Client Proxy Daemon
%% @author yosuke hara
%% @doc
%% @end
%%======================================================================
-module(romacd_app).
-author('yosuke hara').
-vsn('0.8.0').
-behaviour(application).

%% Application and Supervisor callbacks
-export([start/2, stop/1]).

-include("romacd.hrl").

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    romacd_sup:start_link().

stop(_State) ->
    ok.

