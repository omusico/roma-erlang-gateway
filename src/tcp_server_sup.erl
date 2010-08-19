%%======================================================================
%% TCP Server - Supervisor
%%
%% @author yosuke hara
%%======================================================================
-module(tcp_server_sup).
-author('yosuke hara').
-vsn('0.8.0').
-behaviour(supervisor).

-include("romacd.hrl").

%% External API
-export([start_link/4, stop/0]). 

%% Callbacks
-export([init/1]). 

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @spec (Port, MaxWorker, Callback, Option) -> ok
%% @doc start link... 
%% @end
start_link(Port, MaxWorker, Callback, Option) ->
    supervisor:start_link(
      {local, ?MODULE}, ?MODULE, [Port, MaxWorker, Callback, Option]).

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
init([Port, MaxWorker, Callback, Option]) ->
    case gen_tcp:listen(Port, [{active, false}, binary, {packet, line}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            init_result(MaxWorker, Callback, Option, ListenSocket);
        {error, Reason} ->
            log4erl:error("tcp_server_sup.init:[~p]", [Reason]),
            {stop, Reason}
    end.

%% ---------------------------------------------------------------------
%% Internal Functions
%% ---------------------------------------------------------------------
%% @doc stop process.
%% @end
%% @private
init_result(MaxWorker, Callback, Option, ListenSocket) ->
    {ok,{{one_for_one, ?MAX_RESTART, ?MAX_TIME},
        lists:map(
          fun (Id) -> {
                list_to_atom("tcp_acceptor_" ++ integer_to_list(Id)),
                {tcp_acceptor, start_link, [Id, Callback, Option, ListenSocket]},
                permanent,
                ?SHUTDOWN_WAITING_TIME,
                worker,
                [tcp_acceptor]}
          end,
          lists:seq(1, MaxWorker))
        }}.
  
