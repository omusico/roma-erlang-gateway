%%======================================================================
%% ROMA Client Proxy Daemon - common functions.
%% @author yosuke hara
%% @doc
%% @end
%%======================================================================
-module(romacd_common).
-author('yosuke hara').
-vsn('0.8.0').
-include("romacd.hrl").

-export([get_app_env/3,
         power/2,
         start_proc/0,
         stop_proc/1,
         now/0]).

%%----------------------------------------------------------------------
%% External functions
%%----------------------------------------------------------------------
%% @spec (AppName, Opt, Default) -> Val
%% @doc get application environment's value.
%% @end
get_app_env(AppName, Opt, Default) ->
    case application:get_env(AppName, Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error     -> Default
            end
    end.

%% @spec power(N,P) -> Num
%% @doc 
%% @end
power(N, P) when is_integer(N), P == 0 -> 1;
power(N, P) when is_integer(N), P >  0 -> N * power(N,  P - 1);
power(_, _) -> 0.

%% @spec () -> {ok, {Time1, Time2}}
%% @doc Start the profiler.
%% @end
start_proc() ->
    statistics(runtime),
    statistics(wall_clock),
    {_, T1} = statistics(runtime),
    {_, T2} = statistics(wall_clock),
    {ok, {T1, T2}}.

%% @spec (Time) -> {Time1, Time2} |
%%                 {error, Other}
%% @doc Stop profiler
%% @end
stop_proc(Time) ->
    case Time of
        {ok, {OldT1, OldT2}} ->
            {_, T1} = statistics(runtime),
            {_, T2} = statistics(wall_clock),
            {T1 - OldT1, T2 - OldT2};
        Other ->
            {error, Other}
    end.

now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


%%--------------------------------------------------------------------
%% Tests.
%%--------------------------------------------------------------------
power_test() ->    
    ?assertEqual(1,  power(2,0)),
    ?assertEqual(4,  power(2,2)),
    ?assertEqual(512,  power(2,9)),
    ?assertEqual(1024,  power(2,10)).

proc_test() ->
    Time = start_proc(),
    lists:foreach(
      fun (X) ->
              math:pow(X, 10)
      end,
      lists:seq(1, 1000000)),

    {Time1, Time2} = stop_proc(Time),
    ?debugFmt("~n> time=~p (~p) msec~n", [Time1, Time2]).
