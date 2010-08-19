%%======================================================================
%% ROMA Client Proxy Daemon - request proxy
%% @author yosuke hara
%% @doc
%% @end
%%======================================================================
-module(romacd_proxy).
-author('yosuke hara').
-vsn('0.8.0').

-export([main/2,
         main/4]).

-include("romacd.hrl").
-include("romacd_error.hrl").

-define(CMD_TYPE_KEY_ONLY, key_only).
-define(CMD_TYPE_HAS_BODY, has_body).

-define(CMD_GET,                "get").
-define(CMD_ADD,                "add").
-define(CMD_SET,                "set").
-define(CMD_REPLACE,            "replace").
-define(CMD_APPEND,             "append").
-define(CMD_PREPEND,            "prepend").
-define(CMD_DELETE,             "delete").

-define(CMD_ALIST_FIRST,        "alist_first").
-define(CMD_ALIST_TO_JSON,      "alist_to_json").
-define(CMD_ALIST_LAST,         "alist_last").
-define(CMD_ALIST_POP,          "alist_pop").
-define(CMD_ALIST_SHIFT,        "alist_shift").
-define(CMD_ALIST_TO_S,         "alist_to_s").
-define(CMD_ALIST_AT,           "alist_at").
-define(CMD_ALIST_INDEX,        "alist_index").
-define(CMD_ALIST_INSERT,       "alist_insert").
-define(CMD_ALIST_SIZED_INSERT, "alist_sized_insert").
-define(CMD_ALIST_DELETE,       "alist_delete").
-define(CMD_ALIST_INCLUDE,      "alist_include?").
-define(CMD_ALIST_JOIN,         "alist_join").
-define(CMD_ALIST_PUSH,         "alist_push").
-define(CMD_ALIST_CLEAR,        "alist_clear").
-define(CMD_ALIST_DELETE_AT,    "alist_delete_at").
-define(CMD_ALIST_EMPTY,        "alist_empty?").
-define(CMD_ALIST_LENGTH,       "alist_length").

-record(rcommand, {body, method, key, bytes, ctype}).

%%----------------------------------------------------------------------
%% Callback function(s)
%%----------------------------------------------------------------------
%% @spec (Id, WorkerSock) -> ok
%% @doc For receiving get responses containing data.
%% @end
main(Id, WorkerSock) ->
    %% get parameters.
    Config = 
        case mnesia:dirty_read(client_config, 1) of
            [ClientConfig] ->
                {ok, ClientConfig};
            {aborted, Why} ->
                log4erl:error("~p:~p - [~p]", [?MODULE, "main/2a", Why]),
                {error, Why}
        end,
    RetryTimes =
        case Config of
            {ok, C} ->
                C#client_config.retry_times;
            {error, Reason} ->
                log4erl:error("~p:~p - [~p]", [?MODULE, "main/2b", Reason]),
                ?RETRY_TIMES
        end,

    %% get connections.
    case get_roma_nodes() of
        {ok, RomaSocks} ->
            main(Id, WorkerSock, RomaSocks, [Config, RetryTimes]);
        {error, Cause} ->
            log4erl:error("~p:~p - [~p]", [?MODULE, "main/2c", Cause]),
            {error, Cause}
    end.

main(Id, WorkerSock, RomaConnList, [Config ,RetryTimes]) ->
    Ret = gen_tcp:recv(WorkerSock, 0),
    case Ret of
        {ok, <<?QUIT>>} ->
            gen_tcp:close(WorkerSock);
        {ok, Line} ->
            Command = binary_to_list(Line),
            Token    = string:tokens(Command, " \r\n"),
            case parse_command(Command, Token) of
                {error, command_not_found} ->
                    gen_tcp:send(WorkerSock, "ERROR\r\n");

                {ok, ?CMD_TYPE_KEY_ONLY, CommandObj} ->
                    send_get(Id, RomaConnList, WorkerSock, [Config, RetryTimes], CommandObj, 1),
                    main(Id, WorkerSock, RomaConnList, [Config,RetryTimes]);

                {ok, ?CMD_TYPE_HAS_BODY, CommandObj} ->
                    send_set(Id, RomaConnList, WorkerSock, [Config, RetryTimes], CommandObj),
                    main(Id, WorkerSock, RomaConnList, [Config,RetryTimes])
            end;

        {error, _Reason} ->
            gen_tcp:close(WorkerSock),
            {ok, connection_closed}
    end.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
%% @spec get_roma_nodes() -> {ok, Sockets} | {error, Cause}
%% @doc
%% @end
%% @private
get_roma_nodes() ->
    case mnesia:dirty_read(client_config, 1) of
        [ClientConfig] ->
            Nodes = ClientConfig#client_config.roma_nodes,
            NewNodes = lists:map(
                      fun(N) ->
                              {Host, Port} = N,
                              _NodePort = list_to_binary(Host ++ "_" ++ integer_to_list(Port))
                      end, Nodes),
            {ok, NewNodes};

        {aborted, Cause} ->
            log4erl:error("~p:~p - [~p]", [?MODULE, "get_roma_nodes/0", Cause]),
            {error, Cause}
    end.

%% @doc send a set-command to the roma-server.
%% @end
%% @private
send_set(Id, RomaConnList, WorkerSock, [Config,RetryTimes], CommandObj) ->
    inet:setopts(WorkerSock, [{packet, raw}]),

    case gen_tcp:recv(WorkerSock, list_to_integer(CommandObj#rcommand.bytes)) of
        {ok, Value} ->
            BinStrLine = list_to_binary(CommandObj#rcommand.body),
            CRLF = <<"\r\n">>,
            Command = <<BinStrLine/binary, Value/binary, CRLF/binary>>,

            NewCmmandObj = CommandObj#rcommand{body=Command},
            send_get(Id, RomaConnList, WorkerSock, [Config,RetryTimes], NewCmmandObj, 1);

        {error, closed} -> ok
    end,
    gen_tcp:recv(WorkerSock, 2),
    inet:setopts(WorkerSock, [{packet, line}]).

%% @doc send common command to the roma-server.
%% @end
%% @private
send_get(Id, RomaConnList, WorkerSock, [Config,RetryTimes], CommandObj, Times) ->
    try
        case Times of
            RetryTimes ->
                gen_tcp:send(WorkerSock, ?SERVER_ERROR_CAUSE_CONN);
            _ ->
                Key = CommandObj#rcommand.key,

                {_, Nodes} = romacd_routingtable:get_nodes_from_key(Config, Key),
                TargetConns = lists:map(fun(N) -> N#roma_node.bin end, Nodes),

                {{Y,MO,D},{H,MI,S}} = erlang:universaltime(),
                RequestId = {Id, erlang:phash2([Y,MO,D,H,MI,S,Key])},
                RequestHandler = romacd_request:new(RequestId, TargetConns),
                Ret = RequestHandler:invoke(
                        do_query,
                        CommandObj#rcommand.body,
                        CommandObj#rcommand.ctype,
                        receive_type_is(CommandObj#rcommand.method)),

                case Ret of
                    {ok, Reply } ->
                        gen_tcp:send(WorkerSock, Reply);
                    {error, Cause} ->
                        log4erl:error("~p:~p - [~p]", [?MODULE, "send_get/7a", Cause]),
                        send_get(Id, RomaConnList, WorkerSock, [Config,RetryTimes], CommandObj, Times+1)
                end
        end
    catch _:Reason ->
            log4erl:error("~p:~p - [~p]", [?MODULE, "send_get/7b", Reason]),
            gen_tcp:send(WorkerSock, ?SERVER_ERROR_CAUSE_CONN)
    end.

%% @spec (M) -> 1 |
%%              0
%% @doc get receive type.
%% @end
%% @private
%%
receive_type_is(M) when (M == "alist_length");
                        (M == "alist_index" );
                        (M == ?MKLHASH      ) -> 1;
receive_type_is(_) -> 0.


%% [get] get                  <key> 
parse_command(Command, [Method, Key]) when Method =:= ?CMD_GET ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};
%% [get] alist_first          <key> 
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_FIRST ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};
%% [get]alist_last            <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_LAST ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};
%% [get] alist_pop            <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_POP ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};
%% [get] alist_shift          <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_SHIFT ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};
%% [get] alist_to_json        <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_TO_JSON ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};
%% [get] alist_to_s           <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_TO_S ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};
%% [get] alist_at             <key> <index>
parse_command(Command, [Method, Key, _Index]) when Method =:= ?CMD_ALIST_AT ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=read}};


%% [other] delete             <key> 
parse_command(Command, [Method, Key]) when Method =:= ?CMD_DELETE ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=other}};
%% [other] alist_clear        <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_CLEAR ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=other}};
%% [other] alist_empty?       <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_EMPTY ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=other}};
%% [other] alist_length       <key>
parse_command(Command, [Method, Key]) when Method =:= ?CMD_ALIST_LENGTH ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=other}};
%% [other] alist_delete_at    <key> <index>
parse_command(Command, [Method, Key, _Index]) when Method =:= ?CMD_ALIST_DELETE_AT ->
    {ok, ?CMD_TYPE_KEY_ONLY, #rcommand{body = Command, method = Method, key = Key, ctype=other}};


%% [write] append             <key> <bytes>
parse_command(Command, [Method, Key, Bytes]) when Method =:= ?CMD_APPEND ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] prepend            <key> <bytes>
parse_command(Command, [Method, Key, Bytes]) when Method =:= ?CMD_PREPEND ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] alist_delete       <key> <bytes>
parse_command(Command, [Method, Key, Bytes]) when Method =:= ?CMD_ALIST_DELETE ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] alist_include?     <key> <bytes>
parse_command(Command, [Method, Key, Bytes]) when Method =:= ?CMD_ALIST_INCLUDE ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] alist_push         <key> <bytes>
parse_command(Command, [Method, Key, Bytes]) when Method =:= ?CMD_ALIST_PUSH ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] alist_join         <key> <bytes>
parse_command(Command, [Method, Key, Bytes]) when Method =:= ?CMD_ALIST_JOIN ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};

%% [write] alist_insert       <key> <index> <bytes>
parse_command(Command, [Method, Key, _Index, Bytes]) when Method =:= ?CMD_ALIST_INSERT ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] alist_sized_insert <key> <array-size> <bytes>
parse_command(Command, [Method, Key, _ArraySize, Bytes]) when Method =:= ?CMD_ALIST_SIZED_INSERT ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};

%% [write] set                <key> <flags> <exptime> <bytes>
parse_command(Command, [Method, Key, _Flags, _Exptime, Bytes]) when Method =:= ?CMD_SET ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] add                <key> <flags> <exptime> <bytes>
parse_command(Command, [Method, Key, _Flags, _Exptime, Bytes]) when Method =:= ?CMD_ADD ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};
%% [write] replace            <key> <flags> <exptime> <bytes>
parse_command(Command, [Method, Key, _Flags, _Exptime, Bytes]) when Method =:= ?CMD_REPLACE ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};

parse_command(Command, [Method, Key, Bytes]) when Method =:= ?CMD_ALIST_INDEX ->
    {ok, ?CMD_TYPE_HAS_BODY, #rcommand{body = Command, method = Method, key = Key, bytes=Bytes, ctype=write}};

parse_command(_, _)          -> {error, command_not_found}.

%%--------------------------------------------------------------------
%% Tests.
%%--------------------------------------------------------------------
parse_command_get_test() ->
    Command = "get key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_GET, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,  Cmd#rcommand.body),
    ?assertEqual(?CMD_GET, Cmd#rcommand.method),
    ?assertEqual(Key,      Cmd#rcommand.key),
    ?assertEqual(read,     Cmd#rcommand.ctype).

parse_command_alist_first_test() ->
    Command = "alist_first key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_FIRST, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_FIRST,   Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(read,               Cmd#rcommand.ctype).

parse_command_alist_last_test() ->
    Command = "alist_last key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_LAST, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,         Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_LAST, Cmd#rcommand.method),
    ?assertEqual(Key,             Cmd#rcommand.key),
    ?assertEqual(read,            Cmd#rcommand.ctype).

parse_command_alist_pop_test() ->
    Command = "alist_pop key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_POP, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_POP,     Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(read,               Cmd#rcommand.ctype).

parse_command_alist_shift_test() ->
    Command = "alist_shift key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_SHIFT, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_SHIFT,   Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(read,               Cmd#rcommand.ctype).

parse_command_alist_to_json_test() ->
    Command = "alist_to_json key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_TO_JSON, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_TO_JSON, Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(read,               Cmd#rcommand.ctype).

parse_command_alist_to_s_test() ->
    Command = "alist_to_s key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_TO_S, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_TO_S,    Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(read,               Cmd#rcommand.ctype).

parse_command_alist_at_test() ->
    Command = "alist_at key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_AT, Key, 1]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_AT,      Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(read,               Cmd#rcommand.ctype).

parse_command_delete_test() ->
    Command = "delete key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_DELETE, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_DELETE,        Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(other,              Cmd#rcommand.ctype).

parse_command_alist_empty_test() ->
    Command = "alist_empty key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_EMPTY, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_EMPTY,   Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(other,              Cmd#rcommand.ctype).

parse_command_alist_alist_length_test() ->
    Command = "alist_length key\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_LENGTH, Key]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_LENGTH,  Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(other,              Cmd#rcommand.ctype).

parse_command_alist_delete_at_test() ->
    Command = "alist_delete_at key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_DELETE_AT, Key, 1]),
    ?assertEqual(?CMD_TYPE_KEY_ONLY,   Type),
    ?assertEqual(Command,              Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_DELETE_AT, Cmd#rcommand.method),
    ?assertEqual(Key,                  Cmd#rcommand.key),
    ?assertEqual(other,                Cmd#rcommand.ctype).

parse_command_append_test() ->
    Command = "append key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_APPEND, Key, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_APPEND,        Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_preppend_test() ->
    Command = "prepend key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_PREPEND, Key, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_PREPEND,       Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_alist_delete_test() ->
    Command = "alist_delete key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_DELETE, Key, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_DELETE,  Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_alist_include_test() ->
    Command = "alist_include key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_INCLUDE, Key, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_INCLUDE, Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_alist_push_test() ->
    Command = "alist_push key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_PUSH, Key, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_PUSH,    Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_alist_join_test() ->
    Command = "alist_join key 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_JOIN, Key, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_JOIN,    Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_alist_insert_test() ->
    Command = "alist_insert key 1 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_INSERT, Key, 1, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_INSERT,  Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_alist_sized_insert_test() ->
    Command = "alist_sized_insert key 1 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_SIZED_INSERT, Key, 1, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY,      Type),
    ?assertEqual(Command,                 Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_SIZED_INSERT, Cmd#rcommand.method),
    ?assertEqual(Key,                     Cmd#rcommand.key),
    ?assertEqual(write,                   Cmd#rcommand.ctype).

parse_command_alist_index_test() ->
    Command = "alist_index key 0 0 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ALIST_INDEX, Key, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY,      Type),
    ?assertEqual(Command,                 Cmd#rcommand.body),
    ?assertEqual(?CMD_ALIST_INDEX,        Cmd#rcommand.method),
    ?assertEqual(Key,                     Cmd#rcommand.key),
    ?assertEqual(write,                   Cmd#rcommand.ctype).

parse_command_set_test() ->
    Command = "set key 0 0 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_SET, Key, 0, 0, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_SET,           Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_add_test() ->
    Command = "add key 0 0 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_ADD, Key, 0, 0, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_ADD,           Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_replace_test() ->
    Command = "replace key 0 0 1\r\n",
    Key = "key",
    {ok, Type, Cmd} = parse_command(Command, [?CMD_REPLACE, Key, 0, 0, 1]),
    ?assertEqual(?CMD_TYPE_HAS_BODY, Type),
    ?assertEqual(Command,            Cmd#rcommand.body),
    ?assertEqual(?CMD_REPLACE,       Cmd#rcommand.method),
    ?assertEqual(Key,                Cmd#rcommand.key),
    ?assertEqual(write,              Cmd#rcommand.ctype).

parse_command_not_found_test() ->
    Command = "not_found key 0 0 1\r\n",
    Key = "key",
    {State, Message} = parse_command(Command, ["not_found", Key, 1]),
    ?assertEqual(error, State),
    ?assertEqual(command_not_found, Message).


receive_type_is_test() ->
    ?assertEqual(1, receive_type_is("alist_length")),
    ?assertEqual(1, receive_type_is("alist_index")),
    ?assertEqual(1, receive_type_is(?MKLHASH)),
    ?assertEqual(0, receive_type_is("get")),
    ?assertEqual(0, receive_type_is("set")),
    ?assertEqual(0, receive_type_is("add")),
    ?assertEqual(0, receive_type_is("delete")).
