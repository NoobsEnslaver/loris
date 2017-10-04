-module(tester).

-include_lib("common/include/transport_lib.hrl").
-include("apps/server/include/ws_chat_protocol_v1_messages.hrl").

-define(DEFAULT_SERVER_ADDRESS, case os:getenv("TEST_SERVER_ADDRESS") of %export TEST_SERVER_ADDRESS="egg3.local.ru" for example
                                    'false'-> "127.0.0.1";
                                    Server -> Server
                                end).
-define(DEFAULT_SERVER_PORT, case os:getenv("TEST_SERVER_PORT") of
                                 'false'-> proplists:get_value(port, application:get_env(server, tcp_params, [{port, 8080}]));
                                 Port   -> erlang:list_to_integer(Port)
                             end).
-define(DEFAULT_MASTER_PIN, case os:getenv("TEST_MASTER_PIN") of
                                'false'-> 1234;
                                Pin   -> erlang:list_to_integer(Pin)
                            end).

%% -define(DBG, true).
-ifdef(DBG).
-define(CONN_OPTS, #{trace => true}).
-else.
-define(CONN_OPTS, #{trace => false}).
-endif.
%%====================================================================
%% API functions
%%====================================================================
-export([connect_to_ws/2, connect_to_ws/4
        ,send_packet/3
        ,receive_packet/2
        ,disconnect/1
        ,authorize/1, authorize/4
        ,flush_messages/0
        ,flush_messages_dbg/0
        ,get_chats_list/2
        ,get_chat_info/3
        ,send_message/4
        ,receive_packets_until/3]).

-spec authorize(non_neg_integer()) -> binary().
authorize(MSISDN) ->
    authorize(MSISDN, ?DEFAULT_SERVER_ADDRESS, ?DEFAULT_SERVER_PORT, ?DEFAULT_MASTER_PIN).

-spec authorize(non_neg_integer(), string(), non_neg_integer(), non_neg_integer()) -> binary().
authorize(MSISDN, ServerAddress, ServerPort, MasterPin) ->
    {ok, ConnPid} = gun:open(ServerAddress, ServerPort, ?CONN_OPTS),
    case gun:await_up(ConnPid) of
        {ok, _} ->
            Body1 = transport_lib:encode(#{<<"msisdn">> => MSISDN}, ?JSON),
            StreamRef1 = gun:post(ConnPid, "/v1/sms", [{<<"content-type">>, <<"application/", ?JSON/binary>>}], Body1),
            gun:await(ConnPid, StreamRef1),
            Body = transport_lib:encode(#{<<"msisdn">> => MSISDN
                                         ,<<"sms_code">> => MasterPin
                                         ,<<"fname">> => <<"Nikita">>
                                         ,<<"lname">> => <<"Vorontsov">>
                                         ,<<"is_male">> => 'true'
                                         ,<<"age">> => 25}, ?JSON),
            StreamRef2 = gun:post(ConnPid, "/v3/auth", [{<<"content-type">>, <<"application/", ?JSON/binary>>}], Body),
            Result = case gun:await_body(ConnPid, StreamRef2) of
                         {error, _} = Error ->
                             flush_messages(),
                             Error;
                         Resp ->
                             Resp
                     end,
            gun:close(ConnPid),
            Result;
        _Error ->
            lager:info("Error open connectoin to ~p: ~p", [{ServerAddress, ServerPort}, _Error]),
            gun:close(ConnPid),
            _Error
    end.

-spec connect_to_ws(string(), string()) -> {ok, pid()}.
connect_to_ws(Address, Transport) ->            %Address = Tail of the address, "/ws/v1/chat"
    connect_to_ws(?DEFAULT_SERVER_ADDRESS, ?DEFAULT_SERVER_PORT, Address, Transport).
connect_to_ws(ServerAddress, ServerPort, Address, Transport) ->
    {ok, ConnPid} = gun:open(ServerAddress, ServerPort, ?CONN_OPTS),
    case gun:await_up(ConnPid) of
        {ok, _} ->
            gun:ws_upgrade(ConnPid, Address, [{<<"sec-websocket-protocol">>, Transport}]),
            receive
                {gun_ws_upgrade, ConnPid, ok, _Headers} ->
                    {ok, ConnPid};
                {gun_response, ConnPid, _, _, _Status, _Headers} ->
                    {ws_upgrade_failed, _Status, _Headers};
                {gun_error, ConnPid, _StreamRef, Reason} ->
                    {ws_upgrade_failed, Reason}
            after 1000 ->
                    {error, timeout}
            end;
        _Error ->
            gun:close(ConnPid),
            _Error
    end.

-spec disconnect(pid()) -> ok.
disconnect(ConnPid) ->
    gun:close(ConnPid),
    timer:sleep(50),
    ok.

-spec send_packet(pid(), map() | ping, binary()) -> ok.
send_packet(Connection, ping, _Transport) ->
    gun:ws_send(Connection, ping);
send_packet(ConnPid, Data, Transport) when is_map(Data) ->
    BData = transport_lib:encode(Data, Transport),
    gun:ws_send(ConnPid, {binary, BData});
send_packet(ConnPid, BData, _Transport) when is_binary(BData) ->
    gun:ws_send(ConnPid, {binary, BData}).

receive_packet(_ConnPid, Transport)->
    receive
        {gun_ws, _ConnPid, {close, _, _}} -> {close, _ConnPid};
        {gun_ws, _ConnPid, {binary, Frame}} -> transport_lib:decode(Frame, Transport)
    after 2000 -> {error, timeout}
    end.

flush_messages() ->
    receive
        _ -> flush_messages()
    after 50 -> ok
    end.

flush_messages_dbg() ->
    receive
        _Msg ->
            ct:pal("~p~n", [_Msg]),
            flush_messages_dbg()
    after 500 -> ok
    end.

get_chats_list(ConnPid, Transport) ->
    send_packet(ConnPid, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list), Transport),
    #{<<"msg_type">> := ?S2C_CHAT_LIST_TYPE, <<"chats">> := Chats} = receive_packet(ConnPid, Transport),
    Chats.

get_chat_info(ConnPid, Transport, ChatId) ->
    send_packet(ConnPid, ?R2M(#c2s_chat_get_info{chat_id = ChatId}, c2s_chat_get_info), Transport),
    timer:sleep(50),
    receive_packet(ConnPid, Transport).

send_message(ConnPid, Transport, ChatId, MsgBody) ->
    send_packet(ConnPid, ?R2M(#c2s_message_send{chat_id = ChatId, msg_body = MsgBody}, c2s_message_send), Transport),
    timer:sleep(50),
    case receive_packet(ConnPid, Transport) of
        #{<<"msg_type">> := ?S2C_MESSAGE_SEND_RESULT_TYPE, <<"msg_id">> := MsgId, <<"chat_id">> := ChatId} -> {MsgId, ChatId};
        Else -> Else
    end.

receive_packets_until(ConnPid, Transport, Predicate) when is_function(Predicate) ->
    receive_packets_until(ConnPid, Transport, Predicate, []).
receive_packets_until(ConnPid, Transport, Predicate, Acc) ->
    case receive_packet(ConnPid, Transport) of
        {error, _} -> Acc;
        Answer ->
            case Predicate(Answer) of
                true -> [Answer | Acc];
                _ -> receive_packets_until(ConnPid, Transport, Predicate, [Answer | Acc])
            end
    end.


%%====================================================================
%% Internal functions
%%====================================================================
