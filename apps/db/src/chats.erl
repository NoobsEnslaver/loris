%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  18 Apr 2017
%%%-------------------------------------------------------------------
-module(chats).
-include("db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([new/0
        ,new_p2p/2
        ,send_message/3
        ,update_message/4
        ,update_message_status/2
        ,get_messages_from/2, get_messages_from/3
        ,get_messages_by_id/3
        ,subscribe/1
        ,unsubscribe/1
        ,invite_to_chat/3
        ,accept_invatation/2
        ,reject_invatation/2
        ,leave_chat/2
        ,delete/2
        ,typing/2
        ,get_last_msg_id/1
        ]).


new() ->
    Id = get_unique_chat_id(), %TODO: do it in transaction
    Name = erlang:binary_to_atom(<<"chat_", Id/binary>>, 'utf8'),
    Opts = [{attributes, record_info(fields, 'message')}
           ,{record_name, 'message'}
           ,{disc_copies,[node() | nodes()]}
           ,{type, ordered_set}],               %For fastest message grabbing by period
    mnesia:create_table(Name, Opts),
    Id.

new_p2p(MSISDN1, MSISDN2) ->
    [M1, M2] = [erlang:integer_to_binary(M) || M <- lists:sort([MSISDN1, MSISDN2])],
    Id = <<M1/binary, "_", M2/binary>>,
    Name = erlang:binary_to_atom(<<"chat_", Id/binary>>, 'utf8'),
    Opts = [{attributes, record_info(fields, 'message')}
           ,{record_name, 'message'}
           ,{disc_copies,[node() | nodes()]}
           ,{type, ordered_set}],               %For fastest message grabbing by period
    case mnesia:create_table(Name, Opts) of
        {atomic, ok} -> {ok, Id};
        {aborted, {already_exists, Name}} -> {already_exists, Id};
        _ -> 'false'
    end.

send_message(TableId, MsgBody, From) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    TimeStamp = common:timestamp(),
    Id = mnesia:dirty_update_counter('index', TableName, 1),
    Fun = fun()->
                  mnesia:write(TableName, #message{msg_id = Id
                                                  ,msg_body = MsgBody
                                                  ,timestamp = TimeStamp
                                                  ,status = 'pending'
                                                  ,from = From}, 'write')
          end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> Id;
        _ -> false
    end.

update_message(TableId, MsgId, MsgBody, MSISDN) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    Fun = fun()->
                  [#message{from = MSISDN} = OldMsg] = mnesia:read(TableName, MsgId),
                  NewMsg = OldMsg#message{msg_body = MsgBody},
                  mnesia:write(TableName, NewMsg, 'write')
          end,
    case mnesia:transaction(Fun) of
        {atomic, _Res} -> ok;
        _ -> false
    end.

update_message_status(TableId, MsgIdList) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    Fun = fun()->                               %TODO: optimize it with custom lock and dirty operations
                  lists:foreach(fun(MsgId)->
                                        case mnesia:read(TableName, MsgId) of
                                            [#message{status = OldStatus}] = [OldMsg] ->
                                                case OldStatus of
                                                    'delivered' ->
                                                        mnesia:write(TableName, OldMsg#message{status = 'read'}, 'write'),
                                                        'read';
                                                    'read' ->
                                                        'read'; %no update
                                                    _ ->
                                                        mnesia:write(TableName, OldMsg#message{status = 'delivered'}, 'write'),
                                                        'delivered'
                                                end;
                                            _ -> ok
                                        end
                                end, MsgIdList)
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _ -> 'false'
    end.

get_messages_from(TableId, TimeStampFrom) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    Q = qlc:q([M || M <- mnesia:table(TableName), M#message.timestamp > TimeStampFrom]),
    {atomic, Res} = mnesia:transaction(fun()-> qlc:e(Q) end),
    Res.
get_messages_from(TableId, TimeStampFrom, TimeStampTo) ->       %TODO: optimize it with cursor
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    Q = qlc:q([M || M <- mnesia:table(TableName), M#message.timestamp >  TimeStampFrom
                                                 ,M#message.timestamp =< TimeStampTo]),
    {atomic, Res} = mnesia:transaction(fun()-> qlc:e(Q) end),
    Res.

get_messages_by_id(ChatId, MsgId, Count) ->
    TableName = erlang:binary_to_atom(<<"chat_", ChatId/binary>>, 'utf8'),
    Q = qlc:q([M || M <- mnesia:table(TableName), M#message.msg_id > MsgId
                                                , M#message.msg_id =< MsgId + Count]),
    mnesia:async_dirty(fun()-> qlc:e(Q) end).

subscribe(TableId) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    mnesia:subscribe({table, TableName, detailed}).

unsubscribe(TableId)->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    mnesia:unsubscribe({table, TableName, detailed}).

invite_to_chat(ChatId, UserMSISDN, AccessGroup) ->
    users:invite_to_chat(ChatId, UserMSISDN, AccessGroup),
    send_message(ChatId, <<"@system:invite_to_chat">>, UserMSISDN),
    case sessions:get_by_owner_id(UserMSISDN) of
        #session{ws_pid = WsPid} when is_pid(WsPid)->
            WsPid ! {chat_invatation, ChatId};
        _ ->
            'ok'
    end.

accept_invatation(ChatId, MSISDN) ->
    case users:accept_invatation(ChatId, MSISDN) of
        'not_exists' -> 'not_exists';
        AccessGroup ->
            chat_info:add_user(ChatId, MSISDN),
            subscribe(ChatId),
            send_message(ChatId, <<"@system:accept_invatation">>, MSISDN),
            AccessGroup
    end.

reject_invatation(ChatId, MSISDN) ->
    case users:reject_invatatoin(ChatId, MSISDN) of
        'not_exists' -> 'not_exists';
        _Ok -> send_message(ChatId, <<"@system:reject_invatation">>, MSISDN)
    end.

delete(ChatId, MSISDN) ->
    TableInfo = chat_info:get(ChatId),
    TableName = erlang:binary_to_atom(<<"chat_", ChatId/binary>>, 'utf8'),
    notify_all_online_chat_users(ChatId, {chat_delete, ChatId, MSISDN}),
    Users = chat_info:extract(TableInfo, users),
    lists:foreach(fun(U)->
                          users:leave_chat(ChatId, U)
                  end, Users),
    mnesia:delete_table(TableName),
    chat_info:delete(ChatId),
    ok.

leave_chat(ChatId, MSISDN) ->
    send_message(ChatId, <<"@system:leave_chat">>, MSISDN),
    unsubscribe(ChatId),
    users:leave_chat(ChatId, MSISDN),
    chat_info:remove_user(ChatId, MSISDN),
    ok.

typing(ChatId, MSISDN) ->
    notify_all_online_chat_users(ChatId, {chat_typing, ChatId, MSISDN}).

get_last_msg_id(TableId) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    Fun = fun()->
                  mnesia:dirty_read('index', TableName)
          end,
    case mnesia:async_dirty(Fun) of
        Num when is_number(Num) -> Num;
        _ -> -1
    end.

%%%===================================================================
%%% internal functions
%%%===================================================================
get_unique_chat_id()->
    RndId = common:bin2hex(crypto:strong_rand_bytes(24)),
    TmpTableName = erlang:binary_to_atom(<<"chat_", RndId/binary>>, 'utf8'),
    case lists:member(TmpTableName, mnesia:system_info(tables)) of
        'true' -> get_unique_chat_id();
        'false'-> RndId
    end.

notify_all_online_chat_users(ChatId, Msg)->
    ChatInfo = chat_info:get(ChatId),
    Users = chat_info:extract(ChatInfo, users),
    lists:foreach(fun(U)->
                          case sessions:get_by_owner_id(U) of
                              #session{ws_pid = WsPid} when is_pid(WsPid)->
                                  WsPid ! Msg;
                              _ ->
                                  'ok'
                          end
                  end, Users),
    ok.

%% notify_chat_online_owner(ChatId, Msg)->
%%     ChatInfo = chat_info:get(ChatId),
%%     OwnerId = chat_info:extract(ChatInfo, chat_owner),
%%     case sessions:get_by_owner_id(OwnerId) of
%%         #session{ws_pid = WsPid} when is_pid(WsPid)->
%%             WsPid ! Msg;
%%         _ ->
%%             'ok'
%%     end,
%%     ok.
