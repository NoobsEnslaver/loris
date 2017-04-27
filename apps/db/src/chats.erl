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
        ,send_message/3
        ,update_message/3
        ,update_message_status/2
        ,get_messages_from/2, get_messages_from/3
        ,subscribe/1
        ,unsubscribe/1
        ,invite_to_chat/3
        ,accept_invatation/2
        ,reject_invatation/2
        ,leave_chat/2
        ,delete/2
        ]).


new() ->
    Id = get_unique_chat_id(), %TODO: do it in transaction
    Name = erlang:binary_to_atom(<<"chat_", Id/binary>>, 'utf8'),
    Opts = [{attributes, record_info(fields, 'message')}
           ,{record_name, 'message'}
           ,{disc_copies,[node() | nodes()]}],
    mnesia:create_table(Name, Opts),
    Id.

send_message(TableId, MsgBody, From) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    TimeStamp = common:timestamp(),
    Id = mnesia:dirty_update_counter('index', TableName, 1),
    {atomic, Res} = mnesia:transaction(fun()->
                                               mnesia:write(TableName, #message{msg_id = Id
                                                                               ,msg_body = MsgBody
                                                                               ,timestamp = TimeStamp
                                                                               ,status = 'pending'
                                                                               ,from = From}, 'write')
                                       end),
    Res.

update_message(TableId, MsgId, MsgBody) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    {atomic, Res} = mnesia:transaction(fun()->
                                               [OldMsg] = mnesia:read(TableName, MsgId),
                                               NewMsg = OldMsg#message{msg_body = MsgBody},
                                               mnesia:write(TableName, NewMsg, 'write')
                                       end),
    Res.

update_message_status(TableId, MsgId) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    {atomic, Res} = mnesia:transaction(fun()->
                                               [#message{status = OldStatus}] = [OldMsg] = mnesia:read(TableName, MsgId),
                                               case OldStatus of
                                                   'delivered' ->
                                                       mnesia:write(TableName, OldMsg#message{status = 'readed'}, 'write'),
                                                       'readed';
                                                   'readed' ->
                                                       'readed'; %no update
                                                   _ ->
                                                       mnesia:write(TableName, OldMsg#message{status = 'delivered'}, 'write'),
                                                       'delivered'
                                               end
                                       end),
    Res.

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
    users:accept_invatation(ChatId, MSISDN),
    chat_info:add_user(ChatId, MSISDN),
    subscribe(ChatId),
    send_message(ChatId, <<"@system:accept_invatation">>, MSISDN),
    ok.

reject_invatation(ChatId, MSISDN) ->
    users:reject_invatatoin(ChatId, MSISDN),
    send_message(ChatId, <<"@system:reject_invatation">>, MSISDN),
    ok.

delete(ChatId, MSISDN) ->
    TableInfo = table_info:get(ChatId),
    TableName = erlang:binary_to_atom(<<"chat_", ChatId/binary>>, 'utf8'),
    case table_info:extract(ChatId, owner_id) of
        MSISDN ->
            notify_all_online_chat_users(ChatId, {chat_delete, ChatId}),
            send_message(ChatId, <<"@system:delete_chat">>, MSISDN),
            timer:sleep(50),
            Users = table_info:extract(TableInfo, users),
            lists:map(fun(U)->
                              users:leave_chat(ChatId, U)
                      end, Users),
            mnesia:delete_table(TableName),
            table_info:delete(ChatId),
            ok;
        _ -> 'forbidden'
    end.

leave_chat(ChatId, MSISDN) ->
    send_message(ChatId, <<"@system:leave_chat">>, MSISDN),
    unsubscribe(ChatId),
    users:leave_chat(ChatId, MSISDN),
    chat_info:remove_user(ChatId, MSISDN),
    ok.

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
    lists:map(fun(U)->
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
