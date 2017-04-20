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
-compile({no_auto_import,[get/1]}).
-export([new/0
        ,send_message/2
        ,update_message/3
        ,update_message_status/2
        ,get_messages_from/2, get_messages_from/3
        ,subscribe/1
        ,unsubscribe/1
        ]).


new() ->
    Id = get_unique_chat_id(), %TODO: do it in transaction
    Name = erlang:binary_to_atom(<<"chat_", Id/binary>>, 'utf8'),
    Opts = [{attributes, record_info(fields, 'message')}
           ,{record_name, 'message'}
           ,{disc_copies,[node() | nodes()]}],
    mnesia:create_table(Name, Opts),
    Id.

send_message(TableId, MsgBody) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    TimeStamp = common:timestamp(),
    Id = mnesia:dirty_update_counter('index', TableName, 1),
    {atomic, Res} = mnesia:transaction(fun()->
                                               mnesia:write(TableName, #message{msg_id = Id
                                                                               ,msg_body = MsgBody
                                                                               ,timestamp = TimeStamp
                                                                               ,status = 'pending'}, 'write')
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
    mnesia:subscribe({table, TableName, simple}).

unsubscribe(TableId)->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    mnesia:unsubscribe({table, TableName, simple}).

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
