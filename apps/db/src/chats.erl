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
-compile({no_auto_import,[get/1]}).
-export([new/0
        ,send_message/2
        ,update_message/3
        ,update_message_status/2
        ,get_messages_from/2, get_messages_from/3
        ,subscribe/2
        ]).


new() ->
    Id = get_unique_chat_id(),                  %TODO: do it in transaction
    Name = erlang:binary_to_atom(<<"chat_", Id/integer>>),
    Opts = [{attributes, record_info(fields, 'message')}
           ,{record_name, 'message'}
           ,{disc_copies,[node() | nodes()]}],
    mnesia:transaction(fun()->
                               mnesia:create_table(Name, Opts)
                       end),
    Id.

send_message(TableId, MsgBody) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/integer>>),
    TimeStamp = common:timestamp(),
    Id = mnesia:dirty_update_counter('index', TableName, 1),
    mnesia:transaction(fun()->
                               mnesia:write(#message{msg_id = Id
                                                    ,msg_body = MsgBody
                                                    ,timestamp = TimeStamp
                                                    ,status = 'pending'})
                       end).

update_message(_TableId, _MsgId, _MsgBody) ->
    ok.

update_message_status(_TableId, _MsgId) ->
    ok.

get_messages_from(_TableId, _TimeStampFrom) ->
    ok.
get_messages_from(_TableId, _TimeStampFrom, _TimeStampTo) ->
    ok.

subscribe(_TableId, _WSPid) ->
    ok.

%%%===================================================================
%%% internal functions
%%%===================================================================
get_unique_chat_id()->
    RndId = crypto:rand_uniform(100000000, 999999999),
    TmpTableName = erlang:binary_to_atom(<<"chat_", RndId/integer>>),
    case lists:member(TmpTableName, mnesia:system_info(tables)) of
        'true' -> get_unique_chat_id();
        'false'-> RndId
    end.
