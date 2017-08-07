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
        ,get_messages_by_id/4
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

-define(MAY_READ(AL), ((AL rem 4) rem 2) == 1).

new() ->
    Id = get_unique_chat_id(), %TODO: do it in transaction
    Name = erlang:binary_to_atom(<<"chat_", Id/binary>>, 'utf8'),
    Opts = [{attributes, record_info(fields, 'message')}
           ,{record_name, 'message'}
           ,{disc_copies,[node() | nodes()]}
           ,{type, ordered_set}],               %For fastest message grabbing by period
    case mnesia:create_table(Name, Opts) of
        {atomic, ok} -> {ok, Id};
        _ -> 'false'
    end.

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
    Id = common:timestamp(),
    Fun = fun()->
                  mnesia:dirty_write(TableName, #message{msg_id = Id
                                                        ,msg_body = MsgBody
                                                        ,status = 'pending'
                                                        ,from = From})
          end,
    mnesia:sync_dirty(Fun),
    Id.

update_message(TableId, MsgId, MsgBody, MSISDN) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    Fun = fun()->
                  case mnesia:dirty_read(TableName, MsgId) of
                      [#message{from = MSISDN} = OldMsg] ->
                          NewMsg = OldMsg#message{msg_body = MsgBody},
                          mnesia:dirty_write(TableName, NewMsg);
                      _ -> false
                  end
          end,
    mnesia:sync_dirty(Fun).

update_message_status(TableId, MsgIdList) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    Fun = fun()->                               %TODO: optimize it with custom lock and dirty operations
                  lists:foreach(fun(MsgId)->
                                        case mnesia:dirty_read(TableName, MsgId) of
                                            [#message{status = OldStatus}] = [OldMsg] ->
                                                case OldStatus of
                                                    'delivered' ->
                                                        mnesia:dirty_write(TableName, OldMsg#message{status = 'read'}),
                                                        'read';
                                                    'read' ->
                                                        'read'; %no update
                                                    _ ->
                                                        mnesia:dirty_write(TableName, OldMsg#message{status = 'delivered'}),
                                                        'delivered'
                                                end;
                                            _ -> ok
                                        end
                                end, MsgIdList)
          end,
    mnesia:sync_dirty(Fun).

-spec get_messages_by_id(binary(), non_neg_integer() | 'undefined', non_neg_integer(), 'up' | 'down') -> [#message{}].
get_messages_by_id(ChatId, 'undefined', Count, _Direction) ->
    get_messages_by_id(ChatId, common:timestamp(), Count, 'up');
get_messages_by_id(ChatId, MsgId, Count, Direction) ->
    TableName = erlang:binary_to_atom(<<"chat_", ChatId/binary>>, 'utf8'),
    Q = case Direction of
            'up'  -> qlc:sort(qlc:q([M || M <- mnesia:table(TableName), M#message.msg_id < MsgId]), [{'order', 'descending'}]);
            'down'-> qlc:q([M || M <- mnesia:table(TableName), M#message.msg_id > MsgId])
        end,
    Fun = common:get_limited_amount_from_query(Q, Count),
    mnesia:sync_dirty(Fun).

subscribe(TableId) ->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    mnesia:subscribe({table, TableName, detailed}).

unsubscribe(TableId)->
    TableName = erlang:binary_to_atom(<<"chat_", TableId/binary>>, 'utf8'),
    mnesia:unsubscribe({table, TableName, detailed}).

invite_to_chat(ChatId, UserMSISDN, AL) ->
    users:invite_to_chat(ChatId, UserMSISDN, AL),
    send_message(ChatId, <<"@system:invite_to_chat">>, UserMSISDN),
    case users:get_pid(UserMSISDN) of
        WsPid when is_pid(WsPid)->
            WsPid ! {chat_invatation, ChatId, AL};
        _ ->
            'ok'
    end.

accept_invatation(ChatId, MSISDN) ->
    case users:accept_invatation(ChatId, MSISDN) of
        'not_exists' -> 'not_exists';
        AL when ?MAY_READ(AL) ->
            chat_info:add_user(ChatId, MSISDN),
            subscribe(ChatId),
            send_message(ChatId, <<"@system:accept_invatation">>, MSISDN),
            AL;
        AL ->
            chat_info:add_user(ChatId, MSISDN),
            send_message(ChatId, <<"@system:accept_invatation">>, MSISDN),
            AL
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
    mnesia:sync_dirty(fun()-> mnesia:dirty_last(TableName) end).

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
                          case users:get_pid(U) of
                              WsPid when is_pid(WsPid)->
                                  WsPid ! Msg;
                              _ ->
                                  'ok'
                          end
                  end, Users).
