%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created : 7 Apr 2017
%%%-------------------------------------------------------------------
-module(ws_chat_protocol_v1).
-include("ws_chat_protocol_v1_messages.hrl").
-include("server.hrl").
-include_lib("common/include/tables.hrl").
-behaviour(ws_protocol_behaviour).
-export([unwrap_msg/1
        ,do_action/2
        ,wrap_msg/2
        ,default_user_state/1
        ,allowed_groups/0
        ,access_level/0]).

-record(user_state, {chats, rooms, token, muted_chats, msisdn}).

default_user_state(Token)->
    Session = sessions:get(Token),
    UserMSISDN = sessions:extract(Session, owner_id),
    User = users:get(UserMSISDN),
    lists:map(fun({C, _AccessGroup})->
                      chats:subscribe(C)
              end, users:extract(User, chats)),
    #user_state{chats = users:extract(User, chats)
               ,msisdn = users:extract(User, msisdn)
               ,rooms = users:extract(User, rooms)
               ,token = sessions:extract(Session, token)
               ,muted_chats = users:extract(User, muted_chats)}.

%%%===================================================================
%%% Parse users message
%%%===================================================================
-spec unwrap_msg(map()) -> client_msg_type() | 'undefined'.
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_GET_LIST_TYPE}) ->
    #c2s_chat_get_list{};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_GET_INFO_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_get_info{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_CREATE_TYPE, <<"name">> := Name, <<"users">> := Users}) ->
    #c2s_chat_create{name = Name, users = Users};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_LEAVE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_leave{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_DELETE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_delete{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_INVITE_USER_TYPE, <<"chat_id">> := ChatId, <<"user_msisdn">> := MSISDN}) ->
    #c2s_chat_invite_user{chat_id = ChatId, user_msisdn = MSISDN};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_MUTE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_mute{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_UNMUTE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_unmute{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_TYPING_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_typing{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_MESSAGE_SEND_TYPE, <<"chat_id">> := ChatId, <<"msg_body">> := MsgBody}) ->
    #c2s_message_send{chat_id = ChatId, msg_body = MsgBody};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_MESSAGE_GET_LIST_TYPE})-> #c2s_message_get_list{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_MESSAGE_UPDATE_TYPE}) -> #c2s_message_update{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_MESSAGE_UPDATE_STATUS_TYPE}) -> #c2s_message_update_status{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_SYSTEM_LOGOUT_TYPE}) -> #c2s_system_logout{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_USER_GET_INFO_TYPE}) -> #c2s_user_get_info{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_USER_GET_STATUS_TYPE}) -> #c2s_user_get_status{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_USER_SET_INFO_TYPE}) -> #c2s_user_set_info{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_USER_SEARCH_TYPE}) -> #c2s_user_search{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_GET_TREE_TYPE}) -> #c2s_room_get_tree{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_GET_INFO_TYPE}) -> #c2s_room_get_info{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_RENAME_TYPE}) -> #c2s_room_rename{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_ADD_USER_TYPE}) -> #c2s_room_add_user{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_DEL_USER_TYPE}) -> #c2s_room_del_user{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_ADD_SUBROOM_TYPE}) -> #c2s_room_add_subroom{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_CREATE_TYPE}) -> #c2s_room_create{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_DELETE_TYPE}) -> #c2s_room_delete{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_ENTER_TO_CHAT_TYPE}) -> #c2s_room_enter_to_chat{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_ROOM_SEND_MESSAGE_TYPE}) -> #c2s_room_send_message{};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_CHAT_ACCEPT_INVATATION_TYPE, <<"chat_id">> := ChatId}) -> #c2s_chat_accept_invatation{chat_id = ChatId};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_CHAT_REJECT_INVATATION_TYPE, <<"chat_id">> := ChatId}) -> #c2s_chat_reject_invatation{chat_id = ChatId};
unwrap_msg(_) -> 'undefined'.


%%%===================================================================
%%% Prepare server response
%%%===================================================================
-spec wrap_msg(server_msg_type(), binary()) -> binary().
wrap_msg(#async_start{work_id = WorkId}, Transport) ->
    Data = #{<<"msg_type">> => ?C2S_ASYNC_START
            ,<<"req_id">> => WorkId},
    transport_lib:encode(Data, Transport);
wrap_msg(#async_error{work_id = WorkId, error_code = EC}, Transport) ->
    Data = #{<<"msg_type">> => ?C2S_ASYNC_ERROR
            ,<<"req_id">> => WorkId
            ,<<"data">> => #{<<"error_code">> => EC}},
    transport_lib:encode(Data, Transport);
wrap_msg(#async_done{work_id= WorkId, result = Res}, Transport) ->
    Data = #{<<"msg_type">> => ?C2S_ASYNC_DONE
            ,<<"req_id">> => WorkId
            ,<<"data">> => transport_lib:decode(wrap_msg(Res, Transport), Transport)},
    transport_lib:encode(Data, Transport);
wrap_msg(_Msg = #s2c_chat_list{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_chat_list), Transport);
wrap_msg(_Msg = #s2c_chat_info{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_chat_info), Transport);
wrap_msg(_Msg = #s2c_chat_create_result{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_chat_list), Transport);
wrap_msg(_Msg = #s2c_chat_typing{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_chat_typing), Transport);
wrap_msg(_Msg = #s2c_message{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_message), Transport);
wrap_msg(_Msg = #s2c_message_update{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_message_update), Transport);
wrap_msg(_Msg = #s2c_message_update_status{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_message_update_status), Transport);
wrap_msg(_Msg = #s2c_user_info{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_user_info), Transport);
wrap_msg(_Msg = #s2c_user_status{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_user_status), Transport);
wrap_msg(_Msg = #s2c_user_search_result{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_user_search_result), Transport);
wrap_msg(_Msg = #s2c_room_list{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_room_list), Transport);
wrap_msg(_Msg = #s2c_room_info{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_room_info), Transport);
wrap_msg(_Msg = #s2c_room_tree{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_room_tree), Transport);
wrap_msg(_Msg = #s2c_room_create_result{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_room_create_result), Transport);
wrap_msg(_Msg = #s2c_chat_invatation{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_chat_invatation), Transport);
wrap_msg(_Msg = #s2c_error{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_error), Transport);
wrap_msg({error, Msg}, Transport) ->
    lager:error("Can't wrap message: unknown type. Msg: ~p", [Msg]),
    transport_lib:encode(?R2M(#s2c_error{code = 500}, s2c_error), Transport).


%%%===================================================================
%%% Handle users request
%%%===================================================================
-spec do_action(client_msg_type(), #user_state{}) -> {'ok', #user_state{}} | {'async', pid(), reference(), #user_state{}} | {Msg :: server_msg_type(), #user_state{}}.
do_action(#c2s_chat_get_list{}, #user_state{chats = Chats} = State) ->
    Resp = #s2c_chat_list{chat_id = [C || {C, _} <- Chats]},
    {Resp, State};
do_action(#c2s_chat_get_info{chat_id = ChatId}, #user_state{chats = MyChats, muted_chats = MC} = State) ->
    Resp = case chat_info:get(ChatId) of
               'false' ->
                   #s2c_error{code = 404};
               ChatInfo ->
                   #s2c_chat_info{chat_id = ChatId
                                 ,name = chat_info:extract(ChatInfo, name)
                                 ,users = chat_info:extract(ChatInfo, users)
                                 ,is_muted = lists:member(ChatId, MC)
                                 ,chat_owner = chat_info:extract(ChatInfo, chat_owner)
                                 ,access_group = proplists:get_value(ChatId, MyChats)}
           end,
    {Resp, State};
do_action(#c2s_chat_create{name = ChatName, users = Users}, #user_state{msisdn = MSISDN, chats = OldChats} = State) ->
    ChatId = chats:new(),
    chat_info:new(ChatId, ChatName, MSISDN),
    users:invite_to_chat(ChatId, MSISDN, 'administrators'),
    users:accept_invatation(ChatId, MSISDN),
    chats:subscribe(ChatId),
    lists:foreach(fun(U)->
                          chats:invite_to_chat(ChatId, U, 'users')
                  end, Users),
    Resp = #s2c_chat_create_result{chat_id = ChatId},
    {Resp, State#user_state{chats = [{ChatId, 'administrators'} | OldChats]}};
do_action(#c2s_chat_leave{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = OldChats} = State) ->
    case proplists:get_value(ChatId, OldChats) of
        'undefined' ->
            {#s2c_error{code = 404}, State};
        _ ->
            chats:leave_chat(ChatId, MSISDN),
            {ok, State#user_state{chats = proplists:delete(ChatId, OldChats)}}
    end;
do_action(#c2s_chat_delete{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = Chats} = State) ->
    Resp = case proplists:get_value(ChatId, Chats) of
               'administrators' ->
                   chats:delete(ChatId, MSISDN);
               'undefined' ->
                   #s2c_error{code = 404};
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, State};
do_action(#c2s_chat_accept_invatation{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = OldChats} = State) ->
    chats:subscribe(ChatId),
    chats:accept_invatation(ChatId, MSISDN),
    {ok, State#user_state{chats = [{ChatId, 'users'} | OldChats]}};
do_action(#c2s_chat_reject_invatation{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = OldChats} = State) ->
    chats:reject_invatation(ChatId, MSISDN),
    {ok, State#user_state{chats = lists:delete(ChatId, OldChats)}};
do_action(#c2s_chat_invite_user{chat_id = ChatId, user_msisdn = MSISDN}, #user_state{chats = Chats} = State) ->
    Resp = case proplists:get_value(ChatId, Chats) of
               'administrators' ->
                   chats:invite_to_chat(ChatId, MSISDN, 'users'),
                   ok;
               'undefined' -> #s2c_error{code = 404};
               _ -> #s2c_error{code = 403}
           end,
    {Resp, State};
do_action(_Msg = #c2s_chat_mute{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_chat_unmute{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_chat_typing{}, _State) ->
    Resp = #s2c_chat_typing{},
    {Resp, _State};
do_action(_Msg =  #c2s_message_send{chat_id = ChatId, msg_body = MsgBody}, #user_state{msisdn = MSISDN} = State) ->
    chats:send_message(ChatId, MsgBody, MSISDN),
    {ok, State};
do_action(_Msg =  #c2s_message_get_list{}, _State) ->
    Resp = #s2c_user_info{},
    {Resp, _State};
do_action(_Msg =  #c2s_message_update{}, _State) ->
    Resp = #s2c_user_status{},
    {Resp, _State};
do_action(_Msg = #c2s_system_logout{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_user_get_info{}, _State) ->
    Resp = #s2c_user_search_result{},
    {Resp, _State};
do_action(_Msg = #c2s_user_get_status{}, _State) ->
    Resp = #s2c_room_list{},
    {Resp, _State};
do_action(_Msg = #c2s_user_set_info{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_user_search{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_room_get_tree{}, _State) ->
    Resp = #s2c_room_tree{},
    {Resp, _State};
do_action(_Msg = #c2s_room_get_info{}, _State) ->
    Resp = #s2c_room_info{},
    {Resp, _State};
do_action(_Msg = #c2s_room_rename{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_room_add_user{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_room_del_user{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_room_add_subroom{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_room_create{}, _State) ->
    Resp = #s2c_room_create_result{},
    {Resp, _State};
do_action(_Msg = #c2s_room_delete{}, _State) ->
    {ok, _State};
do_action(_Msg = #c2s_room_enter_to_chat{}, _State) ->
    Resp = #s2c_chat_info{},
    {Resp, _State};
do_action(_Msg = #c2s_room_send_message{}, _State) ->
    {ok, _State};
do_action({chat_delete, ChatId}, #user_state{chats = Chats} = State) ->
    chats:unsubscribe(ChatId),
    {ok, State#user_state{chats = proplists:delete(ChatId, Chats)}};
do_action({chat_invatation, ChatId}, _State) ->
    Resp = #s2c_chat_invatation{chat_id = ChatId},
    {Resp, _State};
do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, msg_body = MsgBody, timestamp = TimeStamp, status = Status, from = From} = _NewMsg, _OldRecords, _ActivityId}}, _State) ->
    <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
    Resp = #s2c_message{chat_id = ChatId, msg_body = MsgBody, timestamp = TimeStamp, status = Status, msg_id = MsgId, from = From},
    {Resp, _State};
do_action(_Msg, _State) ->
    lager:info("unknown message type: ~p", [_Msg]),
    {{error, <<"unknown message">>}, _State}.

%%%===================================================================
%%% Module access params
%%%===================================================================
allowed_groups() ->
    ['users', 'administrators'].

access_level() ->
    10.

%%%===================================================================
%%% Internal functions
%%%===================================================================
