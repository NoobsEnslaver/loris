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
        ,access_level/0
        ,terminate/1]).

-record(user_state, {chats, rooms, token, muted_chats, msisdn}).

default_user_state(Token)->
    Session = sessions:get(Token),
    UserMSISDN = sessions:extract(Session, owner_id),
    User = users:get(UserMSISDN),
    lists:foreach(fun({C, _AccessGroup})->
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
    #c2s_chat_create{name = Name, users = [round(U) || U <- Users]};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_LEAVE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_leave{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_DELETE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_delete{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_INVITE_USER_TYPE, <<"chat_id">> := ChatId, <<"user_msisdn">> := MSISDN}) ->
    #c2s_chat_invite_user{chat_id = ChatId, user_msisdn = round(MSISDN)};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_MUTE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_mute{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_UNMUTE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_unmute{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_TYPING_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_typing{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_MESSAGE_SEND_TYPE, <<"chat_id">> := ChatId, <<"msg_body">> := MsgBody}) ->
    #c2s_message_send{chat_id = ChatId, msg_body = MsgBody};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_MESSAGE_GET_LIST_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId})->
    #c2s_message_get_list{chat_id = ChatId, msg_id = round(MsgId)};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_MESSAGE_UPDATE_TYPE, <<"chat_id">> := ChatId, <<"msg_body">> := MsgBody, <<"msg_id">> := MsgId}) ->
    #c2s_message_update{chat_id = ChatId, msg_body = MsgBody, msg_id = MsgId};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgIdList}) ->
    #c2s_message_update_status{chat_id = ChatId, msg_id = MsgIdList};
unwrap_msg(_Msg = #{<<"msg_type">> := ?C2S_SYSTEM_LOGOUT_TYPE}) -> #c2s_system_logout{};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_GET_INFO_TYPE, <<"user_msisdn">> := MSISDN}) ->
    #c2s_user_get_info{user_msisdn = round(MSISDN)};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_GET_STATUS_TYPE, <<"user_msisdn">> := MSISDN}) ->
    #c2s_user_get_status{user_msisdn = round(MSISDN)};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_USER_SET_INFO_TYPE}) ->
    #c2s_user_set_info{fname = maps:get(<<"fname">>, Msg, 'undefined')
                      ,lname = maps:get(<<"lname">>, Msg, 'undefined')
                      ,age = maps:get(<<"age">>, Msg, 'undefined')
                      ,is_male = maps:get(<<"is_male">>, Msg, 'undefined')};
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
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_ACCEPT_INVATATION_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_accept_invatation{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_REJECT_INVATATION_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_reject_invatation{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_INVITE_TYPE, <<"msisdn">> := MSISDN, <<"sdp_offer">> := Offer}) ->
    #c2s_call_invite{msisdn = MSISDN, sdp_offer = Offer};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_ACK_TYPE}) ->
    #c2s_call_ack{};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_ICE_CANDIDATE_TYPE, <<"candidate">> := Candidate}) ->
    #c2s_call_ice_candidate{candidate = Candidate};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_BYE_TYPE, <<"code">> := Code}) ->
    #c2s_call_bye{code = Code};
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
    transport_lib:encode(?R2M(_Msg, s2c_chat_create_result), Transport);
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
wrap_msg(_Msg = #s2c_message_send_result{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_message_send_result), Transport);
wrap_msg(Msg = #s2c_message_list{messages = Messages}, Transport) ->
    MapMessages = [?R2M(M, message) || M <- Messages],
    transport_lib:encode(?R2M(Msg#s2c_message_list{messages = MapMessages}, s2c_message_list), Transport);
wrap_msg(_Msg = #s2c_call_invite{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_call_invite), Transport);
wrap_msg(_Msg = #s2c_call_ack{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_call_ack), Transport);
wrap_msg(_Msg = #s2c_call_ice_candidate{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_call_ice_candidate), Transport);
wrap_msg(_Msg = #s2c_call_bye{}, Transport) ->
    transport_lib:encode(?R2M(_Msg, s2c_call_bye), Transport);
wrap_msg({error, Msg}, Transport) ->
    lager:error("Can't wrap message: unknown type. Msg: ~p", [Msg]),
    transport_lib:encode(?R2M(#s2c_error{code = 500}, s2c_error), Transport);
wrap_msg(_, Transport) ->
    transport_lib:encode(?R2M(#s2c_error{code = 500}, s2c_error), Transport).



%%%===================================================================
%%% Handle users request
%%%===================================================================
-spec do_action(client_msg_type(), #user_state{}) -> {'ok', #user_state{}} | {'async', pid(), reference(), #user_state{}} | {Msg :: server_msg_type(), #user_state{}}.
do_action(#c2s_chat_get_list{}, #user_state{chats = Chats} = State) ->
    ChatsList = [{ChatId, chat_info:extract(chat_info:get(ChatId), name)} || {ChatId, _} <- Chats],
    Resp = #s2c_chat_list{chats = maps:from_list(ChatsList)},
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
    case chats:accept_invatation(ChatId, MSISDN) of
        'not_exists' -> {#s2c_error{code = 404}, State};
        AccessGroup -> {ok, State#user_state{chats = [{ChatId, AccessGroup} | OldChats]}}
    end;
do_action(#c2s_chat_reject_invatation{chat_id = ChatId}, #user_state{msisdn = MSISDN} = State) ->
    Resp = case chats:reject_invatation(ChatId, MSISDN) of
               'not_exists' -> #s2c_error{code = 404};
               _Ok -> ok
           end,
    {Resp, State};
do_action(#c2s_chat_invite_user{chat_id = ChatId, user_msisdn = MSISDN}, #user_state{chats = Chats} = State) ->
    Resp = case proplists:get_value(ChatId, Chats) of
               'administrators' ->
                   chats:invite_to_chat(ChatId, MSISDN, 'users'),
                   ok;
               'undefined' -> #s2c_error{code = 404};
               _ -> #s2c_error{code = 403}
           end,
    {Resp, State};
do_action(#c2s_chat_mute{chat_id = ChatId}, #user_state{msisdn = MSISDN, muted_chats = MC} = State) ->
    NewState = case lists:member(ChatId, MC) of
                   'true' ->
                       State;
                   'false'->
                       users:mute_chat(MSISDN, ChatId),
                       State#user_state{muted_chats = [ChatId | MC]}
               end,
    {ok, NewState};
do_action(#c2s_chat_unmute{chat_id = ChatId}, #user_state{msisdn = MSISDN, muted_chats = MC} = State) ->
    NewState = case lists:member(ChatId, MC) of
                   'true' ->
                       users:unmute_chat(MSISDN, ChatId),
                       State#user_state{muted_chats = MC -- [ChatId]};
                   'false'->
                       State
               end,
    {ok, NewState};
do_action(#c2s_chat_typing{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = Chats} = State) ->
    case proplists:get_value(ChatId, Chats) of
        'undefined' -> ok;
        _ -> chats:typing(ChatId, MSISDN)
    end,
    {ok, State};
do_action(#c2s_message_send{chat_id = ChatId, msg_body = MsgBody}, #user_state{msisdn = MSISDN, chats = Chats} = State) ->
    Resp = case proplists:get_value(ChatId, Chats) of
               'undefined' ->
                   #s2c_error{code = 404};
               _ ->
                   MsgId = chats:send_message(ChatId, MsgBody, MSISDN),
                   #s2c_message_send_result{chat_id = ChatId, msg_id = MsgId}
           end,
    {Resp, State};
do_action(#c2s_message_get_list{chat_id = ChatId, msg_id = MsgId}, #user_state{chats = Chats} = _State) ->
    Resp = case proplists:get_value(ChatId, Chats) of
               'undefined' ->
                   #s2c_error{code = 403};
               _AccessGroup ->
                   Messages = chats:get_messages_by_id(ChatId, MsgId),
                   #s2c_message_list{messages = Messages}
           end,
    {Resp, _State};
do_action(#c2s_message_update{chat_id = ChatId, msg_id = MsgId, msg_body = MsgBody}, #user_state{chats = Chats, msisdn = MSISDN} = _State) ->
    Resp = case proplists:get_value(ChatId, Chats) of
               undefined->
                   #s2c_error{code = 404};
               _AccessGroup ->
                   case chats:update_message(ChatId, MsgId, MsgBody, MSISDN) of
                       ok -> ok;
                       _ -> #s2c_error{code = 403}
                   end
           end,
    {Resp, _State};
do_action(#c2s_message_update_status{chat_id = ChatId, msg_id = MsgIdList}, #user_state{chats = Chats} = _State) ->
    Resp = case proplists:get_value(ChatId, Chats) of
               'undefined' ->
                   #s2c_error{code = 403};
                _ ->
                   chats:update_message_status(ChatId, MsgIdList),
                   ok
           end,
    {Resp, _State};
do_action(_Msg = #c2s_system_logout{}, _State) ->
    {ok, _State};
do_action(#c2s_user_get_info{user_msisdn = MSISDN}, _State) ->
    Resp = case users:get(MSISDN) of
               #user{fname = FName, lname = LName, age = Age, is_male = IsMale} ->
                   #s2c_user_info{user_msisdn = MSISDN, fname = FName, lname = LName, age = Age, is_male = IsMale};
               'false' ->
                   #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_user_get_status{user_msisdn = MSISDN}, _State) ->
    Resp = case sessions:get_by_owner_id(MSISDN) of
               'false' ->
                   case users:get(MSISDN) of
                       'false' ->
                           #s2c_error{code = 404};
                       User ->
                           #s2c_user_status{user_msisdn = MSISDN, is_online = 'false', last_visit_timestamp = users:extract(User, last_visit_timestamp)}
                   end;
               Session ->
                   case sessions:extract(Session, ws_pid) of
                       'undefined' ->
                           case users:get(MSISDN) of
                               'false' ->
                                   #s2c_error{code = 404};
                               User ->
                                   #s2c_user_status{user_msisdn = MSISDN, is_online = 'false', last_visit_timestamp = users:extract(User, last_visit_timestamp)}
                           end;
                       _Pid ->
                           #s2c_user_status{user_msisdn = MSISDN, is_online = 'true', last_visit_timestamp = common:timestamp()}
                   end
           end,
    {Resp, _State};
do_action(#c2s_user_set_info{fname = FName, lname = LName, age = Age, is_male = IsMale}, #user_state{msisdn = MSISDN} = _State) ->
    Info = lists:filter(fun({_,'undefined'}) -> 'false';
                           ({_,_})-> 'true'
                        end, [{fname, FName}, {lname, LName}, {age, Age}, {is_male, IsMale}]),
    users:set_info(MSISDN, Info),
    {ok, _State};
do_action(#c2s_user_search{fname = FName, lname = LName}, _State) ->
    Users = users:search(FName, LName),
    Resp = #s2c_user_search_result{users = Users},
    {Resp, _State};
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
do_action(#c2s_call_invite{}, _State) ->
    {ok, _State};
do_action(#c2s_call_ack{}, _State) ->
    {ok, _State};
do_action(#c2s_call_ice_candidate{}, _State) ->
    {ok, _State};
do_action(#c2s_call_bye{}, _State) ->
    {ok, _State};
do_action({chat_typing, _ChatId, MSISDN}, #user_state{msisdn = MSISDN} = State) -> %you self typing, ignore
    {ok, State};
do_action({chat_typing, ChatId, MSISDN}, #user_state{muted_chats = MC} = State) ->
    Resp = case lists:member(ChatId, MC) of
               'true' -> ok;
               'false'-> #s2c_chat_typing{chat_id = ChatId, user_msisdn = MSISDN}
           end,
    {Resp, State};
do_action({chat_delete, ChatId, MSISDN}, #user_state{chats = Chats, msisdn = MyMSISDN} = State) ->
    chats:unsubscribe(ChatId),
    Resp = case MSISDN == MyMSISDN of
               'true' ->
                   ok;
               'false'->
                   #s2c_message{chat_id = ChatId, msg_body = <<"@system:delete_chat">>, timestamp = common:timestamp(), status = 'pending', msg_id = 0, from = MSISDN}
           end,
    {Resp, State#user_state{chats = proplists:delete(ChatId, Chats)}};
do_action({chat_invatation, ChatId}, _State) ->
    Resp = #s2c_chat_invatation{chat_id = ChatId},
    {Resp, _State};

do_action({mnesia_table_event, {write, _Table, #message{from = MSISDN}, [], _ActivityId}}, #user_state{msisdn = MSISDN} = _State) -> %it's my own message, ignore
    {ok, _State};
%% do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, from = MSISDN}, [], _ActivityId}}, #user_state{msisdn = MSISDN} = _State) -> %it's my own message, send only msg_id and chat_id
%%     <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
%%     Resp = #s2c_message_send_result{chat_id = ChatId, msg_id = MsgId},
%%     {Resp, _State};
do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, msg_body = MsgBody, timestamp = TimeStamp, status = Status, from = From}, [], _ActivityId}}, _State) -> %if no old msg, that's new message
    <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
    Resp = #s2c_message{chat_id = ChatId, msg_body = MsgBody, timestamp = TimeStamp, status = Status, msg_id = MsgId, from = From},
    {Resp, _State};
do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, status = Status, msg_body = MsgBody, from = From}, [#message{msg_id = MsgId, status = Status}], _ActivityId}}, #user_state{msisdn = MSISDN} = _State) -> %if msg statuses are equal, that's msg_body update
    <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
    Resp = case MSISDN == From of
               'true' ->
                   ok;
               'false'->
                   #s2c_message_update{chat_id = ChatId, msg_id = MsgId, msg_body = MsgBody}
           end,
    {Resp, _State};
do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId}, [#message{msg_id = MsgId}], _ActivityId}}, _State) -> %else, that's msg_status update
    <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
    Resp = #s2c_message_update_status{chat_id = ChatId, msg_id = MsgId},
    {Resp, _State};
do_action(_Msg, _State) ->
    lager:info("unknown message type: ~p", [_Msg]),
    {{error, <<"unknown message">>}, _State}.

terminate(#user_state{msisdn = MSISDN, token = Token} = _State) ->
    sessions:bind_pid_to_session(Token, 'undefined'),
    users:update_last_visit_timestamp(MSISDN),
    ok.

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
