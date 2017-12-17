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
        ,wrap_msg/1
        ,default_user_state/1
        ,allowed_groups/0
        ,access_level/0
        ,terminate/1]).

-record(user_state, {chats, muted_chats, msisdn, call, turn_server, storage, group}).
-record(call_info, {pid, msisdn, ref, state, sdp}).

-define(ROOM_TO_ROOM_INFO(R), begin
                                  Tag = rooms:get_tag(R#room.id),
                                  #s2c_room_info{room_id = R#room.id
                                                ,name = R#room.name
                                                ,description = R#room.description
                                                ,tags = Tag#room_tag{name = false, room_id = false}
                                                ,subrooms = R#room.subrooms
                                                ,chat_id = R#room.chat_id
                                                ,room_access = R#room.room_access
                                                ,chat_access = R#room.chat_access
                                                ,additional_info = R#room.additional_info}
                              end).
-define(MAY_ADMIN(AL), AL div 4 == 1).
-define(MAY_WRITE(AL), (AL rem 4) div 2 == 1).
-define(MAY_READ(AL), ((AL rem 4) rem 2) == 1).

default_user_state(MSISDN)->
    User = users:get(MSISDN),
    ChatInvatations = User#user.chats_invatations,
    maps:map(fun(ChatId, AL)->
                     self() ! {chat_invatation, ChatId, AL}
             end, ChatInvatations),
    maps:map(fun(C, AL) when ?MAY_READ(AL) ->
                     chats:subscribe(C);
                (_,_) -> ok
             end, User#user.chats),
    users:notify(MSISDN, 'online'),         %notify all subscribers
    pushes:delete(MSISDN),                  %delete all not sended pushes
    Storage = case storage:get(MSISDN) of
                  'false' -> #{};
                  Val -> Val
              end,
    #user_state{chats = User#user.chats
               ,msisdn = MSISDN
               ,muted_chats = User#user.muted_chats
               ,storage = Storage
               ,group = User#user.group}.

%%%===================================================================
%%% Parse users message
%%%===================================================================
-spec unwrap_msg(map()) -> client_msg_type() | 'undefined'.
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_GET_LIST_TYPE}) ->
    #c2s_chat_get_list{};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_GET_INFO_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_get_info{chat_id = ChatId};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_CHAT_CREATE_TYPE, <<"name">> := Name, <<"users">> := BUsers}) ->
    IsP2P = maps:get(<<"is_p2p">>, Msg, 'false'),
    Users = case BUsers of
                List when is_list(List)->
                    maps:from_list([{MSISDN, 3} || MSISDN <- BUsers]);
                Map when is_map(Map) ->
                    maps:fold(fun(K,V,Acc)->
                                      maps:put(common:to_integer(K), common:to_integer(V), Acc)
                              end, #{}, BUsers)
            end,
    #c2s_chat_create{name = Name, users = Users, is_p2p = IsP2P};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_LEAVE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_leave{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_DELETE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_delete{chat_id = ChatId};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_CHAT_INVITE_USER_TYPE, <<"chat_id">> := ChatId, <<"user_msisdn">> := MSISDN}) ->
    AL = maps:get(<<"access_level">>, Msg, 3),
    #c2s_chat_invite_user{chat_id = ChatId, user_msisdn = round(MSISDN), access_level = round(AL)};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_MUTE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_mute{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_UNMUTE_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_unmute{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_TYPING_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_typing{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_MESSAGE_SEND_TYPE, <<"chat_id">> := ChatId, <<"msg_body">> := MsgBody}) ->
    #c2s_message_send{chat_id = ChatId, msg_body = MsgBody};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_MESSAGE_GET_LIST_TYPE, <<"chat_id">> := ChatId})->
    Direction = case maps:get(<<"direction">>, Msg, <<"up">>) of
                    <<"down">> -> 'down';
                    _ -> 'up'
                end,
    Count = case maps:get(<<"count">>, Msg, 30) of
                Num1 when Num1 =< 50 -> round(Num1);
                _ -> 50
            end,
    MsgId = case maps:get(<<"msg_id">>,Msg, 'undefined') of
                'undefined' -> 'undefined';
                Num when is_number(Num) -> round(Num)
            end,
    #c2s_message_get_list{chat_id = ChatId, msg_id = MsgId, count = Count, direction = Direction};
unwrap_msg(#{<<"msg_type">> := ?C2S_MESSAGE_UPDATE_TYPE, <<"chat_id">> := ChatId, <<"msg_body">> := MsgBody, <<"msg_id">> := MsgId}) ->
    #c2s_message_update{chat_id = ChatId, msg_body = MsgBody, msg_id = round(MsgId)};
unwrap_msg(#{<<"msg_type">> := ?C2S_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgIdList}) ->
    #c2s_message_update_status{chat_id = ChatId, msg_id = [round(M) || M <- MsgIdList]};
unwrap_msg(#{<<"msg_type">> := ?C2S_SYSTEM_LOGOUT_TYPE}) -> #c2s_system_logout{};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_GET_INFO_TYPE, <<"user_msisdn">> := MSISDN}) ->
    #c2s_user_get_info{user_msisdn = round(MSISDN)};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_GET_STATUS_TYPE, <<"user_msisdn">> := MSISDN}) ->
    #c2s_user_get_status{user_msisdn = round(MSISDN)};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_SET_TRAINER_INFO_TYPE, <<"msisdn">> := MSISDN, <<"affiliate_id">> := AId,<<"trainer_committee">> := TC,<<"is_judge">> := IsJ,<<"is_department_head">> := IsDH})->
    #c2s_user_set_trainer_info{msisdn = round(MSISDN)
                              ,affiliate_id = round(AId)
                              ,trainer_committee = (TC =:= 'true')
                              ,is_judge = (IsJ =:= 'true')
                              ,is_department_head = (IsDH =:= 'true')};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_SET_PARENT_INFO_TYPE, <<"msisdn">> := MSISDN,<<"affiliate_id">> := AId,<<"parental_committee">> := PC,<<"is_volunteer">> := IsV})->
    #c2s_user_set_parent_info{msisdn = round(MSISDN)
                             ,affiliate_id = round(AId)
                             ,parental_committee = (PC =:= 'true')
                             ,is_volunteer = (IsV =:= 'true')};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_SET_SPORTSMAN_INFO_TYPE, <<"msisdn">> := MSISDN, <<"height">> := Hei, <<"weight">> := Wei, <<"kyu">> := Kyu, <<"affiliate_id">> := AId, <<"is_volunteer">> := IsV, <<"is_on_team">> := IsOnT}) ->
    #c2s_user_set_sportsman_info{msisdn = round(MSISDN)
                                ,height = Hei
                                ,weight = Wei
                                ,kyu = round(Kyu)
                                ,affiliate_id = round(AId)
                                ,is_volunteer = (IsV =:= 'true')
                                ,is_on_team = (IsOnT =:= 'true')};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_USER_SET_INFO_TYPE}) ->
    #c2s_user_set_info{fname = maps:get(<<"fname">>, Msg, 'undefined')
                      ,lname = maps:get(<<"lname">>, Msg, 'undefined')
                      ,age = maps:get(<<"age">>, Msg, 'undefined')
                      ,is_male = maps:get(<<"is_male">>, Msg, 'undefined')
                      ,city = maps:get(<<"city">>, Msg, 'undefined')};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_SEARCH_TYPE, <<"fname">> := FName, <<"lname">> := LName}) ->
    #c2s_user_search{fname = FName, lname = LName};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_GET_INFO_TYPE, <<"room_id">> := RoomId}) ->
    #c2s_room_get_info{room_id = round(RoomId)};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_ROOM_SET_INFO_TYPE, <<"room_id">> := RoomId}) ->
    Name = maps:get(<<"name">>, Msg, 'undefined'),
    Desc = maps:get(<<"description">>, Msg, 'undefined'),
    Tags = case maps:get(<<"tags">>, Msg, 'undefined') of
               'undefined' -> 'undefined';
               Map when is_map(Map) -> map_to_record(room_tag, Map)
           end,
    RoomAccess = case maps:get(<<"room_access">>, Msg, 'undefined') of
                     'undefined' -> 'undefined';
                     Map1 when is_map(Map1) ->
                         maps:fold(fun(<<"default">>,V,Acc)-> maps:put('default', common:to_integer(V), Acc);
                                      (K,V,Acc)-> maps:put(common:to_integer(K), common:to_integer(V), Acc)
                                   end, #{}, Map1)
                 end,
    ChatAccess = case maps:get(<<"chat_access">>, Msg, 'undefined') of
                     'undefined' -> 'undefined';
                     Map2 when is_map(Map2) ->
                         maps:fold(fun(<<"default">>,V,Acc)-> maps:put('default', common:to_integer(V), Acc);
                                      (K,V,Acc)-> maps:put(common:to_integer(K), common:to_integer(V), Acc)
                                   end, #{}, Map2)
                 end,
    AdInfo = maps:get(<<"additional_info">>, Msg, 'undefined'),
    #c2s_room_set_info{name = Name, description = Desc, room_id = round(RoomId), tags = Tags, room_access = RoomAccess, chat_access = ChatAccess, additional_info = AdInfo};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_ADD_SUBROOM_TYPE, <<"room_id">> := RoomId, <<"subroom_id">> := SubroomId}) ->
    #c2s_room_add_subroom{room_id = round(RoomId), subroom_id = round(SubroomId)};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_DEL_SUBROOM_TYPE, <<"room_id">> := RoomId, <<"subroom_id">> := SubroomId}) ->
    #c2s_room_del_subroom{room_id = round(RoomId), subroom_id = round(SubroomId)};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_CREATE_TYPE, <<"name">> := Name, <<"description">> := Desc, <<"room_access">> := BRoomAccess, <<"chat_access">> := BChatAccess, <<"tags">> := BTags} = Msg) ->
    RoomAccess = maps:fold(fun(<<"default">>,V,Acc)-> maps:put('default', common:to_integer(V), Acc);
                              (K,V,Acc)-> maps:put(common:to_integer(K), common:to_integer(V), Acc)
                           end, #{}, BRoomAccess),
    ChatAccess = maps:fold(fun(<<"default">>,V,Acc)-> maps:put('default', common:to_integer(V), Acc);
                              (K,V,Acc)-> maps:put(common:to_integer(K), common:to_integer(V), Acc)
                           end, #{}, BChatAccess),
    Tags = map_to_record(room_tag, BTags),
    AdInfo = maps:get(<<"additional_info">>, Msg, 'undefined'),
    #c2s_room_create{name=Name, description=Desc, room_access=RoomAccess, chat_access=ChatAccess, tags=Tags, additional_info = AdInfo};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_DELETE_TYPE, <<"room_id">> := RoomId}) ->
    #c2s_room_delete{room_id = round(RoomId)};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_ROOM_SEARCH_TYPE}) ->
    Name = maps:get(<<"name">>, Msg, 'undefined'),
    Tags = case maps:get(<<"tags">>, Msg, 'undefined') of
               'undefined' -> 'undefined';
               BTags when is_map(BTags) -> map_to_record(room_tag, BTags, '_')
           end,
    #c2s_room_search{name = Name, tags = Tags};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_JOIN_TO_CHAT_TYPE, <<"room_id">> := RoomId}) ->
    #c2s_room_join_to_chat{room_id = round(RoomId)};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_GET_MY_ROOMS}) ->
    #c2s_room_get_my_rooms{};
unwrap_msg(#{<<"msg_type">> := ?C2S_ROOM_SEND_RECURSIVE_MESSAGE_TYPE, <<"msg">> := Msg, <<"room_id">> := RoomId}) ->
    #c2s_room_send_recursive_message{msg = Msg, room_id = round(RoomId)};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_ACCEPT_INVATATION_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_accept_invatation{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CHAT_REJECT_INVATATION_TYPE, <<"chat_id">> := ChatId}) ->
    #c2s_chat_reject_invatation{chat_id = ChatId};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_OFFER_TYPE, <<"msisdn">> := MSISDN, <<"sdp">> := Offer}) ->
    #c2s_call_offer{msisdn = round(MSISDN), sdp = Offer};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_ANSWER_TYPE, <<"sdp">> := Answer}) ->
    #c2s_call_answer{sdp = Answer};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_ACK_TYPE}) ->
    #c2s_call_ack{};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_ICE_CANDIDATE_TYPE, <<"candidate">> := Candidate}) ->
    #c2s_call_ice_candidate{candidate = Candidate};
unwrap_msg(#{<<"msg_type">> := ?C2S_CALL_BYE_TYPE, <<"code">> := Code}) ->
    #c2s_call_bye{code = round(Code)};
unwrap_msg(#{<<"msg_type">> := ?C2S_LOCK_TURN_SERVER_TYPE}) ->
    #c2s_lock_turn_server{};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_GET_INFO_BULK_TYPE, <<"msisdns">> := MSISDNS}) ->
    #c2s_user_get_info_bulk{msisdns = [round(M) || M <- MSISDNS]};
unwrap_msg(#{<<"msg_type">> := ?C2S_DEVICE_REGISTER, <<"push_token">> := PushToken,  <<"device_id">> := DeviceId, <<"type">> := Type}) ->
    #c2s_device_register{push_token = PushToken, type = round(Type), device_id = DeviceId};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_SUBSCRIBE_TYPE, <<"msisdn">> := MSISDNS}) ->
    #c2s_user_subscribe{msisdn = [round(M) || M <- MSISDNS]};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_UNSUBSCRIBE_TYPE, <<"msisdn">> := MSISDNS}) ->
    #c2s_user_unsubscribe{msisdn = [round(M) || M <- MSISDNS]};
unwrap_msg(#{<<"msg_type">> := ?C2S_STORAGE_SET_TYPE, <<"key">> := Key, <<"value">> := Value}) ->
    #c2s_storage_set{key = Key, value = Value};
unwrap_msg(#{<<"msg_type">> := ?C2S_STORAGE_GET_TYPE, <<"key">> := Key}) ->
    #c2s_storage_get{key = Key};
unwrap_msg(#{<<"msg_type">> := ?C2S_STORAGE_DELETE_TYPE, <<"key">> := Key}) ->
    #c2s_storage_delete{key = Key};
unwrap_msg(#{<<"msg_type">> := ?C2S_STORAGE_KEYS_TYPE}) ->
    #c2s_storage_keys{};
unwrap_msg(#{<<"msg_type">> := ?C2S_STORAGE_CAPACITY_TYPE}) ->
    #c2s_storage_capacity{};
unwrap_msg(Msg = #{<<"msg_type">> := ?C2S_RESOURCE_GET_TYPE}) ->
    Name = maps:get(<<"name">>, Msg, 'undefined'),
    Group = maps:get(<<"group">>, Msg, 'undefined'),
    #c2s_resource_get{name = Name, group = Group};
unwrap_msg(#{<<"msg_type">> := ?C2S_RESOURCE_SET_TYPE, <<"name">> := Name, <<"group">> := Group, <<"value">> := Value}) ->
    #c2s_resource_set{name = Name, group = Group, value = Value};
unwrap_msg(#{<<"msg_type">> := ?C2S_RESOURCE_DELETE_TYPE, <<"name">> := Name}) ->
    #c2s_resource_delete{name = Name};
unwrap_msg(#{<<"msg_type">> := ?C2S_USER_SET_GROUP_TYPE, <<"msisdn">> := MSISDN, <<"group">> := BGroup}) ->
    Group = case BGroup of
                <<"administrator">> -> administrator;
                <<"guest">> -> guest;
                <<"sportsman">> -> sportsman;
                <<"trainer">> -> trainer;
                <<"parent">> -> parent
            end,
    #c2s_user_set_group{msisdn = round(MSISDN), group = Group};
unwrap_msg(#{<<"msg_type">> := ?C2S_TOURNAMENT_CREATE,<<"city">> := C,<<"name">> := N,<<"judges">> := J, <<"timestamp">> := T, <<"participants">> := P}) ->
    P1 = maps:fold(fun(MSISDN, #{<<"reward">> := Reward, <<"points">> := Points}, Acc) when is_number(MSISDN)->
                           Acc#{round(MSISDN) => #participant{reward = Reward, points = round(Points)}};
                      (MSISDN, #{<<"reward">> := Reward, <<"points">> := Points}, Acc) when is_binary(MSISDN)->
                           Acc#{erlang:binary_to_integer(MSISDN, utf8) => #participant{reward = Reward, points = round(Points)}};
                      (_, _, Acc) -> Acc
                   end, #{}, P),
    #c2s_tournament_create{city = C, name = N, judges = [round(X) || X <- J], timestamp = round(T), participants = P1};
unwrap_msg(#{<<"msg_type">> := ?C2S_TOURNAMENT_UPDATE,<<"id">> := Id,<<"city">> := C,<<"name">> := N,<<"judges">> := J, <<"timestamp">> := T, <<"participants">> := P}) ->
    P1 = maps:fold(fun(MSISDN, #{<<"reward">> := Reward, <<"points">> := Points}, Acc) when is_number(MSISDN)->
                           Acc#{round(MSISDN) => #participant{reward = Reward, points = round(Points)}};
                      (MSISDN, #{<<"reward">> := Reward, <<"points">> := Points}, Acc) when is_binary(MSISDN)->
                           Acc#{erlang:binary_to_integer(MSISDN, utf8) => #participant{reward = Reward, points = round(Points)}};
                      (_, _, Acc) -> Acc
                   end, #{}, P),
    #c2s_tournament_update{id = round(Id), city = C, name = N, judges = [round(X) || X <- J], timestamp = round(T), participants = P1};
unwrap_msg(#{<<"msg_type">> := ?C2S_TOURNAMENT_READ,<<"id">> := Id}) ->
    #c2s_tournament_read{id = round(Id)};
unwrap_msg(#{<<"msg_type">> := ?C2S_TOURNAMENT_DELETE,<<"id">> := Id}) ->
    #c2s_tournament_delete{id = round(Id)};
unwrap_msg(#{<<"msg_type">> := ?C2S_TOURNAMENT_GET_LIST}) ->
    #c2s_tournament_get_list{};
unwrap_msg(_Msg) ->
    lager:debug("Can't unwrap msg: ~p~n", [_Msg]),
    'undefined'.


%%%===================================================================
%%% Prepare server response
%%%===================================================================
-spec wrap_msg(server_msg_type()) -> map().
wrap_msg(Msg) when is_record(Msg, s2c_chat_list) -> ?R2M(Msg, s2c_chat_list);
wrap_msg(Msg) when is_record(Msg, s2c_chat_info) -> ?R2M(Msg, s2c_chat_info);
wrap_msg(Msg) when is_record(Msg, s2c_chat_create_result) -> ?R2M(Msg, s2c_chat_create_result);
wrap_msg(Msg) when is_record(Msg, s2c_chat_typing) -> ?R2M(Msg, s2c_chat_typing);
wrap_msg(Msg) when is_record(Msg, s2c_message) -> ?R2M(Msg, s2c_message);
wrap_msg(Msg) when is_record(Msg, s2c_message_update) -> ?R2M(Msg, s2c_message_update);
wrap_msg(Msg) when is_record(Msg, s2c_message_update_status) -> ?R2M(Msg, s2c_message_update_status);
wrap_msg(Msg) when is_record(Msg, s2c_user_info) ->
    Msg2 = case Msg#s2c_user_info.special_info of
               #sportsman_info{} = I ->
                   Msg#s2c_user_info{special_info = ?R2M(I, sportsman_info)};
               #trainer_info{} = I ->
                   Msg#s2c_user_info{special_info = ?R2M(I, trainer_info)};
               #parent_info{} = I ->
                   Msg#s2c_user_info{special_info = ?R2M(I, parent_info)};
               _ ->
                   Msg#s2c_user_info{special_info = #{}}
           end,
    ?R2M(Msg2, s2c_user_info);
wrap_msg(Msg) when is_record(Msg, s2c_user_info_bulk) ->
    UsersMap = [maps:remove(<<"msg_type">>, wrap_msg(UserInfo)) || UserInfo <- Msg#s2c_user_info_bulk.users],
    ?R2M(Msg#s2c_user_info_bulk{users = UsersMap}, s2c_user_info_bulk);
wrap_msg(Msg) when is_record(Msg, s2c_user_status) ->
    case Msg#s2c_user_status.last_visit_timestamp of
        'undefined' -> maps:remove(last_visit_timestamp, ?R2M(Msg, s2c_user_status));
        _ -> ?R2M(Msg, s2c_user_status)
    end;
wrap_msg(Msg) when is_record(Msg, s2c_user_search_result) -> ?R2M(Msg, s2c_user_search_result);
wrap_msg(Msg) when is_record(Msg, s2c_room_info) ->
    TagsMap = common:remove('false', ?R2M(Msg#s2c_room_info.tags, room_tag)),
    Map = ?R2M(Msg#s2c_room_info{tags = TagsMap}, s2c_room_info),
    common:remove('undefined', Map);
wrap_msg(Msg) when is_record(Msg, s2c_room_create_result) -> ?R2M(Msg, s2c_room_create_result);
wrap_msg(Msg) when is_record(Msg, s2c_room_list) -> ?R2M(Msg, s2c_room_list);
wrap_msg(Msg) when is_record(Msg, s2c_room_search_result) -> ?R2M(Msg, s2c_room_search_result);
wrap_msg(Msg) when is_record(Msg, s2c_chat_invatation) -> ?R2M(Msg, s2c_chat_invatation);
wrap_msg(Msg) when is_record(Msg, s2c_error) -> ?R2M(Msg, s2c_error);
wrap_msg(Msg) when is_record(Msg, s2c_message_send_result) -> ?R2M(Msg, s2c_message_send_result);
wrap_msg(Msg) when is_record(Msg, s2c_message_list) ->
    MapMessages = [maps:remove(<<"msg_type">>, ?R2M(M, message)) || M <- Msg#s2c_message_list.messages],
    ?R2M(Msg#s2c_message_list{messages = MapMessages}, s2c_message_list);
wrap_msg(Msg) when is_record(Msg, s2c_call_offer) ->
    TurnServerMap = ?R2M(Msg#s2c_call_offer.turn_server, s2c_turn_server),
    Msg1 = Msg#s2c_call_offer{turn_server = maps:remove(<<"msg_type">>, TurnServerMap)},
    ?R2M(Msg1, s2c_call_offer);
wrap_msg(Msg) when is_record(Msg, s2c_call_answer) -> ?R2M(Msg, s2c_call_answer);
wrap_msg(Msg) when is_record(Msg, s2c_call_ack) -> ?R2M(Msg, s2c_call_ack);
wrap_msg(Msg) when is_record(Msg, s2c_call_ice_candidate) -> ?R2M(Msg, s2c_call_ice_candidate);
wrap_msg(Msg) when is_record(Msg, s2c_call_bye) -> ?R2M(Msg, s2c_call_bye);
wrap_msg(Msg) when is_record(Msg, s2c_turn_server) -> ?R2M(Msg, s2c_turn_server);
wrap_msg(Msg) when is_record(Msg, s2c_storage_keys) -> ?R2M(Msg, s2c_storage_keys);
wrap_msg(Msg) when is_record(Msg, s2c_storage_capacity) -> ?R2M(Msg, s2c_storage_capacity);
wrap_msg(Msg) when is_record(Msg, s2c_storage_get_result) -> ?R2M(Msg, s2c_storage_get_result);
wrap_msg(Msg) when is_record(Msg, s2c_resource) -> ?R2M(Msg, s2c_resource);
wrap_msg(Msg) when is_record(Msg, s2c_resource_list) -> ?R2M(Msg, s2c_resource_list);
wrap_msg(Msg) when is_record(Msg, s2c_tournament_info) ->
    Participants = maps:map(fun(_K,V)->
                                 ?R2M(V, participant)
                            end, Msg#s2c_tournament_info.participants),
    ?R2M(Msg#s2c_tournament_info{participants = Participants}, s2c_tournament_info);
wrap_msg(Msg) when is_record(Msg, s2c_tournament_list) -> ?R2M(Msg, s2c_tournament_list);
wrap_msg(Msg) when is_record(Msg, s2c_tournament_create_result) -> ?R2M(Msg, s2c_tournament_create_result);
wrap_msg(_) -> ?R2M(#s2c_error{code = 500}, s2c_error).

%%%===================================================================
%%% Handle users request
%%%===================================================================
-spec do_action(client_msg_type(), #user_state{}) -> {'ok', #user_state{}} | {Msg :: server_msg_type(), #user_state{}}.
do_action(#c2s_chat_get_list{}, #user_state{chats = Chats} = State) ->
    ChatsList = [{ChatId, chat_info:extract(chat_info:get(ChatId), name)} || ChatId <- maps:keys(Chats)],
    Resp = #s2c_chat_list{chats = maps:from_list(ChatsList)},
    {Resp, State};
do_action(#c2s_chat_get_info{chat_id = ChatId}, #user_state{muted_chats = MC, chats = Chats} = State) ->
    Resp = case chat_info:get(ChatId) of
               'false' ->
                   #s2c_error{code = 404};
               #chat_info{name = Name, users = Users, chat_owner = ChatOwner, on_room = OnRoom} ->
                   #s2c_chat_info{chat_id = ChatId
                                 ,name = Name
                                 ,users = Users
                                 ,is_muted = lists:member(ChatId, MC)
                                 ,chat_owner = ChatOwner
                                 ,last_msg_id = chats:get_last_msg_id(ChatId)
                                 ,access_level = maps:get(ChatId, Chats, 'undefined')
                                 ,on_room = OnRoom}
           end,
    {Resp, State};
do_action(#c2s_chat_create{users = UsersMap, name = ChatName, is_p2p = 'true'}, #user_state{msisdn = MyMSISDN, chats = OldChats} = State) ->
    YourMSISDN = hd(maps:keys(UsersMap)),
    case chats:new_p2p(MyMSISDN, YourMSISDN) of
        {ok, ChatId} ->
            chat_info:new(ChatId, ChatName, MyMSISDN),
            chat_info:add_user(ChatId, YourMSISDN),
            users:invite_to_chat(ChatId, MyMSISDN, 7),
            users:invite_to_chat(ChatId, YourMSISDN, 7),
            users:accept_invatation(ChatId, MyMSISDN),
            users:accept_invatation(ChatId, YourMSISDN),
            chats:subscribe(ChatId),
            case users:get_pid(YourMSISDN) of
                Pid when is_pid(Pid) -> Pid ! {chat_p2p_invatation, ChatId};
                _ -> ok
            end,
            {#s2c_chat_create_result{chat_id = ChatId}, State#user_state{chats = OldChats#{ChatId => 7}}};
        {already_exists, ChatId} ->
            {#s2c_chat_create_result{chat_id = ChatId}, State};
        _ -> {#s2c_error{code = 500}, State}
    end;
do_action(#c2s_chat_create{name = ChatName, users = Users}, #user_state{msisdn = MSISDN, chats = OldChats} = State) ->
    case chats:new() of
        {ok, ChatId} ->
            chat_info:new(ChatId, ChatName, MSISDN),
            users:invite_to_chat(ChatId, MSISDN, 7),
            users:accept_invatation(ChatId, MSISDN),
            chats:subscribe(ChatId),
            maps:fold(fun(U,AL,_)->
                              chats:invite_to_chat(ChatId, U, AL), ok
                      end, ok, Users),
            {#s2c_chat_create_result{chat_id = ChatId}, State#user_state{chats = OldChats#{ChatId => 7}}};
        _ ->
            {#s2c_error{code = 500}, State}
    end;
do_action(#c2s_chat_leave{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = OldChats} = State) ->
    case maps:take(ChatId, OldChats) of
        'error' ->
            {#s2c_error{code = 404}, State};
        {_AL, NewChats} ->
            chats:leave_chat(ChatId, MSISDN),
            {ok, State#user_state{chats = NewChats}}
    end;
do_action(#c2s_chat_delete{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = Chats} = State) ->
    case maps:get(ChatId, Chats, 'undefined') of
        AL when ?MAY_ADMIN(AL) ->
            chats:delete(ChatId, MSISDN),
            {ok, State#user_state{chats = maps:remove(ChatId, Chats)}};
        'undefined' ->
            {#s2c_error{code = 404}, State};
        _ ->
            {#s2c_error{code = 403}, State}
    end;
do_action(#c2s_chat_accept_invatation{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = OldChats} = State) ->
    case chats:accept_invatation(ChatId, MSISDN) of
        'not_exists' -> {#s2c_error{code = 404}, State};
        AL -> {ok, State#user_state{chats = OldChats#{ChatId => AL}}}
    end;
do_action(#c2s_chat_reject_invatation{chat_id = ChatId}, #user_state{msisdn = MSISDN} = State) ->
    Resp = case chats:reject_invatation(ChatId, MSISDN) of
               'not_exists' -> #s2c_error{code = 404};
               _Ok -> ok
           end,
    {Resp, State};
do_action(#c2s_chat_invite_user{chat_id = ChatId, user_msisdn = MSISDN, access_level = AL}, #user_state{chats = Chats} = State) ->
    Resp = case maps:get(ChatId, Chats, 'undefined') of
               MyAL when is_number(AL) andalso ?MAY_ADMIN(MyAL) ->
                   chats:invite_to_chat(ChatId, MSISDN, AL),
                   ok;
               _ when not is_number(AL) ->
                   #s2c_error{code = 400};
               'undefined' ->
                   #s2c_error{code = 404};
               _ ->
                   #s2c_error{code = 403}
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
do_action(#c2s_chat_typing{chat_id = ChatId}, #user_state{msisdn = MSISDN, chats = Chats} = _State) ->
    case Chats of
        #{ChatId := AL} when ?MAY_WRITE(AL) ->
            chats:typing(ChatId, MSISDN);
        _ -> ok
    end,
    {ok, _State};
do_action(#c2s_message_send{chat_id = ChatId, msg_body = MsgBody}, #user_state{msisdn = MSISDN, chats = Chats} = State) ->
    Resp = case maps:get(ChatId, Chats, 'undefined') of
               'undefined' ->
                   #s2c_error{code = 404};
               AL when ?MAY_WRITE(AL) ->
                   case chat_info:get(ChatId) of
                       #chat_info{users = ChatUsers} when length(ChatUsers) == 2 ->     %p2p chat
                           User = hd(ChatUsers -- [MSISDN]),
                           #user{fname = FName, lname = LName} = users:get(MSISDN),
                           ChatName = <<FName/binary, " ", LName/binary>>,              %my name
                           MsgId = chats:send_message(ChatId, MsgBody, MSISDN),
                           case is_pid(users:get_pid(User)) of
                               true ->
                                   ok;
                               false ->
                                   push_app:notify_msg([User], ChatId, ChatName, MsgId, MsgBody)
                           end,
                           #s2c_message_send_result{chat_id = ChatId, msg_id = MsgId};
                       #chat_info{users = ChatUsers, name = ChatName} ->                %group chat
                           #user{fname = FName} = users:get(MSISDN),
                           MsgId = chats:send_message(ChatId, MsgBody, MSISDN),
                           OfflineUsers = [U || U <- ChatUsers, not(is_pid(users:get_pid(U)))],
                           push_app:notify_msg(OfflineUsers, ChatId, ChatName, MsgId, <<"@",FName/binary,": ",MsgBody/binary>>),          %send silent push to offline users
                           #s2c_message_send_result{chat_id = ChatId, msg_id = MsgId};
                       _->
                           #s2c_error{code = 404}
                   end;
               _ -> #s2c_error{code = 403}
           end,
    {Resp, State};
do_action(#c2s_message_get_list{chat_id = ChatId, msg_id = MsgId, count = Count, direction = Direction}, #user_state{chats = Chats} = _State) ->
    Resp = case maps:get(ChatId, Chats, 'undefined') of
               'undefined' ->
                   #s2c_error{code = 403};
               AL when ?MAY_READ(AL) ->
                   Messages = chats:get_messages_by_id(ChatId, MsgId, Count, Direction),
                   #s2c_message_list{messages = Messages, chat_id = ChatId};
               _ -> #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_message_update{chat_id = ChatId, msg_id = MsgId, msg_body = MsgBody}, #user_state{chats = Chats, msisdn = MSISDN} = _State) ->
    Resp = case maps:get(ChatId, Chats, 'undefined') of
               'undefined'->
                   #s2c_error{code = 404};
               AL when ?MAY_WRITE(AL) ->
                   case chats:update_message(ChatId, MsgId, MsgBody, MSISDN) of
                       ok -> ok;
                       _ -> #s2c_error{code = 403}
                   end;
               _ -> #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_message_update_status{chat_id = ChatId, msg_id = MsgIdList}, #user_state{chats = Chats, msisdn = MSISDN} = _State) ->
    Resp = case maps:get(ChatId, Chats, 'undefined') of
               'undefined' ->
                   #s2c_error{code = 404};
               AL when ?MAY_WRITE(AL) ->
                   chats:update_message_status(ChatId, MsgIdList, MSISDN),
                   ok;
               _ -> #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(_Msg = #c2s_system_logout{}, _State) ->
    {ok, _State};
do_action(#c2s_user_set_group{msisdn = MSISDN, group = Group}, #user_state{msisdn = MyMSISDN} = _State) ->
    Resp = case users:get(MyMSISDN) of
               #user{group = 'administrator'} ->
                   users:set_info(MSISDN, [{'group', Group}]),
                   case sessions:get_by_owner_id(MSISDN) of
                       #session{} = S ->
                           sessions:set(S#session{'group' = Group});
                       'false' -> 'ok'
                   end;
               #user{group = 'trainer', special_info = #trainer_info{is_department_head = 'true'}} when Group /= 'administrator' ->
                   case users:get(MSISDN) of
                       #user{group = G} when G /= 'administrator' ->
                           users:set_info(MSISDN, [{'group', Group}]),
                           case sessions:get_by_owner_id(MSISDN) of
                               #session{} = S ->
                                   sessions:set(S#session{'group' = Group});
                               'false' -> 'ok'
                           end;
                       #user{} ->
                           #s2c_error{code = 403};
                       _ ->
                           #s2c_error{code = 404}
                   end;
               #user{group = 'trainer'} when Group /= 'administrator' orelse Group /= 'trainer' ->
                   case users:get(MSISDN) of
                       #user{group = G} when G /= 'administrator' orelse G /= 'trainer'  ->
                           users:set_info(MSISDN, [{'group', Group}]),
                           case sessions:get_by_owner_id(MSISDN) of
                               #session{} = S ->
                                   sessions:set(S#session{'group' = Group});
                               'false' -> 'ok'
                           end;
                       #user{} ->
                           #s2c_error{code = 403};
                       _ ->
                           #s2c_error{code = 404}
                   end;
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_user_get_info{user_msisdn = MSISDN}, _State) ->
    Resp = case users:get(MSISDN) of
               #user{fname = FName, lname = LName, age = Age, is_male = IsMale, group = Group, city = City, special_info = SpecialInfo} ->
                   #s2c_user_info{user_msisdn = MSISDN, fname = FName, lname = LName, age = Age, is_male = IsMale, city = City, group = Group, special_info = SpecialInfo};
               'false' ->
                   #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_user_get_info_bulk{msisdns = MSISDNS}, _State) ->
    Users = [element(1, do_action(#c2s_user_get_info{user_msisdn = MSISDN}, _State)) || MSISDN <- MSISDNS],
    FoundedUsers = lists:filter(fun(X)-> is_record(X, s2c_user_info) end, Users),
    Resp = #s2c_user_info_bulk{users = FoundedUsers},
    {Resp, _State};
do_action(#c2s_user_get_status{user_msisdn = MSISDN}, _State) ->
    Resp = case users:get_pid(MSISDN) of
               _Pid when is_pid(_Pid) ->
                   #s2c_user_status{msisdn = MSISDN, status = 'online'};
               _ ->
                   case users:get(MSISDN) of
                       'false' ->
                           #s2c_error{code = 404};
                       #user{last_visit_timestamp = LVTS} ->
                           #s2c_user_status{msisdn = MSISDN, status = 'offline', last_visit_timestamp = LVTS}
                   end
           end,
    {Resp, _State};
do_action(#c2s_user_set_trainer_info{msisdn = MSISDN,affiliate_id = AId,trainer_committee = TC,is_judge = IsJ,is_department_head = IsDH}, #user_state{msisdn = MyMSISDN} = _State) ->
    Resp = case users:get(MyMSISDN) of
               #user{group = 'administrator'} ->
                   case usres:get(MSISDN) of
                       #user{group = 'trainer'} ->
                           Info = #trainer_info{affiliate_id = AId %TODO: check exists
                                               ,trainer_committee = TC
                                               ,is_judge = IsJ
                                               ,is_department_head = IsDH},
                           users:set(MSISDN, [{special_info, Info}]),
                           ok;
                       _ ->
                           #s2c_error{code = 400}
                   end;
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_user_set_parent_info{msisdn = MSISDN,affiliate_id = AId,parental_committee = PC,is_volunteer = IsV}, #user_state{msisdn = MyMSISDN} = _State) ->
    Resp = case users:get(MyMSISDN) of
               #user{group = G} when G == 'administrator' orelse G == 'trainer' ->
                   case usres:get(MSISDN) of
                       #user{group = 'parent'} ->
                           Info = #parent_info{affiliate_id = AId
                                              ,parental_committee = PC
                                              ,is_volunteer = IsV},
                           users:set(MSISDN, [{special_info, Info}]),
                           ok;
                       _ ->
                           #s2c_error{code = 400}
                   end;
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_user_set_sportsman_info{msisdn = MSISDN,height = Hei,weight = Wei,kyu = Kyu,affiliate_id = AId,is_volunteer = IsV,is_on_team = IsOnT}, #user_state{msisdn = MyMSISDN} = _State) ->
        Resp = case users:get(MyMSISDN) of
               #user{group = G} when G == 'administrator' orelse G == 'trainer' ->
                   case usres:get(MSISDN) of
                       #user{group = 'sportsman'} ->
                           Info = #sportsman_info{height = Hei
                                                 ,weight = Wei
                                                 ,kyu = Kyu
                                                 ,affiliate_id = AId
                                                 ,is_volunteer = IsV
                                                 ,is_on_team = IsOnT},
                           users:set(MSISDN, [{special_info, Info}]),
                           ok;
                       _ ->
                           #s2c_error{code = 400}
                   end;
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_user_set_info{} = SI, #user_state{msisdn = MSISDN} = _State) ->
    PropList = lists:zip(record_info(fields, c2s_user_set_info), tl(tuple_to_list(SI))),
    Info = lists:filter(fun({_,'undefined'}) -> 'false';
                           ({'msg_type',_})-> 'false';
                           ({_,_})-> 'true'
                        end, PropList),
    users:set_info(MSISDN, Info),
    {ok, _State};
do_action(#c2s_user_search{fname = FName, lname = LName}, _State) ->
    Users = users:search(FName, LName),
    Resp = #s2c_user_search_result{users = Users},
    {Resp, _State};
do_action(#c2s_room_get_info{room_id = RoomId}, #user_state{msisdn = MSISDN} = State) ->
    Response = case rooms:get(RoomId) of
                   'false' ->                               % not found
                       #s2c_error{code = 404};
                   #room{owner_id = MSISDN} = R ->          % it's owner
                       ?ROOM_TO_ROOM_INFO(R);
                   #room{room_access = #{MSISDN := AL}} = R when ?MAY_ADMIN(AL) ->  %it's admin
                       ?ROOM_TO_ROOM_INFO(R);
                   #room{room_access = #{MSISDN := RoomAL}, chat_access = CA} = R when ?MAY_READ(RoomAL) ->     % have access to the room...
                       case CA of
                           #{MSISDN := AL} when ?MAY_ADMIN(AL) ->   % chat admin
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined'};
                           #{MSISDN := 0} ->                        % it's user with baned chat
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined', chat_id = 'undefined'};
                           #{MSISDN := _} ->                        % it's user with normal chat access
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined'};
                           #{'default' := AL} when ?MAY_ADMIN(AL) -> % all chat admins
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined'};
                           #{'default' := 0} ->       % chat closed
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined', chat_id = 'undefined'};
                           _ ->                        % all users have normal chat access
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined'}
                       end;
                   #room{room_access = #{MSISDN := _}} ->
                       #s2c_error{code = 403};
                   #room{room_access = #{'default' := AL}} = R when ?MAY_ADMIN(AL) ->       %all admins
                       ?ROOM_TO_ROOM_INFO(R);
                   #room{room_access = #{'default' := RoomAL}, chat_access = CA} = R when ?MAY_READ(RoomAL) -> % have common access to the room...
                       case CA of
                           #{MSISDN := AL} when ?MAY_ADMIN(AL) ->   % chat admin
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined'};
                           #{MSISDN := AL} when ?MAY_READ(AL) ->    % it's user with normal chat access
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined'};
                           #{MSISDN := _} ->                        % it's user with baned chat
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined', chat_id = 'undefined'};
                           #{'default' := AL} when ?MAY_ADMIN(AL) -> % all chat admins
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined'};
                           #{'default' := AL} when ?MAY_READ(AL) ->  % all users have normal chat access
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined'};
                           #{'default' := _} ->                     % chat closed
                               Resp = ?ROOM_TO_ROOM_INFO(R),
                               Resp#s2c_room_info{room_access = 'undefined', chat_access = 'undefined', chat_id = 'undefined'}
                       end;
                   _ ->
                       #s2c_error{code = 403}
               end,
    {Response, State};
do_action(#c2s_room_set_info{name=Name,description=Desc,room_id=RoomId,tags=T,room_access=RA,chat_access=CA, additional_info = AI},#user_state{msisdn=MSISDN}=_State)->
    UpdateRoom = fun(Room)->
                         Room1 = case Name of
                                     'undefined' -> Room;
                                     _ -> Room#room{name = Name}
                                 end,
                         Room2 = case Desc of
                                     'undefined' -> Room1;
                                     _ -> Room1#room{description = Desc}
                                 end,
                         Room3 = case RA of
                                     'undefined' -> Room2;
                                     _ -> Room2#room{room_access = RA}
                                 end,
                         Room4 = case CA of
                                     'undefined' -> Room3;
                                     _ -> Room3#room{chat_access = CA}
                                 end,
                         case T of
                             'undefined' -> ok;
                             _ when is_record(T, 'room_tag')->
                                 rooms:set_tag(T#room_tag{room_id = RoomId, name = Room4#room.name})
                         end,
                         Room5 = case AI of
                                     'undefined' -> Room4;
                                     _ -> Room4#room{additional_info = AI}
                                 end,
                         rooms:set(Room5),
                         ok
                 end,
    Resp = case rooms:get(RoomId) of
               #room{owner_id = MSISDN} = Room ->
                   UpdateRoom(Room);
               #room{room_access = #{MSISDN := AL}} = Room when ?MAY_ADMIN(AL) ->
                   UpdateRoom(Room);
               #room{room_access = #{MSISDN := _}} ->
                   #s2c_error{code = 403};
               #room{room_access = #{'default' := AL}} = Room when ?MAY_ADMIN(AL) ->
                   UpdateRoom(Room);
               #room{chat_access = #{MSISDN := AL}} = Room when ?MAY_ADMIN(AL) ->
                   case CA of
                       _ when is_map(CA) -> rooms:set(Room#room{chat_access = CA}), ok;
                       _ -> #s2c_error{code = 403}
                   end;
               #room{chat_access = #{MSISDN := _}} ->
                   #s2c_error{code = 403};
               #room{chat_access = #{'default' := AL}} = Room when ?MAY_ADMIN(AL) ->
                   case CA of
                       _ when is_map(CA) -> rooms:set(Room#room{chat_access = CA}), ok;
                       _ -> #s2c_error{code = 403}
                   end;
               #room{} ->
                   #s2c_error{code = 403};
               _ ->
                   #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_room_add_subroom{room_id = RoomId, subroom_id = SubroomId}, #user_state{msisdn = MSISDN} = _State) ->
    Resp = case rooms:get(RoomId) of
               #room{owner_id = MSISDN} = Room ->
                   rooms:set(Room#room{subrooms = lists:usort([SubroomId | Room#room.subrooms])}), ok;
               #room{room_access = #{MSISDN := AL}} = Room when ?MAY_ADMIN(AL) ->
                   rooms:set(Room#room{subrooms = lists:usort([SubroomId | Room#room.subrooms])}), ok;
               #room{room_access = #{MSISDN := _}} ->
                   #s2c_error{code = 403};
               #room{room_access = #{'default' := AL}} = Room when ?MAY_ADMIN(AL) ->
                   rooms:set(Room#room{subrooms = lists:usort([SubroomId | Room#room.subrooms])}), ok;
               #room{} ->
                   #s2c_error{code = 403};
               _ ->
                   #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_room_del_subroom{room_id = RoomId, subroom_id = SubroomId}, #user_state{msisdn = MSISDN} = _State) ->
    Resp = case rooms:get(RoomId) of
               #room{owner_id = MSISDN} = Room ->
                   rooms:set(Room#room{subrooms = Room#room.subrooms -- [SubroomId]}), ok;
               #room{room_access = #{MSISDN := AL}} = Room when ?MAY_ADMIN(AL) ->
                   rooms:set(Room#room{subrooms = Room#room.subrooms -- [SubroomId]}), ok;
               #room{room_access = #{MSISDN := _}} ->
                   #s2c_error{code = 403};
               #room{room_access = #{'default' := AL}} = Room when ?MAY_ADMIN(AL) ->
                   rooms:set(Room#room{subrooms = Room#room.subrooms -- [SubroomId]}), ok;
               #room{} ->
                   #s2c_error{code = 403};
               _ ->
                   #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_room_search{name = 'undefined', tags = 'undefined'}, _State) ->
    {ok, _State};
do_action(#c2s_room_search{name = 'undefined', tags = Tags}, _State) ->
    Rooms = rooms:search_by_tags(Tags#room_tag{room_id = '_'}),
    Resp = #s2c_room_search_result{rooms = Rooms},
    {Resp, _State};
do_action(#c2s_room_search{name = Name, tags = 'undefined'}, _State) ->
    Rooms = rooms:search_by_name(Name),
    Resp = #s2c_room_search_result{rooms = Rooms},
    {Resp, _State};
do_action(#c2s_room_search{name = Name, tags = Tags}, _State) ->
    Rooms1 = rooms:search_by_tags(Tags#room_tag{room_id = '_'}),
    Rooms2 = rooms:search_by_name(Name),
    Intersection = [R || R <- Rooms1, lists:member(R, Rooms2)],
    Resp = #s2c_room_search_result{rooms = Intersection},
    {Resp, _State};
do_action(#c2s_room_create{name=Name,description=Desc,room_access=RoomAccess,chat_access = ChatAccess,tags = Tags, additional_info = AdInfo},#user_state{msisdn=MSISDN}=State) ->
    Resp = case sessions:get_by_owner_id(MSISDN) of
               #session{group = 'administrator'} ->
                   case rooms:new(MSISDN, Name, Desc, RoomAccess, ChatAccess, Tags, AdInfo) of
                       'false'-> #s2c_error{code = 500};
                       RoomId -> #s2c_room_create_result{room_id = RoomId}
                   end;
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, State};
do_action(#c2s_room_delete{room_id = RoomId}, #user_state{msisdn = MSISDN} = _State) ->
    Resp = case rooms:get(RoomId) of
               #room{owner_id = MSISDN} -> rooms:delete(RoomId), ok;
               #room{} -> #s2c_error{code = 403};
               _ -> #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_room_join_to_chat{room_id = RoomId}, #user_state{msisdn = MSISDN} = _State) ->
    Resp = case rooms:get(RoomId) of
               #room{owner_id = MSISDN, chat_id = ChatId} when ChatId /= 'undefined' ->
                   chats:invite_to_chat(ChatId, MSISDN, 7), ok;
               #room{room_access = #{MSISDN := 0}} ->
                   #s2c_error{code = 403};
               #room{room_access = #{MSISDN := _}, chat_access = #{MSISDN := 0}} ->
                   #s2c_error{code = 403};
               #room{room_access = #{MSISDN := _}, chat_access = #{MSISDN := AL}, chat_id = ChatId} when ChatId /= 'undefined' ->
                   chats:invite_to_chat(ChatId, MSISDN, AL), ok;
               #room{room_access = #{'default' := 0}} ->
                   #s2c_error{code = 403};
               #room{chat_access = #{MSISDN := 0}} ->
                   #s2c_error{code = 403};
               #room{chat_access = #{MSISDN := AL}, chat_id = ChatId} when ChatId /= 'undefined' ->
                   chats:invite_to_chat(ChatId, MSISDN, AL), ok;
               #room{chat_access = #{'default' := 0}} ->
                   #s2c_error{code = 403};
               #room{chat_access = #{'default' := AL}, chat_id = ChatId} when ChatId /= 'undefined' ->
                   chats:invite_to_chat(ChatId, MSISDN, AL), ok;
               _ ->
                   #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_room_get_my_rooms{}, #user_state{msisdn = MSISDN} = State) ->
    #user{rooms = Rooms} = users:get(MSISDN),
    Resp = #s2c_room_list{rooms = maps:keys(Rooms)},
    {Resp, State};
do_action(#c2s_room_send_recursive_message{msg = Msg, room_id = RoomId}, #user_state{msisdn = MSISDN} = State) ->
    Resp = case rooms:get(RoomId) of
               #room{owner_id = MSISDN} ->
                   case rooms:send_msg_to_room(RoomId, Msg, MSISDN, true) of
                       ok -> ok;
                       _ -> #s2c_error{code = 500}
                   end;
               #room{room_access = #{MSISDN := AL}} when ?MAY_ADMIN(AL) ->
                   case rooms:send_msg_to_room(RoomId, Msg, MSISDN, true) of
                       ok -> ok;
                       _ -> #s2c_error{code = 500}
                   end;
               #room{room_access = #{MSISDN := _}} ->
                   #s2c_error{code = 403};
               #room{room_access = #{'default' := AL}} when ?MAY_ADMIN(AL) ->
                   case rooms:send_msg_to_room(RoomId, Msg, MSISDN, true) of
                       ok -> ok;
                       _ -> #s2c_error{code = 500}
                   end;
               #room{} ->
                   #s2c_error{code = 403};
               _ ->
                   #s2c_error{code = 404}
           end,
    {Resp, State};
do_action(#c2s_call_offer{}, #user_state{call = #call_info{}} = _State) ->      % call record defined, call in progress
    Resp = #s2c_call_bye{code = 491},                                           % Request Pending
    {Resp, _State};
do_action(#c2s_call_offer{msisdn = CalleeMSISDN, sdp = Offer}, #user_state{msisdn = CallerMSISDN} = State) ->
    case users:get(CalleeMSISDN) of
        'false' ->
            {#s2c_call_bye{code = 404}, State};   %user not found
        _ ->
            {TurnServer, NewState} = case State#user_state.turn_server of
                                         T when is_record(T, s2c_turn_server) ->
                                             {T, State};
                                         _ ->
                                             do_action(#c2s_lock_turn_server{}, State)
                                     end,
            case users:get_pid(CalleeMSISDN) of
                CalleePid when is_pid(CalleePid) ->                  %user online
                    Ref = monitor(process, CalleePid),
                    CalleePid ! {call_offer, CallerMSISDN, Offer, self(), TurnServer},
                    {ok, NewState#user_state{call = #call_info{pid = CalleePid, msisdn = CalleeMSISDN, ref = Ref}}};
                _ ->                                                                    %user offline
                    users:subscribe(CalleeMSISDN, CallerMSISDN),
                    push_app:notify_call(CalleeMSISDN, CallerMSISDN),
                    {#s2c_call_ack{}, NewState#user_state{call = #call_info{msisdn = CalleeMSISDN, sdp = Offer}}}
            end
    end;
do_action(#c2s_call_answer{sdp = Answer}, #user_state{msisdn = CallerMSISDN, call = #call_info{pid = CalleePid}} = _State) ->
    CalleePid ! {call_answer, CallerMSISDN, Answer, self()},
    {ok, _State};
do_action(#c2s_call_answer{}, _State) ->
    Resp = #s2c_call_bye{code = 410},                                           % Gone: offer is not available any more
    {Resp, _State};
do_action(#c2s_call_ack{}, #user_state{call = #call_info{pid = Pid}} = _State) ->
    Pid ! call_ack,
    {ok, _State};
do_action(#c2s_call_ack{}, _State) ->
    {ok, _State};
do_action(#c2s_call_ice_candidate{candidate = C}, #user_state{call = #call_info{pid = Pid}} = _State) when is_pid(Pid) ->
    Pid ! {call_ice_candidate, C},
    {ok, _State};
do_action(#c2s_call_ice_candidate{}, _State) ->
    {ok, _State};
do_action(#c2s_call_bye{code = Code}, #user_state{call = #call_info{pid = Pid, ref = Ref}} = State) ->
    if is_pid(Pid)-> demonitor(Ref),
                     Pid ! {call_bye, Code};
       true -> ok
    end,
    {ok, State#user_state{call = 'undefined'}};
do_action(#c2s_call_bye{}, State) ->
    {ok, State#user_state{call = 'undefined'}};
do_action(#c2s_lock_turn_server{}, State) ->
    case application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'turn_servers') of
        {ok, TURNs} when is_list(TURNs) andalso length(TURNs) > 1 ->
            Index = rand:uniform(length(TURNs)),
            #{adress := Adress
             ,port := Port
             ,username := UserName
             ,realm := Realm
             ,credential := Credential
             ,credential_type := CredentialType} = lists:nth(Index, TURNs),
            ServerRec = #s2c_turn_server{adress = Adress
                                        ,port = Port
                                        ,username = UserName
                                        ,realm = Realm
                                        ,credential = Credential
                                        ,credential_type = CredentialType},
            {ServerRec, State#user_state{turn_server = ServerRec}};
        {ok, [#{adress := Adress
               ,port := Port
               ,username := UserName
               ,realm := Realm
               ,credential := Credential
               ,credential_type := CredentialType}]} ->
            ServerRec = #s2c_turn_server{adress = Adress
                                        ,port = Port
                                        ,username = UserName
                                        ,realm = Realm
                                        ,credential = Credential
                                        ,credential_type = CredentialType},
            {ServerRec, State#user_state{turn_server = ServerRec}};
        _ ->
            {#s2c_error{code = 404}, State}
    end;
do_action(#c2s_device_register{push_token = PushToken, type = Type, device_id = DeviceId}, #user_state{msisdn = MSISDN} = _State) ->
    Resp = case Type of
               0 -> device:new(MSISDN, DeviceId, 'android', PushToken), ok;
               1 -> device:new(MSISDN, DeviceId, 'ios', PushToken), ok;
               2 -> device:new(MSISDN, DeviceId, 'ios_voip', PushToken), ok;
               _ -> #s2c_error{code = 400}
           end,
    {Resp, _State};
do_action(#c2s_user_subscribe{msisdn = Users}, #user_state{msisdn = MSISDN} = _State) ->
    Self = self(),
    lists:foreach(fun(U)->
                          case users:get_pid(U) of
                              Pid when is_pid(Pid) -> Self ! {notify, U, online, Pid};
                              _ -> Self ! {notify, U, offline, undefined}
                          end,
                          users:subscribe(U, MSISDN)
                  end, Users),
    {ok, _State};
do_action(#c2s_user_unsubscribe{msisdn = Users}, #user_state{msisdn = MSISDN} = _State) ->
    [users:unsubscribe(U, MSISDN) || U <- Users],
    {ok, _State};
do_action(#c2s_storage_set{key = Key, value = Value}, #user_state{storage = Storage} = State) ->
    MaxCapacity = application:get_env('server', 'user_storage_capacity', 1024),
    case maps:size(Storage) of
        Mastadonic when Mastadonic >= MaxCapacity ->
            {#s2c_error{code = 413}, State};
        _ ->
            {ok, State#user_state{storage = Storage#{Key => Value}}}
    end;
do_action(#c2s_storage_get{key = Key}, #user_state{storage = Storage} = _State) ->
    Resp = case maps:get(Key, Storage, 'undefined') of
               'undefined' ->
                   #s2c_error{code = 404};
               Value ->
                   #s2c_storage_get_result{key = Key, value = Value}
           end,
    {Resp, _State};
do_action(#c2s_storage_delete{key = Key}, #user_state{storage = Storage} = State) ->
    {ok, State#user_state{storage = maps:remove(Key, Storage)}};
do_action(#c2s_storage_keys{}, #user_state{storage = Storage} = _State) ->
    Keys = maps:keys(Storage),
    Resp = #s2c_storage_keys{keys = Keys},
    {Resp, _State};
do_action(#c2s_storage_capacity{}, #user_state{storage = Storage} = _State) ->
    Capacity = maps:size(Storage),
    Max = application:get_env('server', 'user_storage_capacity', 1024),
    Resp = #s2c_storage_capacity{used = Capacity, max = Max},
    {Resp, _State};
do_action(#c2s_resource_get{name = Name, group = Group}, _State) ->
    Resp = if is_binary(Name) ->
                   case resources:get(Name) of
                       'false'-> #s2c_error{code = 404};
                       Value  -> #s2c_resource{name = Name, value = Value}
                   end;
              'true' ->
                   if is_binary(Group) ->
                           #s2c_resource_list{group = Group, names = resources:list(Group)};
                      'true' ->
                           #s2c_resource_list{group = 'undefined', names = resources:list()}
                   end
           end,
    {Resp, _State};
do_action(#c2s_resource_set{name = Name, group = Group, value = Value}, #user_state{msisdn = MSISDN} = _State) ->
    Resp = case sessions:get_by_owner_id(MSISDN) of
               #session{group = 'administrator'} ->
                   case resources:set(Group, Name, Value) of
                       'false'-> #s2c_error{code = 500};
                       'ok' -> 'ok'
                   end;
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_resource_delete{name = Name}, #user_state{msisdn = MSISDN} = _State) ->
    Resp = case sessions:get_by_owner_id(MSISDN) of
               #session{group = 'administrator'} ->
                   case resources:delete(Name) of
                       'false'-> #s2c_error{code = 500};
                       'ok' -> 'ok'
                   end;
               _ ->
                   #s2c_error{code = 403}
           end,
    {Resp, _State};
do_action(#c2s_tournament_read{id = Id}, _State) ->
    Resp = case tournaments:get(Id) of
               #tournament{id = Id, city = City, name = Name ,judges = Judges, timestamp = Ts, participants = Ps} ->
                   #s2c_tournament_info{id = Id, city = City, name = Name ,judges = Judges, timestamp = Ts, participants = Ps};
               _ ->
                   #s2c_error{code = 404}
           end,
    {Resp, _State};
do_action(#c2s_tournament_create{city = C, name = N, judges = J, timestamp = T, participants = P}, #user_state{group = G} = _State) when G == 'administrator' orelse G == 'trainer' ->
    Resp = case tournaments:new(N, C, J, T, P) of
               false -> #s2c_error{code = 500};
               Id -> #s2c_tournament_create_result{id = Id}
           end,
    {Resp, _State};
do_action(#c2s_tournament_update{id = Id, city = C, name = N, judges = J, timestamp = T, participants = P}, #user_state{group = G} = _State) when G == 'administrator' orelse G == 'trainer' ->
    Resp =  case tournaments:set(#tournament{id = Id, city = C, name = N, judges = J, timestamp = T, participants = P}) of
                ok -> ok;
                _ -> #s2c_error{code = 500}
            end,
    {Resp, _State};
do_action(#c2s_tournament_delete{id = Id}, #user_state{group = G} = _State) when G == 'administrator' orelse G == 'trainer' ->
    tournaments:delete(Id),
    {ok, _State};
do_action(#c2s_tournament_get_list{}, _State) ->
    Resp =  case tournaments:detailed_list() of
                false -> #s2c_error{code = 500};
                Map -> #s2c_tournament_list{tournaments = Map}
            end,
    {Resp, _State};
do_action(#c2s_tournament_create{}, _State) ->
    {#s2c_error{code = 403}, _State};
do_action(#c2s_tournament_update{}, _State) ->
    {#s2c_error{code = 403}, _State};
do_action(#c2s_tournament_delete{}, _State) ->
    {#s2c_error{code = 403}, _State};
do_action({call_offer, _CallerMSISDN, _Offer, CallerPid, _TurnServer}, #user_state{call = #call_info{}} = _State) -> % you are busy
    CallerPid ! {call_bye, 486},
    {ok, _State};
do_action({call_offer, CallerMSISDN, Offer, CallerPid, TurnServer}, State) ->
    Ref = monitor(process, CallerPid),
    NewState = State#user_state{call = #call_info{msisdn = CallerMSISDN, pid = CallerPid, ref = Ref}},
    Resp = #s2c_call_offer{msisdn = CallerMSISDN, sdp = Offer, turn_server = TurnServer},
    {Resp, NewState};
do_action({call_answer, CallerMSISDN, Answer, _CallerPid}, #user_state{call = #call_info{msisdn = CallerMSISDN}} = _State) ->
    {#s2c_call_answer{sdp = Answer}, _State};
do_action({call_answer, _CallerMSISDN, _Answer, CallerPid}, _State) ->
    CallerPid ! {call_bye, 410},                                                % Gone: offer is not available any more
    {ok, _State};
do_action(call_ack, _State) ->
    Resp = #s2c_call_ack{},
    {Resp, _State};
do_action({call_bye, Code}, #user_state{call = CallInfo} = State) -> %user hang up
    case CallInfo of
        #call_info{ref = Ref} ->
            demonitor(Ref);
        _ -> ok
    end,
    Resp = #s2c_call_bye{code = Code},
    {Resp, State#user_state{call = 'undefined'}};
do_action({call_ice_candidate, Candidate}, _State) ->
    Resp = #s2c_call_ice_candidate{candidate = Candidate},
    {Resp, _State};
do_action({'DOWN', Ref, _Type, Pid, _ErrorReason}, #user_state{call = #call_info{pid = Pid, msisdn = _CalleeMSISDN, ref = Ref}} = State) -> %monitor triggered, opponents proc dies
    lager:debug("call interrupted, because opponent proc crashed: ~p", [_ErrorReason]),
    %% TODO: create CDR about end of call
    Resp = #s2c_call_bye{code = 500},
    {Resp, State#user_state{call = 'undefined'}};
do_action({chat_typing, _ChatId, MSISDN}, #user_state{msisdn = MSISDN} = State) -> %you self typing, ignore
    {ok, State};
do_action({chat_typing, ChatId, MSISDN}, #user_state{muted_chats = MC, chats = Chats} = State) ->
    Resp = case maps:get(ChatId, Chats, 'undefined') of
               'undefined' -> 'ok';
               AL when ?MAY_READ(AL) ->
                   case lists:member(ChatId, MC) of
                       'true' -> ok;
                       'false'-> #s2c_chat_typing{chat_id = ChatId, user_msisdn = MSISDN}
                   end;
               _ -> 'ok'
           end,
    {Resp, State};
do_action({chat_delete, ChatId, MSISDN}, #user_state{chats = Chats, msisdn = MyMSISDN} = State) ->
    chats:unsubscribe(ChatId),
    Resp = case MSISDN == MyMSISDN of
               'true' ->
                   ok;
               'false'->
                   #s2c_message{chat_id = ChatId, msg_body = <<"@system:delete_chat">>, status = 'pending', msg_id = common:timestamp(), from = MSISDN}
           end,
    {Resp, State#user_state{chats = maps:remove(ChatId, Chats)}};
do_action({chat_p2p_invatation, ChatId}, #user_state{chats = OldChats} = State) ->
    chats:subscribe(ChatId),
    Resp = #s2c_chat_create_result{chat_id = ChatId},
    {Resp, State#user_state{chats = OldChats#{ChatId => 7}}};
do_action({chat_invatation, ChatId, AL}, _State) ->
    Resp = #s2c_chat_invatation{chat_id = ChatId, access_level = AL},
    {Resp, _State};

do_action({mnesia_table_event, {write, _Table, #message{from = MSISDN}, [], _ActivityId}}, #user_state{msisdn = MSISDN} = _State) -> %it's my own message, ignore
    {ok, _State};
%% do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, from = MSISDN}, [], _ActivityId}}, #user_state{msisdn = MSISDN} = _State) -> %it's my own message, send only msg_id and chat_id
%%     <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
%%     Resp = #s2c_message_send_result{chat_id = ChatId, msg_id = MsgId},
%%     {Resp, _State};
do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, msg_body = MsgBody, status = Status, from = From}, [], _ActivityId}}, _State) -> %if no old msg, that's new message
    <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
    Resp = #s2c_message{chat_id = ChatId, msg_body = MsgBody, status = Status, msg_id = MsgId, from = From},
    {Resp, _State};
do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, status = NewStatus}, [#message{msg_id = MsgId, status = OldStatus}], _ActivityId}}, _State) when NewStatus /= OldStatus -> %msg status update
    <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
    Resp = #s2c_message_update_status{chat_id = ChatId, msg_id = MsgId, status = NewStatus},
    {Resp, _State};
do_action({mnesia_table_event, {write, _Table, #message{from = From}, _OldRecList, _ActivityId}}, #user_state{msisdn = From} = _State) -> %it's my own update - ignore
    {ok, _State};
do_action({mnesia_table_event, {write, Table, #message{msg_id = MsgId, msg_body = NewBody}, [#message{msg_body = OldBody}], _ActivityId}}, _State) when OldBody /= NewBody-> %msg_body update
    <<"chat_", ChatId/binary>> = erlang:atom_to_binary(Table, 'utf8'),
    Resp = #s2c_message_update{chat_id = ChatId, msg_id = MsgId, msg_body = NewBody},
    {Resp, _State};
do_action({mnesia_table_event, _}, _State) -> %redundant status update or bodies same, ignore
    {ok, _State};
do_action({notify, MSISDN, 'online', Pid}, #user_state{call = #call_info{sdp = SdpOffer, msisdn = MSISDN}, msisdn = MyMSISDN, turn_server = TurnServer} = State) when is_pid(Pid) andalso SdpOffer /= 'undefined' andalso TurnServer /= 'undefined'->
    Ref = monitor(process, Pid),
    Pid ! {call_offer, MyMSISDN, SdpOffer, self(), TurnServer},
    Resp = #s2c_user_status{msisdn = MSISDN, status = 'online'},
    {Resp, State#user_state{call = #call_info{pid = Pid, msisdn = MSISDN, ref = Ref}}};
do_action({notify, MSISDN, Status, _Pid}, _State) ->
    Resp = case Status of
               'offline' ->
                   case users:get(MSISDN) of
                       #user{last_visit_timestamp = LVTS} when is_number(LVTS) ->
                           #s2c_user_status{msisdn = MSISDN, status = <<"offline">>, last_visit_timestamp = LVTS};
                       #user{} ->
                           #s2c_user_status{msisdn = MSISDN, status = <<"offline">>};
                       _ ->
                           #s2c_user_status{msisdn = MSISDN, status = <<"not_exists">>}
                   end;
               _ ->
                   #s2c_user_status{msisdn = MSISDN, status = erlang:atom_to_binary(Status, 'utf8')}
               end,
    {Resp, _State};
do_action('undefined', _State) ->
    {ok, _State};
do_action(_Msg, _State) ->
    lager:debug("unknown message type: ~p", [_Msg]),
    Resp = #s2c_error{code = 501},
    {Resp, _State}.

terminate(#user_state{msisdn = MSISDN, storage = Storage} = _State) ->
    users:update_last_visit_timestamp(MSISDN),
    users:notify(MSISDN, 'offline'),
    users:unsubscribe(MSISDN),
    users:delete_pid(MSISDN),
    storage:set(MSISDN, Storage),
    ok.

%%%===================================================================
%%% Module access params
%%%===================================================================
allowed_groups() ->
    ['guest','sportsman','administrator','parent','trainer'].

access_level() ->
    10.

%%%===================================================================
%%% Internal functions
%%%===================================================================
map_to_record('room_tag', Map) ->
    map_to_record('room_tag', Map, 'false').
map_to_record('room_tag', Map, Default) ->
    #room_tag{tag1 = maps:get(<<"tag1">>, Map, Default)
             ,tag2 = maps:get(<<"tag2">>, Map, Default)
             ,tag3 = maps:get(<<"tag3">>, Map, Default)
             ,tag4 = maps:get(<<"tag4">>, Map, Default)
             ,tag5 = maps:get(<<"tag5">>, Map, Default)
             ,tag6 = maps:get(<<"tag6">>, Map, Default)
             ,tag7 = maps:get(<<"tag7">>, Map, Default)
             ,tag8 = maps:get(<<"tag8">>, Map, Default)
             ,tag9 = maps:get(<<"tag9">>, Map, Default)
             ,tag10= maps:get(<<"tag10">>, Map, Default)
             ,tag11= maps:get(<<"tag11">>, Map, Default)
             ,tag12= maps:get(<<"tag12">>, Map, Default)
             ,tag13= maps:get(<<"tag13">>, Map, Default)
             ,tag14= maps:get(<<"tag14">>, Map, Default)
             ,tag15= maps:get(<<"tag15">>, Map, Default)
             ,tag16= maps:get(<<"tag16">>, Map, Default)
             ,tag17= maps:get(<<"tag17">>, Map, Default)
             ,tag18= maps:get(<<"tag18">>, Map, Default)
             ,tag19= maps:get(<<"tag19">>, Map, Default)
             ,tag20= maps:get(<<"tag20">>, Map, Default)
             ,tag21= maps:get(<<"tag21">>, Map, Default)
             ,tag22= maps:get(<<"tag22">>, Map, Default)
             ,tag23= maps:get(<<"tag23">>, Map, Default)
             ,tag24= maps:get(<<"tag24">>, Map, Default)
             ,tag25= maps:get(<<"tag25">>, Map, Default)
             ,tag26= maps:get(<<"tag26">>, Map, Default)
             ,tag27= maps:get(<<"tag27">>, Map, Default)
             ,tag28= maps:get(<<"tag28">>, Map, Default)
             ,tag29= maps:get(<<"tag29">>, Map, Default)
             ,tag30= maps:get(<<"tag30">>, Map, Default)
             ,tag31= maps:get(<<"tag31">>, Map, Default)
             ,tag32= maps:get(<<"tag32">>, Map, Default)}.
