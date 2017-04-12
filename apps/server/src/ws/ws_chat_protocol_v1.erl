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
-behaviour(ws_protocol_behaviour).
-export([unwrap_msg/1
        ,do_action/2
        ,wrap_msg/2
        ,default_user_state/1
        ,allowed_groups/0
        ,access_level/0]).

-record(user_state, {}).

-define(R2M(Record, RecName),
        maps:from_list(lists:zip(record_info(fields, RecName), tl(tuple_to_list(Record))))). %TODO: это костыль

default_user_state('false')->
    #user_state{};
default_user_state(_Session)->
    #user_state{}.

%%%===================================================================
%%% Parse users message
%%%===================================================================
-spec unwrap_msg(map()) -> msg_type().
unwrap_msg(_Msg = #{<<"msg_type">> := 1})  -> #c2s_chat_get_list{};
unwrap_msg(_Msg = #{<<"msg_type">> := 2})  -> #c2s_chat_get_info{};
unwrap_msg(_Msg = #{<<"msg_type">> := 3})  -> #c2s_chat_create{};
unwrap_msg(_Msg = #{<<"msg_type">> := 4})  -> #c2s_chat_leave{};
unwrap_msg(_Msg = #{<<"msg_type">> := 5})  -> #c2s_chat_delete{};
unwrap_msg(_Msg = #{<<"msg_type">> := 6})  -> #c2s_chat_invite_user{};
unwrap_msg(_Msg = #{<<"msg_type">> := 7})  -> #c2s_chat_mute{};
unwrap_msg(_Msg = #{<<"msg_type">> := 8})  -> #c2s_chat_unmute{};
unwrap_msg(_Msg = #{<<"msg_type">> := 9})  -> #c2s_chat_typing{};
unwrap_msg(_Msg = #{<<"msg_type">> := 10})  -> #c2s_message_send{};
unwrap_msg(_Msg = #{<<"msg_type">> := 11})  -> #c2s_message_get_list{};
unwrap_msg(_Msg = #{<<"msg_type">> := 12})  -> #c2s_message_update{};
unwrap_msg(_Msg = #{<<"msg_type">> := 13})  -> #c2s_message_update_status{};
unwrap_msg(_Msg = #{<<"msg_type">> := 14})  -> #c2s_system_logout{};
unwrap_msg(_Msg = #{<<"msg_type">> := 15})  -> #c2s_user_get_info{};
unwrap_msg(_Msg = #{<<"msg_type">> := 16})  -> #c2s_user_get_status{};
unwrap_msg(_Msg = #{<<"msg_type">> := 17})  -> #c2s_user_set_info{};
unwrap_msg(_Msg = #{<<"msg_type">> := 18})  -> #c2s_user_search{};
unwrap_msg(_Msg = #{<<"msg_type">> := 19})  -> #c2s_room_get_tree{};
unwrap_msg(_Msg = #{<<"msg_type">> := 20})  -> #c2s_room_get_info{};
unwrap_msg(_Msg = #{<<"msg_type">> := 21})  -> #c2s_room_rename{};
unwrap_msg(_Msg = #{<<"msg_type">> := 22})  -> #c2s_room_add_user{};
unwrap_msg(_Msg = #{<<"msg_type">> := 23})  -> #c2s_room_del_user{};
unwrap_msg(_Msg = #{<<"msg_type">> := 24})  -> #c2s_room_add_subroom{};
unwrap_msg(_Msg = #{<<"msg_type">> := 25})  -> #c2s_room_create{};
unwrap_msg(_Msg = #{<<"msg_type">> := 26})  -> #c2s_room_delete{};
unwrap_msg(_Msg = #{<<"msg_type">> := 27})  -> #c2s_room_enter_to_chat{};
unwrap_msg(_Msg = #{<<"msg_type">> := 28})  -> #c2s_room_send_message{};

unwrap_msg(_) -> 'undefined'.


%%%===================================================================
%%% Prepare server response
%%%===================================================================
-spec wrap_msg(msg_type(), binary()) -> binary().
wrap_msg(#async_start{work_id = WorkId}, Transport) ->
    Data = #{<<"msg_type">> => 100
            ,<<"req_id">> => WorkId},
    transport_lib:encode(Data, Transport);
wrap_msg(#async_error{work_id = WorkId, error_code = EC}, Transport) ->
    Data = #{<<"msg_type">> => 101
            ,<<"req_id">> => WorkId
            ,<<"data">> => #{<<"error_code">> => EC}},
    transport_lib:encode(Data, Transport);
wrap_msg(#async_done{work_id= WorkId, result = Res}, Transport) ->
    Data = #{<<"msg_type">> => 101
            ,<<"req_id">> => WorkId
            ,<<"data">> => transport_lib:decode(wrap_msg(Res, Transport), Transport)},
    transport_lib:encode(Data, Transport);
wrap_msg(_Msg = #s2c_chat_list{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 102, ?R2M(_Msg, s2c_chat_list)), Transport);
wrap_msg(_Msg = #s2c_chat_info{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 103, ?R2M(_Msg, s2c_chat_info)), Transport);
wrap_msg(_Msg = #s2c_chat_create_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 104, ?R2M(_Msg, s2c_chat_create_result)), Transport);
wrap_msg(_Msg = #s2c_chat_leave_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 105, ?R2M(_Msg, s2c_chat_leave_result)), Transport);
wrap_msg(_Msg = #s2c_chat_delete_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 106, ?R2M(_Msg, s2c_chat_delete_result)), Transport);
wrap_msg(_Msg = #s2c_chat_invite_user_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 107, ?R2M(_Msg, s2c_chat_invite_user_result)), Transport);
wrap_msg(_Msg = #s2c_chat_mute_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 108, ?R2M(_Msg, s2c_chat_mute_result)), Transport);
wrap_msg(_Msg = #s2c_chat_unmute_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 109, ?R2M(_Msg, s2c_chat_unmute_result)), Transport);
wrap_msg(_Msg = #s2c_chat_typing{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 110, ?R2M(_Msg, s2c_chat_typing)), Transport);
wrap_msg(_Msg = #s2c_message{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 111, ?R2M(_Msg, s2c_message)), Transport);
wrap_msg(_Msg = #s2c_message_update{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 112, ?R2M(_Msg, s2c_message_update)), Transport);
wrap_msg(_Msg = #s2c_message_update_status{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 113, ?R2M(_Msg, s2c_message_update_status)), Transport);
wrap_msg(_Msg = #s2c_user_info{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 114, ?R2M(_Msg, s2c_user_info)), Transport);
wrap_msg(_Msg = #s2c_user_status{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 115, ?R2M(_Msg, s2c_user_status)), Transport);
wrap_msg(_Msg = #s2c_user_set_info_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 116, ?R2M(_Msg, s2c_user_set_info_result)), Transport);
wrap_msg(_Msg = #s2c_user_search_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 117, ?R2M(_Msg, s2c_user_search_result)), Transport);
wrap_msg(_Msg = #s2c_room_list{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 118, ?R2M(_Msg, s2c_room_list)), Transport);
wrap_msg(_Msg = #s2c_room_info{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 119, ?R2M(_Msg, s2c_room_info)), Transport);
wrap_msg(_Msg = #s2c_room_rename_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 120, ?R2M(_Msg, s2c_room_rename_result)), Transport);
wrap_msg(_Msg = #s2c_room_add_user_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 121, ?R2M(_Msg, s2c_room_add_user_result)), Transport);
wrap_msg(_Msg = #s2c_room_del_user_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 122, ?R2M(_Msg, s2c_room_del_user_result)), Transport);
wrap_msg(_Msg = #s2c_room_add_subroom_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 123, ?R2M(_Msg, s2c_room_add_subroom_result)), Transport);
wrap_msg(_Msg = #s2c_room_create_result{}, Transport) ->
    transport_lib:encode(maps:put(<<"msg_type">>, 124, ?R2M(_Msg, s2c_room_create_result)), Transport).


%%%===================================================================
%%% Handle users request
%%%===================================================================
-spec do_action(client_msg_type(), #user_state{}) -> {'ok', #user_state{}} | {'async', pid(), reference(), #user_state{}} | {Msg :: server_msg_type(), #user_state{}}.
do_action(_Msg = #c2s_chat_get_list{}, _State) ->
    Resp = #s2c_chat_list{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_get_info{}, _State) ->
    Resp = #s2c_chat_info{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_create{}, _State) ->
    Resp = #s2c_chat_create_result{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_leave{}, _State) ->
    Resp = #s2c_chat_leave_result{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_delete{}, _State) ->
    Resp = #s2c_chat_delete_result{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_invite_user{}, _State) ->
    Resp = #s2c_chat_invite_user_result{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_mute{}, _State) ->
    Resp = #s2c_chat_mute_result{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_unmute{}, _State) ->
    Resp = #s2c_chat_unmute_result{},
    {Resp, _State};
do_action(_Msg = #c2s_chat_typing{}, _State) ->
    Resp = #s2c_chat_typing{},
    {Resp, _State};
do_action(_Msg =  #c2s_message_send{}, _State) ->
    Resp = #s2c_message{},
    {Resp, _State};
do_action(_Msg =  #c2s_message_get_list{}, _State) ->
    Resp = #s2c_user_info{},
    {Resp, _State};
do_action(_Msg =  #c2s_message_update{}, _State) ->
    Resp = #s2c_user_status{},
    {Resp, _State};
do_action(_Msg = #c2s_system_logout{}, _State) ->
    Resp = #s2c_user_set_info_result{},
    {Resp, _State};
do_action(_Msg = #c2s_user_get_info{}, _State) ->
    Resp = #s2c_user_search_result{},
    {Resp, _State};
do_action(_Msg = #c2s_user_get_status{}, _State) ->
    Resp = #s2c_room_list{},
    {Resp, _State};
do_action(_Msg = #c2s_user_set_info{}, _State) ->
    Resp = #s2c_room_info{},
    {Resp, _State};
do_action(_Msg = #c2s_user_search{}, _State) ->
    Resp = #s2c_room_rename_result{},
    {Resp, _State};
do_action(_Msg = #c2s_room_get_tree{}, _State) ->
    Resp = #s2c_room_add_user_result{},
    {Resp, _State};
do_action(_Msg = #c2s_room_get_info{}, _State) ->
    Resp = #s2c_room_del_user_result{},
    {Resp, _State};
do_action(_Msg = #c2s_room_rename{}, _State) ->
    Resp = #s2c_room_add_subroom_result{},
    {Resp, _State};
do_action(_Msg = #c2s_room_add_user{}, _State) ->
    Resp = #s2c_room_create_result{},
{Resp, _State};
do_action(_Msg = #c2s_room_del_user{}, _State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(?ASYNC_WORK_TIMEOUT + 2000),
                                                #s2c_room_add_subroom_result{}
                                        end),
    {'async', Pid, Ref, _State};
do_action(_Msg = #c2s_room_add_subroom{}, _State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(1000),
                                                #s2c_room_del_user_result{}
                                        end),
    {'async', Pid, Ref, _State};
do_action(_Msg = #c2s_room_create{}, _State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(2000),
                                                #s2c_room_add_user_result{}
                                        end),
    {'async', Pid, Ref, _State};
do_action(_Msg = #c2s_room_delete{}, _State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(2000),
                                                #s2c_room_rename_result{}
                                        end),
    {'async', Pid, Ref, _State};
do_action(_Msg = #c2s_room_enter_to_chat{}, _State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(2000),
                                                #s2c_room_list{}
                                        end),
    {'async', Pid, Ref, _State};
do_action(_Msg = #c2s_room_send_message{}, _State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(2000),
                                                #s2c_user_info{}
                                        end),
    {'async', Pid, Ref, _State};
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
