%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 7 Dec 2016
%%%-------------------------------------------------------------------
-module(ws_default_protocol_v1).
-include("ws_default_protocol_v1_messages.hrl").
-include("server.hrl").
-behaviour(ws_protocol_behaviour).
-export([construct_msg/1
        ,do_action/2
        ,wrap_data/3, wrap_data/4
        ,default_user_state/1
        ,allowed_groups/0
        ,access_level/0]).

-record(user_state, {authorized = 'false'}).

default_user_state('false')->
    #user_state{authorized = 'false'};
default_user_state(_Session)->
    #user_state{authorized = 'true'}.

-spec construct_msg(map()) -> msg_type().
construct_msg(_Msg = #{<<"msg_type">> := 1})  -> #some_bad_async_work{};
construct_msg(_Msg = #{<<"msg_type">> := 2})  -> #some_good_async_work{};
construct_msg(_Msg = #{<<"msg_type">> := 3})  -> #some_long_async_work{};
construct_msg(_Msg = #{<<"msg_type">> := 4})  -> #some_good_sync_work{};
construct_msg(_Msg = #{<<"msg_type">> := 5})  -> #some_bad_sync_work{};
construct_msg(_Msg = #{<<"msg_type">> := 6})  -> #sync_get_socket_info{};
construct_msg(_) -> 'undefined'.


-spec wrap_data(binary(), map(), binary()) -> binary().
wrap_data(Type, MapData, Transport) ->
    Data = #{<<"msg_type">> => Type
            ,<<"data">> => MapData},
    transport_lib:encode(Data, Transport).
wrap_data(Type, Id, MapData, Transport) ->
    Data = #{<<"msg_type">> => Type
            ,<<"req_id">> => Id
            ,<<"data">> => MapData},
    transport_lib:encode(Data, Transport).


-spec do_action(msg_type(), #user_state{}) -> {'ok', #user_state{}} | {'async', pid(), reference(), #user_state{}} | {Type :: binary(), Msg :: map() | integer(), #user_state{}}.
do_action(#some_bad_async_work{}, State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->        %async error test
                                                timer:sleep(2000),
                                                throw(permission_denied),
                                                #{<<"msg_name">> => <<"msg_get_users">>}
                                        end),
    {'async', Pid, Ref, State};
do_action(#some_good_async_work{}, State) ->             %async test
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(3000),
                                                #{<<"msg_type">> => <<"msg_get_aws">>}
                                        end),
    {'async', Pid, Ref, State};
do_action(#some_long_async_work{}, State) ->          %async timeout test
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(?ASYNC_WORK_TIMEOUT + 2000),
                                                #{<<"msg_name">> => <<"msg_update_msg">>}
                                        end),
    {'async', Pid, Ref, State};
do_action(#some_good_sync_work{}, State) ->
    {'ok', State};
do_action(#some_bad_sync_work{}, State) ->
    throw(permission_denied),
    {'ok', State};
do_action(#sync_get_socket_info{}, State) ->
    Data = <<"qwe">>,
    {<<"socket_info">>, Data, State};
do_action(_Msg, _State) ->
    lager:info("unknown message type: ~p", [_Msg]),
    {<<"error">>, <<"unknown_msg">>, _State}.

allowed_groups() ->
    ['users', 'administrators'].

access_level() ->
    10.
