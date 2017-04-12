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
-behaviour(ws_protocol_behaviour).
-export([unwrap_msg/1
        ,do_action/2
        ,wrap_msg/2
        ,default_user_state/1
        ,allowed_groups/0
        ,access_level/0]).

-record(user_state, {authorized = 'false'}).

default_user_state('false')->
    #user_state{authorized = 'false'};
default_user_state(_Session)->
    #user_state{authorized = 'true'}.

-spec unwrap_msg(map()) -> client_msg_type().
unwrap_msg(_Msg = #{<<"msg_type">> := 1})  -> #some_bad_async_work{};
unwrap_msg(_Msg = #{<<"msg_type">> := 2})  -> #some_good_async_work{};
unwrap_msg(_Msg = #{<<"msg_type">> := 3})  -> #some_long_async_work{};
unwrap_msg(_Msg = #{<<"msg_type">> := 4})  -> #some_good_sync_work{};
unwrap_msg(_Msg = #{<<"msg_type">> := 5})  -> #some_bad_sync_work{};
unwrap_msg(_Msg = #{<<"msg_type">> := 6})  -> #sync_get_socket_info{};
unwrap_msg(_) -> 'undefined'.


-spec wrap_msg(server_msg_type(), binary()) -> binary().
wrap_msg('error', Transport) ->
    transport_lib:encode(#{<<"error">> => <<"500">>}, Transport);
wrap_msg(#async_start{work_id = WID}, Transport) -> %Не забываем про встроенные типы сообщений:
    Data = #{<<"msg_type">> => 1                    %async_start, async_done, async_error
            ,<<"work_id">> => WID},
    transport_lib:encode(Data, Transport);
wrap_msg(#async_done{work_id = WID, result = R}, Transport) ->
    Data = #{<<"msg_type">> => 2
            ,<<"work_id">> => WID
            ,<<"data">> => transport_lib:decode(wrap_msg(R, Transport), Transport)}, %вложенные map конвертируются хорошо, а когда уже закодирована часть - ошибка
    transport_lib:encode(Data, Transport);
wrap_msg(#async_error{work_id = WID, error_code = EC}, Transport) ->
    Data = #{<<"msg_type">> => 3
            ,<<"work_id">> => WID
            ,<<"error_code">> => EC},
    transport_lib:encode(Data, Transport);
wrap_msg(#some_server_response_1{field1 = F1, field2 = F2}, Transport) ->
    Data = #{<<"msg_type">> => 4
            ,<<"field1">> => F1
            ,<<"field2">> => F2},
    transport_lib:encode(Data, Transport);
wrap_msg(#some_server_response_2{field3 = F3, field4 = F4}, Transport) ->
    Data = #{<<"msg_type">> => 5
            ,<<"field3">> => F3
            ,<<"field4">> => F4},
    transport_lib:encode(Data, Transport);
wrap_msg(#some_server_response_3{field5 = F5, field6 = F6, field7 = F7}, Transport) ->
    Data1 = transport_lib:encode(#{<<"field5">> => F5
                                  ,<<"field6">> => F6
                                  ,<<"field7">> => F7}, Transport),
    Data = #{<<"msg_type">> => 6
            ,<<"data">> => Data1},
    transport_lib:encode(Data, Transport);
wrap_msg(_Msg, _Transport) ->
    lager:info("Try to wrap unknown message type: ~p~n", [_Msg]),
    <<>>.


-spec do_action(client_msg_type(), #user_state{}) -> {'ok', #user_state{}} | {'async', pid(), reference(), #user_state{}} | {Msg :: server_msg_type(), #user_state{}}.
do_action(#some_bad_async_work{}, State) ->
    {Pid, Ref} = ws_utils:do_async_work(fun () ->        %async error test
                                                timer:sleep(2000),
                                                throw(permission_denied),
                                                #some_server_response_1{field1 = <<"try">>
                                                                       ,field2 = <<"internal error!">>}
                                        end),
    {'async', Pid, Ref, State};
do_action(#some_good_async_work{}, State) ->             %async test
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(3000),
                                                #some_server_response_2{field3 = <<"try">>
                                                                       ,field4 = <<"async!">>}
                                        end),
    {'async', Pid, Ref, State};
do_action(#some_long_async_work{}, State) ->          %async timeout test
    {Pid, Ref} = ws_utils:do_async_work(fun () ->
                                                timer:sleep(?ASYNC_WORK_TIMEOUT + 2000),
                                                #some_server_response_3{field5 = <<"try">>
                                                                       ,field6 = <<"async">>
                                                                       ,field7 = <<"timeout!">>}
                                        end),
    {'async', Pid, Ref, State};
do_action(#some_good_sync_work{}, State) ->
    {'ok', State};
do_action(#some_bad_sync_work{}, State) ->
    throw(permission_denied),
    {'ok', State};
do_action(#sync_get_socket_info{}, State) ->
    Data = #some_server_response_2{field3 = <<"Some info">>},
    {Data, State};
do_action(_Msg, _State) ->
    lager:info("unknown message type: ~p", [_Msg]),
    {'error', _State}.                          %не забываем написать хэндлер на 'error' в wrap_msg

allowed_groups() ->
    ['users', 'administrators'].

access_level() ->
    10.
