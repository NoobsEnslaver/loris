%%%-------------------------------------------------------------------
%% @doc push public API
%% @end
%%%-------------------------------------------------------------------

-module(push_app).

-behaviour(application).
-include_lib("common/include/tables.hrl").

%% Application callbacks
-export([start/2
        ,stop/1
        ,notify_call/2
        ,notify_msg/5
        ,notify_msg_loud/4]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    {ok, FcmApiKey} = application:get_env('push', 'fcm_api_key'),
    fcm:start('push_android_server', FcmApiKey),
    push_sup:start_link().

stop(_State) ->
    fcm:stop('push_android_server'),
    ok.

%%--------------------------------------------------------------------
notify_call(CalleeMSISDN, CallerMSISDN)->
    #{ios_voip := IosDevices
     ,android := AndroidDevices} = device:get_by_type([CalleeMSISDN]),
    case AndroidDevices of
        [] -> ok;
        _ ->
            AndroidMessage = [{<<"data">>, [{<<"msisdn">>, CallerMSISDN}
                                           ,{<<"type">>, <<"call">>}]}
                             ,{<<"time_to_live">>, 60}
                             ,{<<"priority">>, <<"high">>}],
            Resp = fcm:sync_push(push_android_server, [D#device.push_token || D <- AndroidDevices], AndroidMessage),
            BadTokens = [T || {T, Result} <- Resp, Result == <<"NotRegistered">> orelse Result == <<"InvalidRegistration">>],
            [device:delete(M, Id) || #device{msisdn = M, id = Id, push_token = T} <- AndroidDevices, lists:member(T, BadTokens)]
    end,
    [gen_server:cast('push_apple_server', {call, D, CallerMSISDN}) || D <- IosDevices],
    ok.

notify_msg(MSISDNs, ChatId, ChatName, MsgId, MsgBody)->
    #{ios := IosDevices
     ,android := AndroidDevices} = device:get_by_type(MSISDNs),
    lists:foreach(fun(U)->
                          pushes:put(U, MsgBody, ChatName)                      %for deferred loud push if required
                  end, MSISDNs),
    case AndroidDevices of
        [] -> ok;
        _ ->
            AndroidMessage = [{<<"notification">>, [{<<"body">>, MsgBody}
                                                   ,{<<"title">>, ChatName}]}
                             ,{<<"time_to_live">>,3600}
                             ,{<<"collapse_key">>, list_to_binary(pid_to_list(self()))}],
            Resp = fcm:sync_push(push_android_server, [D#device.push_token || D <- AndroidDevices], AndroidMessage),
            BadTokens = [T || {T, Result} <- Resp, Result == <<"NotRegistered">> orelse Result == <<"InvalidRegistration">>],
            [device:delete(M, Id) || #device{msisdn = M, id = Id, push_token = T} <- AndroidDevices, lists:member(T, BadTokens)]
    end,
    [gen_server:cast('push_apple_server', {msg, D, ChatId, MsgId}) || D <- IosDevices],
    ok.

notify_msg_loud(MSISDN, ChatName, Msg, Badge)->
    #{ios := IosDevices} = device:get_by_type([MSISDN]),
    [gen_server:cast('push_apple_server', {msg_loud, D, ChatName, Msg, Badge}) || D <- IosDevices],
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
