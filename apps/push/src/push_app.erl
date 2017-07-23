%%%-------------------------------------------------------------------
%% @doc push public API
%% @end
%%%-------------------------------------------------------------------

-module(push_app).

-behaviour(application).

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
    #{ios_voip := IosTokens
     ,android := AndroidTokens} = device:get_tokens_by_type([CalleeMSISDN]),
    case AndroidTokens of
        [] -> ok;
        _ ->
            AndroidMessage = [{<<"data">>, [{<<"msisdn">>, CallerMSISDN}]}
                             ,{<<"time_to_live">>, 60}
                             ,{<<"priority">>, <<"high">>}],
            fcm:sync_push(push_android_server, AndroidTokens, AndroidMessage)
    end,
    lists:foreach(fun(T)->
                          gen_server:cast('push_apple_server', {call, T, CallerMSISDN})
                  end, IosTokens).

notify_msg(MSISDNs, ChatId, ChatName, MsgId, MsgBody)->
    #{ios := IosTokens
     ,android := AndroidTokens} = device:get_tokens_by_type(MSISDNs),
    lists:foreach(fun(U)->
                          pushes:put(U, MsgBody, ChatName)                      %for deferred loud push if required
                  end, MSISDNs),
    case AndroidTokens of
        [] -> ok;
        _ ->
            AndroidMessage = [{<<"notification">>, [{<<"body">>, MsgBody}
                                                   ,{<<"title">>, ChatName}]}
                             ,{<<"time_to_live">>,3600}
                             ,{<<"collapse_key">>, list_to_binary(pid_to_list(self()))}],
            fcm:sync_push(push_android_server, AndroidTokens, AndroidMessage)
    end,
    lists:foreach(fun(T)->
                          gen_server:cast('push_apple_server', {msg_silent, T, ChatId, MsgId})
                  end, IosTokens).

notify_msg_loud(MSISDN, ChatName, Msg, Badge)->
    #{ios := IosTokens} = device:get_tokens_by_type([MSISDN]),
    lists:foreach(fun(T)->
                          gen_server:cast('push_apple_server', {msg, T, ChatName, Msg, Badge})
                  end, IosTokens).

%%====================================================================
%% Internal functions
%%====================================================================
