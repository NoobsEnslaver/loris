%%%-------------------------------------------------------------------
%% @doc push public API
%% @end
%%%-------------------------------------------------------------------

-module(push_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1
        ,notify_call/1
        ,notify_msg/4
        ,notify_msg_silent/3]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    push_sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
notify_call(MSISDN)->
    Devices = device:get(MSISDN),
    lists:foreach(fun(D)->
                          case device:extract(D, 'type') of
                              'ios_voip' -> gen_server:cast('push_apple_server', {call, D});
                              _ -> ok
                          end
                  end, Devices).

notify_msg_silent(MSISDN, ChatId, MsgId)->
    Devices = device:get(MSISDN),
    lists:foreach(fun(D)->
                          case device:extract(D, 'type') of
                              'ios' -> gen_server:cast('push_apple_server', {msg_silent, D, ChatId, MsgId});
                              _ -> ok
                          end
                  end, Devices).

notify_msg(_MSISDN, _User, _Msg, _Badge)->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
