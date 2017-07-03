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
        ,notify_msg/1
        ,notify_invatation/1]).

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
    %% #device{{msisdn, id, type :: 'android' | 'ios' | 'ios_voip', push_token :: binary()}}
    lists:foreach(fun(D)->
                          case device:extract(D, 'type') of
                              'ios_voip' -> gen_server:cast('push_apple_server', {call, D});
                              _ -> ok
                          end
                  end, Devices),
    ok.

notify_msg(_MSISDN)->
    ok.

notify_invatation(_MSISDN)->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
