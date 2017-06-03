%%%-------------------------------------------------------------------
%% @doc push public API
%% @end
%%%-------------------------------------------------------------------

-module(push_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    PrivDir = code:priv_dir('push'),
    %% ApplePushConfig = #{'name' => apple_push
    %%                    ,'apple_host' => "api.push.apple.com"
    %%                    ,'apple_port' => 443
    %%                    ,'type' => cert
    %%                    ,'certfile' => PrivDir ++ "/apns-push-cert.pem"
    %%                    ,'keyfile' => PrivDir ++ "/apns-push-key.pem"},
    AppleVoipPushConfig = #{'name' => apple_voip_push
                           ,'apple_host' => "api.push.apple.com"
                           ,'apple_port' => 443
                           ,'type' => cert
                           ,'certfile' => PrivDir ++ "/apns-voip-push-cert.pem"
                           ,'keyfile' => PrivDir ++ "/apns-voip-push-key.pem"},
    %% {ok, _} = apns:connect(ApplePushConfig),
    {ok, _} = apns:connect(AppleVoipPushConfig),
    push_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    apns:stop(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
