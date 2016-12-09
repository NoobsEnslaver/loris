%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2016
%%%-------------------------------------------------------------------
-module(server_app).
-behaviour(application).

-include("server.hrl").

-export([start/2
        ,stop/1]).

-define(LISTENER_NAME, 'server').

%%====================================================================
%% API
%%====================================================================

-spec start('normal' | {'failover',atom()} | {'takeover',atom()}, any()) -> {'ok', pid()} | {'error', any()}.
start(_Type, _Args) ->
    lager:md([{'appname', ?APP_NAME}]),
    TcpOpts = application:get_env(?APP_NAME, 'tcp_params', [{'port', 8080}
                                                           ,{'buffer', 32768}
                                                           ,{'max_connections', 65536}]),
    Acceptors = application:get_env(?APP_NAME, 'acceptors', 100),
    StaticDir = application:get_env(?APP_NAME, 'static_dir', "/srv"),
    MaxFileSize = application:get_env(?APP_NAME, 'max_file_size', 16777216),
    Dispatch = cowboy_router:compile(
                 [{'_',
                    [{"/static/[...]", 'cowboy_static', {'dir', StaticDir}}
                    ,{"/ws/[:protocol]/[:version]", server_ws_handler, []}
                    ,{"/upload/[:token]", file_upload_handler, [MaxFileSize]}
                    ,{'_', 'server_404_handler', []}]
                  }]),
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    {'ok', _Pid} = cowboy:start_clear(?LISTENER_NAME, Acceptors, TcpOpts, ProtocolOpts),
    server_sup:start_link().               %dummy

%%--------------------------------------------------------------------
-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(_) ->
    cowboy:stop_listener(?LISTENER_NAME).

%%====================================================================
%% Internal functions
%%====================================================================
