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
    PrivDir = code:priv_dir(server),
    {ok, CertFile} = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'certfile'),
    {ok, KeyFile} = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'keyfile'),
    TcpOpts1 = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'tcp_params', [{'port', 8080}
                                                                                   ,{'buffer', 32768}
                                                                                   ,{'max_connections', 65536}]),
    TcpOpts = [{certfile, PrivDir ++ CertFile}|
               [{keyfile, PrivDir ++ KeyFile} | TcpOpts1]],
    io:format("TcpOptions: ~p~n", [TcpOpts]),
    Acceptors = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'acceptors', 100),
    StaticDir = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'static_dir', "/srv"),
    Dispatch = cowboy_router:compile(
                 [{'_',
                    [{"/", 'cowboy_static', {file, StaticDir ++"/index.html"}} %TODO: redirect to '/static'
                    ,{"/static", 'cowboy_static', {file, StaticDir ++"/index.html"}}
                    ,{"/static/[...]", 'cowboy_static', {'dir', StaticDir}}
                    ,{"/session/[:token]/ws/[:version]/[:protocol]", server_ws_handler, []}
                    ,{"/ws/[:version]/[:protocol]", server_ws_handler, []}
                    ,{"/:version/[...]", server_rest_handler, []}
                    ,{'_', 'server_404_handler', []}]
                  }]),
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    {'ok', _Pid} = cowboy:start_tls(?LISTENER_NAME, Acceptors, TcpOpts, ProtocolOpts),

    server_sup:start_link().               %dummy

%%--------------------------------------------------------------------
-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(_) ->
    cowboy:stop_listener(?LISTENER_NAME).

%%====================================================================
%% Internal functions
%%====================================================================
