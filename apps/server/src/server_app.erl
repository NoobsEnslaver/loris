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
-include_lib("common/include/logging.hrl").

-export([start/2
        ,stop/1]).

-define(LISTENER_NAME, 'websock_api').

%%====================================================================
%% API
%%====================================================================

-spec start('normal' | {'failover',atom()} | {'takeover',atom()}, any()) -> {'ok', pid()} | {'error', any()}.
start(_Type, _Args) ->
    lager:md([{'appname', ?APP_NAME}]),
    Conf = get_conf(node(), ?APP_NAME),
    TcpOpts = proplists:get_value('tcp_params', Conf),
    Acceptors = proplists:get_value('acceptors', Conf),
    StaticDir = proplists:get_value('static_dir', Conf),
    Dispatch = cowboy_router:compile(
                 [{'_',
                    [{"/static/[...]", 'cowboy_static', {'dir', StaticDir}}
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
get_conf(_Node, _AppName) ->
    [{'tcp_params', [{'port', 5555}
                    ,{'buffer', 32768}
                    ,{'max_connections', 65536}]}
    ,{'acceptors', 100}
    ,{'static_dir', "/srv"}].
