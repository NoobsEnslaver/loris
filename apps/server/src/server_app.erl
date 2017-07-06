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
-ifdef(TEST).
-define(IS_TEST, true).
-else.
-define(IS_TEST, false).
-endif.

%%====================================================================
%% API
%%====================================================================

-spec start('normal' | {'failover',atom()} | {'takeover',atom()}, any()) -> {'ok', pid()} | {'error', any()}.
start(_Type, _Args) ->
    lager:md([{'appname', ?APP_NAME}]),
    EnableTLS = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'enable_tls', true),
    TcpOpts = get_tcp_opts(EnableTLS and not ?IS_TEST),
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
    {'ok', _Pid} = case proplists:get_value(certfile, TcpOpts) of
                       'undefined' ->
                           cowboy:start_clear(?LISTENER_NAME, Acceptors, TcpOpts, ProtocolOpts);
                       _ ->
                           cowboy:start_tls(?LISTENER_NAME, Acceptors, TcpOpts, ProtocolOpts)
                   end,
    server_sup:start_link().               %dummy

%%--------------------------------------------------------------------
-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(_) ->
    cowboy:stop_listener(?LISTENER_NAME).

%%====================================================================
%% Internal functions
%%====================================================================
get_tcp_opts('true') ->
    PrivDir = code:priv_dir(server),
    {ok, CertFile} = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'certfile'),
    {ok, KeyFile} = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'keyfile'),
    {ok, CacertFile} = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'cacertfile'),
    {ok, TcpOpts1} = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'tcp_params'),
    [{certfile, PrivDir ++ CertFile}
    ,{cacertfile, PrivDir ++ CacertFile}
    ,{keyfile, PrivDir ++ KeyFile}] ++ TcpOpts1;
get_tcp_opts('false') ->
    {ok, TcpOpts} = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'tcp_params'),
    TcpOpts.
