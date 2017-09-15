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
    EnableTLS = application:get_env('server', 'enable_tls', true) and not ?IS_TEST,
    TcpOpts = get_tcp_opts(EnableTLS),
    StaticDir = application:get_env('server', 'static_dir', "/srv"),
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
    ProtocolOpts = #{env => #{dispatch => Dispatch}
                    ,stream_handlers => [cowboy_compress_h, cowboy_stream_h]},
    {'ok', _Pid} = if EnableTLS-> lager:debug("start TLS"), cowboy:start_tls(?LISTENER_NAME, TcpOpts, ProtocolOpts);
                      'true'   -> lager:debug("start clear TCP"), cowboy:start_clear(?LISTENER_NAME, TcpOpts, ProtocolOpts)
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
    PrivDir = code:priv_dir(server) ++ "/",
    {ok, TlsOpts} = application:get_env('server', 'tls_params'),
    {ok, TcpOpts} = application:get_env('server', 'tcp_params'),
    DhFilePath = PrivDir ++ proplists:get_value('dhfile', TlsOpts),
    TlsOpts1 = proplists:delete('dhfile', TlsOpts),
    CertOpts = [{'dhfile', DhFilePath}],
    TlsOpts1 ++ TcpOpts ++ CertOpts;
get_tcp_opts('false') ->
    {ok, TcpOpts} = application:get_env('server', 'tcp_params'),
    TcpOpts.
