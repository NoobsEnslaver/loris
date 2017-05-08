%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  21 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_auth_protocol_v1).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").

-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, _Req, #q_state{req_body = #{<<"user">> := Login, <<"password">> := PwdHash}} = State, Args) ->
    lager:md([{'appname', <<"server->auth">>}]),
    SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
    {IP, _Port} = cowboy_req:peer(_Req),
    NewState = case users:authorize(Login, PwdHash) of
                   'false' ->
                       lager:info("unauthorized with pair ~p:~p from IP: ~p", [Login, PwdHash, IP]),
                       State#q_state{code = 401}; %unauthorized
                   User ->
                       lager:info("user authorized with pair ~p:~p from IP: ~p", [Login, PwdHash, IP]),
                       Token = sessions:new(User, 'undefined', SessionLiveTime),
                       T = common:take_first(Args),
                       case lists:member(T, ?SUPPORTED_TRANSPORT) of
                           'true' ->
                               State#q_state{body = transport_lib:encode(#{<<"token">> => Token}, T), code = 200};
                           _False ->
                               State#q_state{body = Token, code = 200}
                       end
               end,
    {_Req, NewState, []};
handle(<<"POST">>, _Req, _State, _Args) ->
    {_Req, _State#q_state{code = 400}, _Args};  %bad request
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, _Other) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/auth.html"),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.       %method not allowed

access_level(_Method) ->
    'infinity'.

allowed_groups(_Method) ->
    ['users', 'administrators', 'guests'].
