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
handle(<<"POST">>, _Req, #q_state{body = B} = State, []) ->
    lager:md([{'appname', <<"server->auth">>}]),
    SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
    {IP, _Port} = cowboy_req:peer(_Req),
    NewState = case common:get_body_data(_Req) of
                   #{<<"msisdn">> := BMSISDN
                    ,<<"password">> := PwdHash} ->
                       MSISDN = if  is_binary(BMSISDN) -> binary_to_integer(BMSISDN);
                                    true -> BMSISDN
                                end,
                       case users:authorize(MSISDN, PwdHash) of
                           'false' ->
                               lager:info("unauthorized with pair ~p:~p from IP: ~p", [MSISDN, PwdHash, IP]),
                               State#q_state{code = 401}; %unauthorized
                           User ->
                               lager:info("user ~p authorized with pair ~p:~p from IP: ~p", [User, MSISDN, PwdHash, IP]),
                               Token = sessions:new(User, 'undefined', SessionLiveTime),
                               State#q_state{body = <<B/binary, Token/binary>>, code = 200}
                       end;
                   _ ->
                       State#q_state{code = 400} %bad request
               end,
    {_Req, NewState, []};
handle(<<"POST">>, _Req, #q_state{body = B} = State, [T | _Args]) ->
    lager:md([{'appname', <<"server->auth">>}]),
    SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
    {IP, _Port} = cowboy_req:peer(_Req),
    IsSupportedTransport = lists:member(T, ?SUPPORTED_TRANSPORT),
    NewState = case common:get_body_data(_Req) of
                   #{<<"user">> := Login
                    ,<<"password">> := PwdHash}  when IsSupportedTransport == 'true' ->
                       case users:authorize(Login, PwdHash) of
                           'false' ->
                               lager:info("unauthorized with pair ~p:~p from IP: ~p", [Login, PwdHash, IP]),
                               State#q_state{code = 401}; %unauthorized
                           User ->
                               lager:info("user ~p authorized with pair ~p:~p from IP: ~p", [User, Login, PwdHash, IP]),
                               Token = sessions:new(User, 'undefined', SessionLiveTime),
                               State#q_state{body = transport_lib:encode(#{<<"token">> => <<B/binary, Token/binary>>}, T), code = 200}
                       end;
                   _ ->
                       State#q_state{code = 400} %bad request
               end,
    {_Req, NewState, _Args};
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
