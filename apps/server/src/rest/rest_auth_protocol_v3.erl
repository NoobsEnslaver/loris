%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2017
%%%-------------------------------------------------------------------
-module(rest_auth_protocol_v3).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").

-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{body = B} = State, []) ->
    lager:md([{'appname', <<"server->auth3">>}]),
    SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
    AllowPwdAuthorization = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'allow_password_authorization', 'false'),
    AllowSmsAuthorization = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'allow_sms_authorization', 'true'),
    {IP, _Port} = cowboy_req:peer(Req),
    case common:get_body_data(Req) of
        #{<<"msisdn">> := BMSISDN
         ,<<"sms_code">> := BCode} when AllowSmsAuthorization == 'true'->
            MSISDN = common:to_integer(BMSISDN),
            SmsCode = common:to_integer(BCode),
            case users:get(MSISDN) of
                'false' ->
                    lager:info("no user ~p, redirect to registration", [MSISDN]),
                    {Req, State, [<<"register">>]}; %redirect to registration
                User ->
                    case sms:get(MSISDN) of
                        'false' ->
                            lager:info("unauthorized ~p from IP: ~p - no sms on db", [MSISDN, IP]),
                            {Req, State#q_state{code = 428}, []}; %no sms in db
                        SmsRecord ->
                            case sms:extract(SmsRecord, code) of
                                SmsCode ->
                                    lager:info("user authorized ~p from IP: ~p", [MSISDN, IP]),
                                    sms:delete(MSISDN),
                                    Token = sessions:new(User, 'undefined', SessionLiveTime),
                                    {Req, State#q_state{body = <<B/binary, Token/binary>>, code = 200}, []};
                                _Else ->
                                    lager:info("unauthorized ~p from IP: ~p - incorrect sms code", [MSISDN, IP]),
                                    {Req, State#q_state{code = 401}, []} %incorrect sms code
                            end
                    end
            end;
        #{<<"msisdn">> := BMSISDN
         ,<<"password">> := PwdHash} when AllowPwdAuthorization == 'true' ->
            MSISDN = common:to_integer(BMSISDN),
            case users:authorize(MSISDN, PwdHash) of
                'false' ->
                    lager:info("unauthorized with pair ~p:~p from IP: ~p", [MSISDN, PwdHash, IP]),
                    {Req, State#q_state{code = 401}, []}; %unauthorized
                User ->
                    lager:info("user authorized ~p from IP: ~p", [MSISDN, IP]),
                    Token = sessions:new(User, 'undefined', SessionLiveTime),
                    {Req, State#q_state{body = <<B/binary, Token/binary>>, code = 200}, []}
            end;
        #{<<"msisdn">> := _, <<"sms_code">> := _} when AllowSmsAuthorization == 'false'->
            {Req, State#q_state{code = 403}, []};
        #{<<"msisdn">> := _, <<"password">> := _} when AllowPwdAuthorization == 'false'->
            {Req, State#q_state{code = 403}, []};
        _ ->
            {Req, State#q_state{code = 400}, []} %bad request
    end;
handle(<<"POST">>, Req, #q_state{body = B} = State, [T | _Args]) ->
    lager:md([{'appname', <<"server->auth3">>}]),
    SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
    AllowPwdAuthorization = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'allow_password_authorization', 'false'),
    AllowSmsAuthorization = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'allow_sms_authorization', 'true'),
    IsSupportedTransport = lists:member(T, ?SUPPORTED_TRANSPORT),
    {IP, _Port} = cowboy_req:peer(Req),
    case common:get_body_data(Req) of
        #{<<"msisdn">> := BMSISDN
         ,<<"sms_code">> := BCode} when AllowSmsAuthorization == 'true', IsSupportedTransport == 'true' ->
            MSISDN = common:to_integer(BMSISDN),
            SmsCode = common:to_integer(BCode),
            case users:get(MSISDN) of
                'false' ->
                    lager:info("no user ~p, redirect to registration", [MSISDN]),
                    {Req, State, [<<"register">>, T]}; %redirect to registration
                User ->
                    case sms:get(MSISDN) of
                        'false' ->
                            lager:info("unauthorized ~p from IP: ~p - no sms on db", [MSISDN, IP]),
                            {Req, State#q_state{code = 428}, []}; %no sms in db
                        SmsRecord ->
                            case sms:extract(SmsRecord, code) of
                                SmsCode ->
                                    lager:info("user authorized ~p from IP: ~p", [MSISDN, IP]),
                                    sms:delete(MSISDN),
                                    Token = sessions:new(User, 'undefined', SessionLiveTime),
                                    {Req, State#q_state{body = transport_lib:encode(#{<<"token">> => <<B/binary, Token/binary>>}, T), code = 200}, []};
                                _Else ->
                                    lager:info("unauthorized ~p from IP: ~p - incorrect sms code", [MSISDN, IP]),
                                    {Req, State#q_state{code = 401}, []} %incorrect sms code
                            end
                    end
            end;
        #{<<"msisdn">> := BMSISDN
         ,<<"password">> := PwdHash} when AllowPwdAuthorization == 'true', IsSupportedTransport == 'true' ->
            MSISDN = common:to_integer(BMSISDN),
            case users:authorize(MSISDN, PwdHash) of
                'false' ->
                    lager:info("unauthorized with pair ~p:~p from IP: ~p", [MSISDN, PwdHash, IP]),
                    {Req, State#q_state{code = 401}, []}; %unauthorized
                User ->
                    lager:info("user authorized ~p from IP: ~p", [MSISDN, IP]),
                    Token = sessions:new(User, 'undefined', SessionLiveTime),
                    {Req, State#q_state{body = transport_lib:encode(#{<<"token">> => <<B/binary, Token/binary>>}, T), code = 200}, []}
            end;
        #{<<"msisdn">> := _, <<"sms_code">> := _} when AllowSmsAuthorization == 'false'->
            {Req, State#q_state{code = 403}, []};
        #{<<"msisdn">> := _, <<"password">> := _} when AllowPwdAuthorization == 'false'->
            {Req, State#q_state{code = 403}, []};
        _ ->
            {Req, State#q_state{code = 400}, []} %bad request
    end;
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, _Other) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/auth2.html"),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.       %method not allowed

access_level(_Method) ->
    'infinity'.

allowed_groups(_Method) ->
    ['users', 'administrators', 'guests'].
