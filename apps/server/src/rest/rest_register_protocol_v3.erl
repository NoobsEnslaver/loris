%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2017
%%%-------------------------------------------------------------------
-module(rest_register_protocol_v3).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").

-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

%% Required fields:
%% msisdn|sms_code
%% Available fields:
%% group|pwd_hash|fname|lname|age|is_male|access_level
-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{req_body = #{<<"msisdn">> := BMSISDN, <<"sms_code">> := BSmsCode} = ReqBody} = State, Args) ->
    {IP, _Port} = cowboy_req:peer(Req),
    Pwd = maps:get(<<"pwd_hash">>, ReqBody, <<>>),
    Group = case maps:get(<<"group">>, ReqBody, <<"users">>) of
                <<"users">> -> 'users';
                <<"guests">>-> 'guests';
                <<"administrators">> -> 'administrators';
                <<"company">> -> 'company'
            end,
    FName = maps:get(<<"fname">>, ReqBody, <<>>),
    LName = maps:get(<<"lname">>, ReqBody, <<>>),
    Age = common:to_integer(maps:get(<<"age">>, ReqBody, 0)),
    IsMale = case maps:get(<<"is_male">>, ReqBody, true) of
                 B when is_boolean(B) -> B;
                 <<"true">> -> 'true';
                 _ -> 'false'
             end,
    AL = case maps:get(<<"access_level">>, ReqBody, 0) of
             <<"infinity">> -> 'infinity';
             Num -> common:to_integer(Num)
         end,
    MSISDN = common:to_integer(BMSISDN),
    SmsCode = common:to_integer(BSmsCode),
    T = common:take_first(Args),
    NewState = case application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'register_with_sms', 'true') of
                   'true' ->
                       case sms:get(MSISDN) of
                           'false' ->
                               lager:info("Error on creating user ~p from IP: ~p - no sms on db", [MSISDN, IP]),
                               State#q_state{code = 428}; %no sms auth
                           SmsRecord ->
                               case sms:extract(SmsRecord, code) of
                                   SmsCode ->
                                       case users:new(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AL, 'nohash') of
                                           'exists' ->
                                               State#q_state{code = 409};      % conflict
                                           'false' ->
                                               State#q_state{code = 500};      % error
                                           User ->
                                               lager:info("Creating user ~p with access level ~p on group ~p from IP: ~p", [MSISDN, AL, Group, IP]),
                                               sms:delete(MSISDN),
                                               SessionLiveTime = application:get_env('server', 'sessions_live_time', 3600), %1 hour
                                               Token = sessions:new(User, SessionLiveTime),
                                               case lists:member(T, ?SUPPORTED_TRANSPORT) of
                                                   'true' ->
                                                       State#q_state{body = transport_lib:encode(#{<<"token">> => <<Token/binary>>}, T), code = 201}; % created
                                                   _False ->
                                                       State#q_state{body = <<Token/binary>>, code = 201}     % created
                                               end
                                       end;
                                   _Else ->
                                       lager:info("Error on creating user ~p from IP: ~p - incorrect sms code", [MSISDN, IP]),
                                       State#q_state{code = 403}      %bad sms code
                               end
                       end;
                   _False ->                    %no-sms registration
                       case users:new(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AL, 'nohash') of
                           'exists' ->
                               State#q_state{code = 409};      % conflict
                           'false' ->
                               State#q_state{code = 500};      % error
                           User ->
                               lager:info("Creating user ~p with access level ~p on group ~p from IP: ~p", [MSISDN, AL, Group, IP]),
                               SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
                               Token = sessions:new(User, SessionLiveTime),
                               case lists:member(T, ?SUPPORTED_TRANSPORT) of
                                   'true' ->
                                       State#q_state{body = transport_lib:encode(#{<<"token">> => <<Token/binary>>}, T), code = 201}; % created
                                   _False ->
                                       State#q_state{body = <<Token/binary>>, code = 201}     % created
                               end
                       end
               end,
    {Req, NewState, []};
handle(<<"POST">>, _Req, State, _Args) ->
    {_Req, State#q_state{code = 400}, []};
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, _Other) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/register2.html"),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.

access_level(_Method) ->
    'infinity'.

allowed_groups(_Method) ->
    ['administrators', 'users', 'guests'].

%%====================================================================
%% Internal functions
%%====================================================================
