%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2017
%%%-------------------------------------------------------------------
-module(rest_register_protocol_v2).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").

-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

%% Required fields:
%% msisdn|group|pwd_hash|fname|lname|age|is_male|access_level|sms_code
-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, State, _Args) ->
    lager:md([{'appname', <<"server->register">>}]),
    {IP, _Port} = cowboy_req:peer(Req),
    NewState = case common:get_body_data(Req) of
                   #{<<"msisdn">> := BMSISDN
                    ,<<"pwd_hash">> := Pwd
                    ,<<"group">> := BGroup
                    ,<<"fname">> := FName
                    ,<<"lname">> := LName
                    ,<<"age">> := BAge
                    ,<<"is_male">> := BIsMale
                    ,<<"sms_code">> := BSmsCode
                    ,<<"access_level">> := AL1} when BGroup =:= <<"users">> orelse BGroup =:= <<"guests">> orelse BGroup =:= <<"administrators">> ->
                       Group = binary_to_existing_atom(BGroup, 'utf8'),
                       IsMale = case BIsMale of
                                    B when is_boolean(BIsMale) -> B;
                                    <<"true">> -> true;
                                    <<"false">> -> false;
                                    <<"male">> -> true;
                                    <<"female">> -> false
                                end,
                       MSISDN = common:to_integer(BMSISDN),
                       Age = common:to_integer(BAge),
                       AL = case AL1 of
                                <<"infinity">> -> 'infinity';
                                Num -> common:to_integer(Num)
                            end,
                       SmsCode = common:to_integer(BSmsCode),
                       case sms:get(MSISDN) of
                           'false' ->
                               lager:info("Error on creating user ~p from IP: ~p - no sms on db", [MSISDN, IP]),
                               State#q_state{code = 428}; %no sms auth
                           SmsRecord ->
                               case sms:extract(SmsRecord, code) of
                                   SmsCode ->
                                       lager:info("Creating user ~p with access level ~p on group ~p from IP: ~p", [MSISDN, AL, Group, IP]),
                                       sms:delete(MSISDN),
                                       new_user(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AL, State);
                                   _Else ->
                                       io:format("_Else: ~p~n", [_Else]),
                                       lager:info("Error on creating user ~p from IP: ~p - incorrect sms code", [MSISDN, IP]),
                                       State#q_state{code = 403} %bad sms code
                               end
                       end;
                   _ ->
                       State#q_state{code = 400} % bad request
               end,
    {Req, NewState, []};
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
new_user(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AccessLevel, State) ->
    case users:new(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AccessLevel, 'nohash') of
        'exists' ->
            State#q_state{code = 409};          % conflict
        'false' ->
            State#q_state{code = 500};          % error
        _ ->
            State#q_state{code = 201}           % created
    end.
