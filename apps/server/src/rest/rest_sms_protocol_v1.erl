%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  2 May 2017
%%%-------------------------------------------------------------------
-module(rest_sms_protocol_v1).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").

-define(TEST, true).
-define(SMS_SERVER_URL, "https://sms.ru/sms/send").
-ifdef(TEST).
-define(BODY(AppId, MSISDN, Code), "api_id=" ++ AppId ++ "&to=+" ++ erlang:integer_to_list(MSISDN) ++ "&text=" ++ erlang:integer_to_list(Code) ++ "&test=1").
-define(GET_APP_ID, {ok, "25B1DB2A-4F68-27D4-3FD6-4DC122309D6B"}).
-define(SMS_RESEND_INTERVAL, {ok, 60}).
-else.
-define(GET_APP_ID, application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sms_api_id')).
-define(BODY(AppId, MSISDN, Code), "api_id=" ++ AppId ++ "&to=+" ++ erlang:integer_to_list(MSISDN) ++ "&text=" ++ erlang:integer_to_list(Code)).
-define(SMS_RESEND_INTERVAL, application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sms_resend_interval')).
-endif.

-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

%% Required fields:
%% msisdn
-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{req_body = #{<<"msisdn">> := BMSISDN}} = State, _Args) ->
    {IP, _Port} = cowboy_req:peer(Req),
    {ok, ResendInterval} = ?SMS_RESEND_INTERVAL,
    {ok, AppId} = ?GET_APP_ID,
    {MSec, Sec, _} = erlang:timestamp(),
    Now = MSec * 1000000 + Sec,
    MSISDN = common:to_integer(BMSISDN),
    lager:debug("IP: ~p require to send sms to MSISDN: ~p~n", [IP, MSISDN]),
    NewState = case sms:new(MSISDN) of
                   {exists, TimeStamp, _Code} when Now - TimeStamp < ResendInterval ->  %too fast
                       State#q_state{code = 429};
                   {exists, _TimeStamp, Code} ->                                        %resend
                       sms:update_timestamp(MSISDN),
                       Resp = send_sms(AppId, MSISDN, Code),
                       State#q_state{code = Resp};
                   'false' ->
                       lager:error("Error on sms:new()", []),
                       State#q_state{code = 500};
                   Code ->
                       Resp = send_sms(AppId, MSISDN, Code),
                       State#q_state{code = Resp}
               end,
    {Req, NewState, _Args};
handle(<<"POST">>, _Req, State, _Args) ->
    {_Req, State#q_state{code = 400}, []};
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.

access_level(_Method) ->
    'infinity'.

allowed_groups(_Method) ->
    ['administrators', 'users', 'guests'].

%%====================================================================
%% Internal functions
%%====================================================================
send_sms(AppId, MSISDN, Code) ->
    case httpc:request(post, {?SMS_SERVER_URL, [], "application/x-www-form-urlencoded", ?BODY(AppId, MSISDN, Code)}, [], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            case erlang:list_to_integer(lists:takewhile(fun(X)-> X /= $\n end, RespBody)) of
                100 ->
                    lager:debug("sms sended successfuly to MSISDN: ~p~n", [MSISDN]),
                    200;
                232 ->
                    lager:debug("sms not sended to MSISDN: ~p: day limit~n", [MSISDN]),
                    429;
                _Other ->
                    lager:error("Error on send sms to MSISDN ~p, error: ~p~n", [MSISDN, _Other]),
                    500
            end;
        _Error ->
            lager:error("Error on send sms to MSISDN ~p, error: ~p~n", [MSISDN, _Error]),
            500
    end.
