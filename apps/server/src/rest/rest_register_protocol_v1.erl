%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2016
%%%-------------------------------------------------------------------
-module(rest_register_protocol_v1).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").

-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

%% Required fields:
%% msisdn|group|pwd_hash|fname|lname|age|is_male|access_level
-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{tmp_state = #{'session' := Session}} = State, _Args) ->
    lager:md([{'appname', <<"server->register">>}]),
    CurrentAccessLevel = sessions:extract(Session, 'access_level'),
    {IP, _Port} = cowboy_req:peer(Req),
    NewState = case common:get_body_data(Req) of
                   #{<<"msisdn">> := BMSISDN
                    ,<<"pwd_hash">> := Pwd
                    ,<<"group">> := BGroup
                    ,<<"fname">> := FName
                    ,<<"lname">> := LName
                    ,<<"age">> := BAge
                    ,<<"is_male">> := BIsMale
                    ,<<"access_level">> := AL1} when BGroup =:= <<"users">> orelse BGroup =:= <<"guests">> orelse BGroup =:= <<"administrators">> ->
                       Group = binary_to_existing_atom(BGroup, 'utf8'),
                       IsMale = case BIsMale of
                                    B when is_boolean(BIsMale) -> B;
                                    <<"true">> -> true;
                                    <<"false">> -> false;
                                    <<"male">> -> true;
                                    <<"female">> -> false
                                end,
                       MSISDN = if  is_binary(BMSISDN) -> binary_to_integer(BMSISDN);
                                    true -> BMSISDN
                                end,
                       Age = if  is_binary(BAge) -> binary_to_integer(BAge);
                                 true -> BAge
                             end,
                       case AL1 of
                           AL when is_number(AL) andalso AL > CurrentAccessLevel ->
                               lager:info("user id=~p creating user ~p with access level ~p on group ~p from IP: ~p", [sessions:extract(Session, 'owner_id'), MSISDN, AL, Group, IP]),
                               new_user(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AL, State);
                           <<"infinity">> ->
                               lager:info("user id=~p creating user ~p with access level ~p on group ~p from IP: ~p", [sessions:extract(Session, 'owner_id'), MSISDN, 'infinity', Group, IP]),
                               new_user(MSISDN, Pwd, FName, LName, Age, IsMale, Group, 'infinity', State);
                           BinNum when is_binary(BinNum) ->
                               AL = binary_to_integer(BinNum),
                               if  CurrentAccessLevel < AL->
                                       lager:info("user id=~p creating user ~p with access level ~p on group ~p from IP: ~p", [sessions:extract(Session, 'owner_id'), MSISDN, AL, Group, IP]),
                                       new_user(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AL, State);
                                   'true' ->
                                       lager:info("user id=~p try to create user ~p with access level ~p on group ~p: permission denied (not enought access right) from IP: ~p", [sessions:extract(Session, 'owner_id'), MSISDN, AL, Group, IP]),
                                       State#q_state{code = 403} % forbidden
                               end;
                           _ ->
                               lager:info("user id=~p try to create user ~p with unexpected access level on group ~p: permission denied (not enought access right) from IP: ~p", [sessions:extract(Session, 'owner_id'), MSISDN, Group, IP]),
                               State#q_state{code = 403} % forbidden
                       end;
                   _ ->
                       State#q_state{code = 400} % bad request
               end,
    {Req, NewState, []};
handle(<<"POST">>, Req, State, _Args) ->        %if no session and it's allowed - register anything
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
                    ,<<"access_level">> := AL1} when BGroup =:= <<"users">> orelse BGroup =:= <<"guests">> orelse BGroup =:= <<"administrators">> ->
                       Group = binary_to_existing_atom(BGroup, 'utf8'),
                       IsMale = case BIsMale of
                                    B when is_boolean(BIsMale) -> B;
                                    <<"true">> -> true;
                                    <<"false">> -> false;
                                    <<"male">> -> true;
                                    <<"female">> -> false
                                end,
                       MSISDN = if  is_binary(BMSISDN) -> binary_to_integer(BMSISDN);
                                    true -> BMSISDN
                                end,
                       Age = if  is_binary(BAge) -> binary_to_integer(BAge);
                                 true -> BAge
                             end,
                       AL = case AL1 of
                                <<"infinity">> -> 'infinity';
                                BinNum when is_binary(BinNum) -> binary_to_integer(BinNum);
                                Num when is_integer(Num) -> Num
                            end,
                       lager:info("unauthorized creating user ~p with access level ~p on group ~p from IP: ~p", [MSISDN, AL, Group, IP]),
                       new_user(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AL, State);
                   _ ->
                       State#q_state{code = 400} % bad request
                   end,
    {Req, NewState, []};
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, _Other) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/register.html"),
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
