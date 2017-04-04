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
%% login, password_hash, name, group, access_level :: number() | binary().
-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{tmp_state = #{'session' := Session}} = State, _Args) ->
    lager:md([{'appname', <<"server->register">>}]),
    CurrentAccessLevel = sessions:extract(Session, 'access_level'),
    {IP, _Port} = cowboy_req:peer(Req),
    NewState = case common:get_body_data(Req) of
                   #{<<"login">> := Login
                    ,<<"password_hash">> := Pwd
                    ,<<"name">> := Name
                    ,<<"group">> := BGroup
                    ,<<"access_level">> := AL1} when BGroup =:= <<"users">> orelse BGroup =:= <<"guests">> orelse BGroup =:= <<"administrators">> ->
                       Group = binary_to_existing_atom(BGroup, 'utf8'),
                       case AL1 of
                           AL when is_number(AL) andalso AL > CurrentAccessLevel ->
                               lager:info("user id=~p creating user ~p with access level ~p on group ~p from IP: ~p", [sessions:extract(Session, 'owner_id'), Login, AL, Group, IP]),
                               new_user(Login, Pwd, Name, Group, AL, State);
                           <<"infinity">> ->
                               lager:info("user id=~p creating user ~p with access level ~p on group ~p from IP: ~p", [sessions:extract(Session, 'owner_id'), Login, 'infinity', Group, IP]),
                               new_user(Login, Pwd, Name, Group, 'infinity', State);
                           BinNum when is_binary(BinNum) ->
                               AL = binary_to_integer(BinNum),
                               if  CurrentAccessLevel < AL->
                                       lager:info("user id=~p creating user ~p with access level ~p on group ~p from IP: ~p", [sessions:extract(Session, 'owner_id'), Login, AL, Group, IP]),
                                       new_user(Login, Pwd, Name, Group, AL, State);
                                   'true' ->
                                       lager:info("user id=~p try to create user ~p with access level ~p on group ~p: permission denied (not enought access right) from IP: ~p", [sessions:extract(Session, 'owner_id'), Login, AL, Group, IP]),
                                       State#q_state{code = 403} % forbidden
                               end;
                           _ ->
                               lager:info("user id=~p try to create user ~p with unexpected access level on group ~p: permission denied (not enought access right) from IP: ~p", [sessions:extract(Session, 'owner_id'), Login, Group, IP]),
                               State#q_state{code = 403} % forbidden
                       end;
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
    ['administrators'].

%%====================================================================
%% Internal functions
%%====================================================================
new_user(Login, Password, Name, Group, AL, State)->
    case users:new(Login, Password, Name, Group, AL, 'nohash') of
        'exists' ->
            State#q_state{code = 409};          % conflict
        'false' ->
            State#q_state{code = 500};          % error
        _ ->
            State#q_state{code = 201}           % created
    end.
