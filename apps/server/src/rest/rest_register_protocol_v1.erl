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
        ,access_level/1]).

%% Required fields:
%% login, password_hash, name, access_level :: number() | binary().
-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{tmp_state = #{'session' := Session}} = State, _Args) ->
    Body = common:get_body_data(Req),
    CurrentAccessLevel = sessions:extract(Session, 'access_level'),
    {<<"login">>, Login} = lists:keyfind(<<"login">>, 1, Body),
    {<<"password_hash">>, Pwd} = lists:keyfind(<<"password_hash">>, 1, Body),
    {<<"name">>, Name} = lists:keyfind(<<"name">>, 1, Body),
    NewState = case  lists:keyfind(<<"access_level">>, 1, Body) of
                   {<<"access_level">>, AL} when is_number(AL) andalso AL > CurrentAccessLevel ->
                       new_user(Login, Pwd, Name, AL, State);
                   {<<"access_level">>, <<"infinity">>} ->
                       new_user(Login, Pwd, Name, 'infinity', State);
                   {<<"access_level">>, BinNum} when is_binary(BinNum) ->
                       AL = binary_to_integer(BinNum),
                       if  CurrentAccessLevel < AL->
                               new_user(Login, Pwd, Name, AL, State);
                           'true' ->
                               State#q_state{code = 403} % forbidden
                       end;
                   _ ->
                       State#q_state{code = 403} % forbidden
               end,
    {Req, NewState, []};
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, _Other) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/register.html"),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.

access_level(_Method) ->
    100.

%%====================================================================
%% Internal functions
%%====================================================================
new_user(Login, Password, Name, AL, State)->
    case users:new(Login, Password, Name, AL, 'nohash') of
        'exists' ->
            State#q_state{code = 409};          % conflict
        'false' ->
            State#q_state{code = 500};          % error
        _ ->
            State#q_state{code = 201}           % created
    end.
