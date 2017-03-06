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
        ,access_level/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{body = B} = State, _Args) ->
    SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
    NewState = case common:get_body_data(Req) of
                   #{<<"user">> := Login
                    ,<<"password">> := PwdHash} ->
                       case users:authorize(Login, PwdHash) of
                           'false' ->
                               State#q_state{code = 401}; %unauthorized
                           User ->
                               Token = sessions:new(User, 'undefined', SessionLiveTime),
                               State#q_state{body = <<B/binary, Token/binary>>, code = 200}
                       end;
                   _ ->
                       State#q_state{code = 400} %bad request
               end,
    {Req, NewState, []};
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, _Other) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/auth.html"),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.       %method not allowed

access_level(_Method) ->
    'infinity'.
