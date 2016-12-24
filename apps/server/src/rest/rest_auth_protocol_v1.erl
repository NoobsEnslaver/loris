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
-export([get/3
        ,head/3
        ,post/3
        ,put/3
        ,patch/3
        ,delete/3
        ,options/3]).

-spec post(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
post(Req, _State, _Args) ->
    {'ok', Body, Req2} = cowboy_req:read_urlencoded_body(Req),
    SessionLiveTime = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_live_time', 3600), %1 hour
    {<<"user">>, Login} = lists:keyfind(<<"user">>, 1, Body),
    {<<"password">>, PwdHash} = lists:keyfind(<<"password">>, 1, Body),
    NewState = case users:authorize(Login, PwdHash) of
                   'false' ->
                       #q_state{body = <<>>, code = 401, headers = #{}};
                   User ->
                       Token = sessions:new(User, 'undefined', SessionLiveTime),
                       #q_state{body = Token, code = 200, headers = #{}}
               end,
    {Req2, NewState, []}.

-spec get(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
get(Req, #q_state{headers = Hdrs, body = Body} = State, [Arg1 | [Arg2 | _Other]]) ->
    QS = cowboy_req:parse_qs(Req),
    Data1 = ["<p>" ++ binary_to_list(Key) ++ " = "++ binary_to_list(Val) ++ "</p>" || {Key, Val} <- QS],
    Data2 = Data1 ++ ["<h1>Arg1 = " ++ binary_to_list(Arg1) ++ "</h1><br>"],
    Data = Data2 ++ ["<h1>Arg2 = " ++ binary_to_list(Arg2) ++ "</h1><br>"],
    BData = list_to_binary(Data),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
get(Req, State, [Arg1]) ->
    get(Req, State, [Arg1, <<"none">>]);
get(Req, State, []) ->
    get(Req, State, [<<"none">>, <<"none">>]).

-spec head(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
head(Req, _State, _Args) ->
    Resp = #q_state{body = <<>>, code = 501, headers = #{}},
    {Req, Resp, []}.

-spec put(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
put(Req, _State, _Args) ->
    Resp = #q_state{body = <<>>, code = 501, headers = #{}},
    {Req, Resp, []}.

-spec patch(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
patch(Req, _State, _Args) ->
    Resp = #q_state{body = <<>>, code = 501, headers = #{}},
    {Req, Resp, []}.

-spec delete(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
delete(Req, _State, _Args) ->
    Resp = #q_state{body = <<>>, code = 501, headers = #{}},
    {Req, Resp, []}.

-spec options(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
options(Req, _State, _Args) ->
    Resp = #q_state{body = <<>>, code = 501, headers = #{}},
    {Req, Resp, []}.
