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
        ,options/3
        ,required_auth/0
        ,get_access_level/0
        ,head_access_level/0
        ,post_access_level/0
        ,put_access_level/0
        ,patch_access_level/0
        ,delete_access_level/0
        ,options_access_level/0]).

-spec post(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
post(Req, _State, _Args) ->
    Body = case cowboy_req:header(<<"content-type">>, Req) of
               <<"application/json">> ->
                   {'ok', B, _Req2} = cowboy_req:read_body(Req),
                   jsone:decode(B, [{object_format, proplist}]);
               _ ->
                   {'ok', B, _Req2} = cowboy_req:read_urlencoded_body(Req),
                   B
           end,
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
    {Req, NewState, []}.

-spec get(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
get(Req, #q_state{headers = Hdrs, body = Body} = State, _Other) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/auth.html"),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other}.

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

required_auth() ->
    'false'.
get_access_level()->
    'infinity'.
head_access_level() ->
    'infinity'.
post_access_level()->
    'infinity'.
put_access_level()->
    'infinity'.
patch_access_level()->
    'infinity'.
delete_access_level()->
    'infinity'.
options_access_level()->
    'infinity'.
