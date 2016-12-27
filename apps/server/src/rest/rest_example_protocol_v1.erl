%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  14 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_example_protocol_v1).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").
-include_lib("common/include/tables.hrl").
-export([get/3
        ,head/3
        ,post/3
        ,put/3
        ,patch/3
        ,delete/3
        ,options/3]).

-spec get(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
get(Req, #q_state{headers = Hdrs, body = Body, tmp_state = #{session := #session{owner_id = OID}}} = State, [Arg1 | [Arg2 | _Other]]) ->
    #user{name = N} = users:get_by_id(OID),
    QS = cowboy_req:parse_qs(Req),
    Data1 = ["<p>" ++ binary_to_list(Key) ++ " = "++ binary_to_list(Val) ++ "</p>" || {Key, Val} <- QS],
    Data2 = Data1 ++ ["<h1>Arg1 = " ++ binary_to_list(Arg1) ++ "</h1><br>"],
    Data3 = Data2 ++ ["<h1>Arg2 = " ++ binary_to_list(Arg2) ++ "</h1><br>"],
    Data = Data3 ++ ["<h1>UserName = " ++ binary_to_list(N) ++ "</h1><br>"],
    BData = list_to_binary(Data),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
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

-spec post(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
post(Req, _State, _Args) ->
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
