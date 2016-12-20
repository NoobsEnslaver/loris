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
-export([get/3
        ,head/3
        ,post/3
        ,put/3
        ,patch/3
        ,delete/3
        ,options/3]).

-spec get(cowboy_req:req(), map(), [binary()]) -> {cowboy_req:req(), map(), [binary()]}.
get(Req, State, [Arg1 | [Arg2 | _Other]]) ->
    QS = cowboy_req:parse_qs(Req),
    Data1 = ["<p>" ++ binary_to_list(Key) ++ " = "++ binary_to_list(Val) ++ "</p>" || {Key, Val} <- QS],
    Data2 = Data1 ++ ["<h1>Arg1 = " ++ binary_to_list(Arg1) ++ "</h1><br>"],
    Data = Data2 ++ ["<h1>Arg2 = " ++ binary_to_list(Arg2) ++ "</h1><br>"],
    BData = list_to_binary(Data),
    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, BData, Req),
    {Resp, State, _Other};
get(Req, State, [Arg1]) ->
    get(Req, State, [Arg1, <<"none">>]);
get(Req, State, []) ->
    get(Req, State, [<<"none">>, <<"none">>]).




-spec head(cowboy_req:req(), map(), [binary()]) -> {cowboy_req:req(), map(), [binary()]}.
head(Req, _State, _Args) ->
    Resp = cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req),
    {Resp, _State, _Args}.

-spec post(cowboy_req:req(), map(), [binary()]) -> {cowboy_req:req(), map(), [binary()]}.
post(Req, _State, _Args) ->
    Resp = cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req),
    {Resp, _State, _Args}.

-spec put(cowboy_req:req(), map(), [binary()]) -> {cowboy_req:req(), map(), [binary()]}.
put(Req, _State, _Args) ->
    Resp = cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req),
    {Resp, _State, _Args}.

-spec patch(cowboy_req:req(), map(), [binary()]) -> {cowboy_req:req(), map(), [binary()]}.
patch(Req, _State, _Args) ->
    Resp = cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req),
    {Resp, _State, _Args}.

-spec delete(cowboy_req:req(), map(), [binary()]) -> {cowboy_req:req(), map(), [binary()]}.
delete(Req, _State, _Args) ->
    Resp = cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req),
    {Resp, _State, _Args}.

-spec options(cowboy_req:req(), map(), [binary()]) -> {cowboy_req:req(), map(), [binary()]}.
options(Req, _State, _Args) ->
    Resp = cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req),
    {Resp, _State, _Args}.
