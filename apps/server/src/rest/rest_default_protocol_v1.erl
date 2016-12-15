%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  14 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_default_protocol_v1).
-behaviour(rest_protocol_behaviour).
-export([get/2
        ,head/2
        ,post/2
        ,put/2
        ,patch/2
        ,delete/2
        ,options/2]).

-spec get(cowboy_req:req(), list()) -> cowboy_req:req().
get(Req, _Opts) ->
    QS = cowboy_req:parse_qs(Req),
    Data = ["<p>" ++ binary_to_list(Key) ++ " = "++ binary_to_list(Val) ++ "</p>" || {Key, Val} <- QS],
    BData = list_to_binary(Data),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, BData, Req).

-spec head(cowboy_req:req(), list()) -> cowboy_req:req().
head(Req, _Opts) ->
    cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req).

-spec post(cowboy_req:req(), list()) -> cowboy_req:req().
post(Req, _Opts) ->
    cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req).

-spec put(cowboy_req:req(), list()) -> cowboy_req:req().
put(Req, _Opts) ->
    cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req).

-spec patch(cowboy_req:req(), list()) -> cowboy_req:req().
patch(Req, _Opts) ->
    cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req).

-spec delete(cowboy_req:req(), list()) -> cowboy_req:req().
delete(Req, _Opts) ->
    cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req).

-spec options(cowboy_req:req(), list()) -> cowboy_req:req().
options(Req, _Opts) ->
    cowboy_req:reply(501, #{<<"content-type">> => <<"text/html">>}, <<"Not implemented">>, Req).
