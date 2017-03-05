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
-export([handle/4
        ,access_level/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body, tmp_state = #{session := Session}} = State, [Arg1 | [Arg2 | _Other]]) ->
    OID = sessions:extract(Session, 'owner_id'),
    Name = users:extract(users:get_by_id(OID), 'name'),
    QS = cowboy_req:parse_qs(Req),
    Data1 = ["<p>" ++ binary_to_list(Key) ++ " = "++ binary_to_list(Val) ++ "</p>" || {Key, Val} <- QS],
    Data2 = Data1 ++ ["<h1>Arg1 = " ++ binary_to_list(Arg1) ++ "</h1><br>"],
    Data3 = Data2 ++ ["<h1>Arg2 = " ++ binary_to_list(Arg2) ++ "</h1><br>"],
    Data = Data3 ++ ["<h1>UserName = " ++ binary_to_list(Name) ++ "</h1><br>"],
    BData = list_to_binary(Data),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, [Arg1 | [Arg2 | _Other]]) ->
    QS = cowboy_req:parse_qs(Req),
    Data1 = ["<p>" ++ binary_to_list(Key) ++ " = "++ binary_to_list(Val) ++ "</p>" || {Key, Val} <- QS],
    Data2 = Data1 ++ ["<h1>Arg1 = " ++ binary_to_list(Arg1) ++ "</h1><br>"],
    Data = Data2 ++ ["<h1>Arg2 = " ++ binary_to_list(Arg2) ++ "</h1><br>"],
    BData = list_to_binary(Data),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, _Other};
handle(<<"GET">>, Req, State, [Arg1]) ->
    handle(<<"GET">>, Req, State, [Arg1, <<"none">>]);
handle(<<"GET">>, Req, State, []) ->
    handle(<<"GET">>, Req, State, [<<"none">>, <<"none">>]);
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.       % method not implemented

access_level(_Method)->
    'infinity'.
