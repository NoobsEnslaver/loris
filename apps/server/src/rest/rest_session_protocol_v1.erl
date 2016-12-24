%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  23 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_session_protocol_v1).
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
post(_Req, #q_state{tmp_state = TmpState} = State, [Token | _Other]) ->
    {_Req, State#q_state{tmp_state = TmpState#{'session' => sessions:get(Token)}}, _Other}.

-spec get(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
get(Req, State, Args) ->
    post(Req, State, Args).

-spec head(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
head(Req, State, Args) ->
    post(Req, State, Args).

-spec put(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
put(Req, State, Args) ->
    post(Req, State, Args).

-spec patch(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
patch(Req, State, Args) ->
    post(Req, State, Args).

-spec delete(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
delete(Req, State, Args) ->
    post(Req, State, Args).

-spec options(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
options(Req, State, Args) ->
    post(Req, State, Args).
