%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  14 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_protocol_behaviour).
-include("server.hrl").

-callback handle(method(), Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.

-callback access_level(method()) -> non_neg_integer() | 'infinity'.
