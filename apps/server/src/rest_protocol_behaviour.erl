%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  14 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_protocol_behaviour).

-callback get(Req :: cowboy_req:req(), State :: map(), AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: map(), NotMyArgs :: [binary()]}.
-callback head(Req :: cowboy_req:req(), State :: map(), AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: map(), NotMyArgs :: [binary()]}.
-callback post(Req :: cowboy_req:req(), State :: map(), AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: map(), NotMyArgs :: [binary()]}.
-callback put(Req :: cowboy_req:req(), State :: map(), AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: map(), NotMyArgs :: [binary()]}.
-callback patch(Req :: cowboy_req:req(), State :: map(), AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: map(), NotMyArgs :: [binary()]}.
-callback delete(Req :: cowboy_req:req(), State :: map(), AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: map(), NotMyArgs :: [binary()]}.
-callback options(Req :: cowboy_req:req(), State :: map(), AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: map(), NotMyArgs :: [binary()]}.
