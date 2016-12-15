%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  14 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_protocol_behaviour).

-callback get(Req :: cowboy_req:req(), Opts :: list()) -> any().
-callback head(Req :: cowboy_req:req(), Opts :: list()) -> any().
-callback post(Req :: cowboy_req:req(), Opts :: list()) -> any().
-callback put(Req :: cowboy_req:req(), Opts :: list()) -> any().
-callback patch(Req :: cowboy_req:req(), Opts :: list()) -> any().
-callback delete(Req :: cowboy_req:req(), Opts :: list()) -> any().
-callback options(Req :: cowboy_req:req(), Opts :: list()) -> any().
