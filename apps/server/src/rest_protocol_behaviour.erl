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

-callback get(Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.
-callback head(Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.
-callback post(Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.
-callback put(Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.
-callback patch(Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.
-callback delete(Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.
-callback options(Req :: cowboy_req:req(), State :: #q_state{}, AllArgs :: [binary()]) ->
    {Resp :: cowboy_req:req(), NewState :: #q_state{}, NotMyArgs :: [binary()]}.
-callback required_auth() -> boolean().
-callback get_access_level() -> non_neg_integer() | 'infinity'.
-callback put_access_level() -> non_neg_integer() | 'infinity'.
-callback post_access_level() -> non_neg_integer() | 'infinity'.
-callback delete_access_level() -> non_neg_integer() | 'infinity'.
-callback patch_access_level() -> non_neg_integer() | 'infinity'.
-callback options_access_level() -> non_neg_integer() | 'infinity'.
-callback head_access_level() -> non_neg_integer() | 'infinity'.
