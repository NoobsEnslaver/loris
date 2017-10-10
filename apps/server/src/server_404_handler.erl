%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 6 Dec 2016
%%%-------------------------------------------------------------------
-module(server_404_handler).
-behaviour(cowboy_handler).

-export([init/2
        ,terminate/3]).
-include("server.hrl").

-spec init(cowboy_req:req(), map()) -> {'ok', cowboy_req:req(), []}.
init(Req, _Opts) ->
    Resp = cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>}, <<"">>, Req),
    {'ok', Resp, []}.

terminate(_Reason, _Req, _State) ->
    'ok'.
