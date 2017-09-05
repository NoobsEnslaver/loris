%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2017
%%%-------------------------------------------------------------------
-module(server_acme_handler).
-behaviour(cowboy_handler).

-export([init/2
        ,handle/2
        ,terminate/3]).
-include("server.hrl").

-spec init(cowboy_req:req(), map()) -> {'ok', cowboy_req:req(), []}.
init(Req, _Opts) ->
    Key = cowboy_req:binding('acme_key', Req, <<"">>),
    Resp = case acme:get(Key) of
               'false' ->
                   cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>}, <<"">>, Req);
               Val ->
                   cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Val, Req)
           end,
    {'ok', Resp, []}.

handle(_Req, _State) ->
    {'shutdown', _Req, _State}.

terminate(_Reason, _Req, _State) ->
    'ok'.
