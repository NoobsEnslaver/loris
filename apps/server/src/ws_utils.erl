%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 7 Dec 2016
%%%-------------------------------------------------------------------
-module(ws_utils).
-include_lib("common/include/transport_lib.hrl").

%% API
-export([is_going_upgrade_to/2
        ,supported_transport/1
        ,do_async_work/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================
-spec is_going_upgrade_to(cowboy_req:req(), binary()) -> boolean().
is_going_upgrade_to(Req, Protocol) ->
    Targets = cowboy_req:parse_header(<<"upgrade">>, Req, []),
    lists:member(Protocol, Targets).

-spec supported_transport(cowboy_req:req()) -> [binary()].
supported_transport(Req) ->
    Protocols = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req, []),
    [Protocol || Protocol <- Protocols, lists:member(Protocol, ?SUPPORTED_TRANSPORT)].

-spec do_async_work(fun(() -> map())) -> {pid(), reference()}.
do_async_work(Fun) ->
    Socket = self(),
    spawn_monitor(fun()->
                          Socket ! {'async_done', self(), Fun()}
                  end).

%%%===================================================================
%%% Internal functions
%%%===================================================================
