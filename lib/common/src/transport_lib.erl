%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 7 Dec 2016
%%%-------------------------------------------------------------------
-module(transport_lib).
-include("transport_lib.hrl").

%% API
-export([decode/2
        ,encode/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================
-spec decode(binary(), binary()) -> map().
decode(Bin, ?MSGPACK) ->
    {'ok', Map} = msgpack:unpack(Bin, [{unpack_str, as_binary}]),
    Map.

-spec encode(map(), binary()) -> binary().
encode(Map, ?MSGPACK) ->
    msgpack:pack(Map, [{pack_str, from_binary}]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
