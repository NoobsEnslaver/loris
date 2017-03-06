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
    Map;
decode(Bin, ?JSON) ->
    jsone:decode(Bin, {object_format, map}).

-spec encode(map() | list(), binary()) -> binary().
encode(Map, ?MSGPACK) ->
    msgpack:pack(Map, [{pack_str, from_binary}]);
encode(Data, ?JSON) ->
    jsone:encode(Data).



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
