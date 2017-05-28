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
    TC = common:start_measure('decode_msgpack'),
    {'ok', Map} = msgpack:unpack(Bin, [{unpack_str, as_binary}]),
    common:end_measure('decode_msgpack', TC),
    Map;
decode(Bin, ?JSON) ->
    TC = common:start_measure('decode_json'),
    Map = jsone:decode(Bin),
    common:end_measure('decode_json', TC),
    Map.

-spec encode(map() | list(), binary()) -> binary().
encode(Map, ?MSGPACK) ->
    TC = common:start_measure('encode_msgpack'),
    Bin = msgpack:pack(Map, [{pack_str, from_binary}]),
    common:end_measure('encode_msgpack', TC),
    Bin;
encode(Data, ?JSON) ->
    TC = common:start_measure('encode_json'),
    Bin = jsone:encode(Data, [{object_key_type, scalar}]),
    common:end_measure('encode_json', TC),
    Bin.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
