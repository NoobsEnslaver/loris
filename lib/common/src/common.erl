-module(common).
-include("transport_lib.hrl").

%% API exports
-export([bin2hex/1
        ,get_body_data/1]).

%%====================================================================
%% API functions
%%====================================================================
bin2hex(Bytes)->
    list_to_binary([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bytes]]).

get_body_data(Req)->
    case cowboy_req:header(<<"content-type">>, Req) of
        <<"application/json">> ->
            {'ok', Bin, _Req2} = cowboy_req:read_body(Req),
            transport_lib:decode(Bin, ?JSON);
        <<"application/msgpack">> ->
            {'ok', Bin, _Req2} = cowboy_req:read_body(Req),
            transport_lib:decode(Bin, ?MSGPACK);
        <<"application/x-msgpack">> ->
            {'ok', Bin, _Req2} = cowboy_req:read_body(Req),
            transport_lib:decode(Bin, ?MSGPACK);
        _ ->
            {'ok', List, _Req2} = cowboy_req:read_urlencoded_body(Req),
            maps:from_list(List)
    end.
%%====================================================================
%% Internal functions
%%====================================================================
