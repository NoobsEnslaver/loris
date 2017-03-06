-module(common).

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
            {'ok', B, _Req2} = cowboy_req:read_body(Req),
            jsone:decode(B, [{object_format, proplist}]);
        _ ->
            {'ok', B, _Req2} = cowboy_req:read_urlencoded_body(Req),
            B
    end.
%%====================================================================
%% Internal functions
%%====================================================================
