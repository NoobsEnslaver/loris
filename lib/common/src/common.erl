%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2017

-module(common).
-include("transport_lib.hrl").

%% API exports
-export([bin2hex/1
        ,get_body_data/1
        ,timestamp/0
        ,trace_it/1
        ,to_integer/1
        ,take_first/1, take_first/2]).

%%====================================================================
%% API functions
%%====================================================================
bin2hex(Bytes)->
    list_to_binary([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bytes]]).

get_body_data(Req)->
    case cowboy_req:has_body(Req) of
        'true' ->
            case cowboy_req:header(<<"content-type">>, Req) of
                <<"application/json", _/binary>> ->
                    {'ok', Bin, _Req2} = cowboy_req:read_body(Req),
                    transport_lib:decode(Bin, ?JSON);
                <<"application/msgpack", _/binary>> ->
                    {'ok', Bin, _Req2} = cowboy_req:read_body(Req),
                    transport_lib:decode(Bin, ?MSGPACK);
                <<"application/x-msgpack", _/binary>> ->
                    {'ok', Bin, _Req2} = cowboy_req:read_body(Req),
                    transport_lib:decode(Bin, ?MSGPACK);
                _ ->
                    {'ok', List, _Req2} = cowboy_req:read_urlencoded_body(Req),
                    maps:from_list(List)
            end;
        _False ->
            #{}
    end.

timestamp()->
    {MegaSecs,Secs,MicroSecs} = erlang:timestamp(),
        (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

trace_it(Module)->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(Module, '_', '_', []).

to_integer(X) when is_integer(X) -> X;
to_integer(X) when is_binary(X) -> binary_to_integer(X);
to_integer(X) when is_float(X) -> round(X);
to_integer(X) when is_list(X) -> list_to_integer(X).

take_first([])-> 'undefined';
take_first([Head | _Tail])-> Head.
take_first([], Def)-> Def;
take_first([Head | _Tail], _Def) -> Head.

%%====================================================================
%% Internal functions
%%====================================================================
