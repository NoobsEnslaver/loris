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
        ,trace_it/1, trace_it/2
        ,to_integer/1
        ,take_first/1, take_first/2
        ,start_measure/1
        ,end_measure/2
        ,stringify/1]).

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
                <<"multipart/form-data", _/binary>> -> %no reading - it's file
                    #{};
                _ ->
                    {'ok', List, _Req2} = cowboy_req:read_urlencoded_body(Req),
                    maps:from_list(List)
            end;
        _False ->
            #{}
    end.

timestamp()->
    {MegaSecs,Secs,MicroSecs} = erlang:timestamp(),
    round((MegaSecs*1000000 + Secs)*1000 + MicroSecs/1000). %in miliseconds

trace_it(Module)->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(Module, '_', '_', []).
trace_it(Module, Func)->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(Module, Func, '_', []).

to_integer(X) when is_integer(X) -> X;
to_integer(X) when is_binary(X) -> binary_to_integer(X);
to_integer(X) when is_float(X) -> round(X);
to_integer(X) when is_list(X) -> list_to_integer(X).

take_first([])-> 'undefined';
take_first([Head | _Tail])-> Head.
take_first([], Def)-> Def;
take_first([Head | _Tail], _Def) -> Head.

start_measure(Name) ->
    BName = erlang:atom_to_binary(Name, 'utf8'),
    CounterName = <<BName/binary, "_counter">>,
    MeterName = <<BName/binary, "_meter">>,
    case folsom_metrics:metric_exists(CounterName) of
        'true' -> ok;
        'false'-> folsom_metrics:new_counter(CounterName)
    end,
    case folsom_metrics:metric_exists(MeterName) of
        'true' -> ok;
        'false'-> folsom_metrics:new_meter(MeterName)
    end,
    folsom_metrics:notify(CounterName, {inc, 1}),
    os:system_time().

end_measure(Name, TC) ->
    BName = erlang:atom_to_binary(Name, 'utf8'),
    MeterName = <<BName/binary, "_meter">>,
    Duration = os:system_time() - TC,
    folsom_metrics:notify(MeterName, Duration/1000), %uSec
    ok.

stringify(X) when is_atom(X) -> erlang:atom_to_binary(X, 'utf8');
stringify(X) when is_tuple(X)->
    TList = erlang:tuple_to_list(X),
    BList = [stringify(A) || A <- TList],
    erlang:iolist_to_binary(BList);
stringify(X) when is_list(X) -> list_to_binary(X);
stringify(X) when is_pid(X) -> stringify(erlang:pid_to_list(X));
stringify(X) when is_number(X) orelse is_binary(X) -> X.

%%====================================================================
%% Internal functions
%%====================================================================
