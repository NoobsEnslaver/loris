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
        ,trace_it/1, trace_it/2, trace_it/3
        ,to_integer/1
        ,take_first/1, take_first/2
        ,start_measure/1
        ,end_measure/2
        ,stringify/1
        ,get_limited_amount_from_query/2
        ,remove/2
        ,intersection/2
        ,select_oldest_node/0
        ,get_cluster_mnesia_state/0]).

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
    os:system_time('millisecond').

%% Если WhatPrint - строка, то указываем pid строкой "<X.Y.Z>" - будет трассироваться он.
-spec trace_it(module(), atom(), 'processes'|'new_processes'|'existing_processes'|string())-> any().
trace_it(Module)->
    trace_it(Module, '_').
trace_it(Module, Func)->
    trace_it(Module, Func, processes).
trace_it(Module, Func, WhatPrint)->
    dbg:tracer(),
    dbg:p(WhatPrint, c),
    dbg:tpl(Module, Func, '_', [{'_',[],[{return_trace}]}]).

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

get_limited_amount_from_query(Q, Count) ->
    fun()->
            Cursor = qlc:cursor(Q),
            Resp = qlc:next_answers(Cursor, Count),
            qlc:delete_cursor(Cursor),
            Resp
    end.

remove(Value, [{_,_} | _] = Proplist)->
    [{A,B} || {A,B} <- Proplist, A /= Value, B /= Value];
remove(Value, Map) when is_map(Map) ->
    maps:from_list(remove(Value, maps:to_list(Map))).

intersection(List1, List2) ->
    [X || X <- List1, lists:member(X, List2)].


select_oldest_node() ->
    element(1, lists:foldl(fun(Node2, {Node1, UpTime1})->
                                   case rpc:call(Node2, erlang, statistics, [wall_clock], 5000) of
                                       {UpTime2, _} when is_number(UpTime2) andalso UpTime2 > UpTime1 ->
                                           {Node2, UpTime2};
                                       _ ->
                                           {Node1, UpTime1}
                                   end
                           end, {node(), element(1, statistics(wall_clock))}, nodes())).

get_cluster_mnesia_state()->
    lists:foldl(fun(Node, Acc)->
                        case {rpc:call(Node, application, info, [], 5000), rpc:call(Node, mnesia, system_info, [db_nodes], 5000)} of
                            {{badrpc, _Reason}, _} ->
                                lager:debug("rpc error on 'get_cluster_mnesia_state': ~p", [_Reason]),
                                Acc#{Node => error};
                            {_, {badrpc, _Reason}} ->
                                lager:debug("rpc error on 'get_cluster_mnesia_state': ~p", [_Reason]),
                                Acc#{Node => error};
                            {AppInfo, DbNodes} ->
                                IsActive = proplists:is_defined(mnesia, proplists:get_value(running, AppInfo, [])),
                                Acc#{Node => {IsActive, DbNodes}}
                        end
                end, #{}, [node() | nodes()]).

%%====================================================================
%% Internal functions
%%====================================================================
