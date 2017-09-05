%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2017
%%%-------------------------------------------------------------------
-module(acme).
-include_lib("common/include/tables.hrl").

-compile({no_auto_import,[get/1
                         ,set/2]}).
-export([new/2
        ,delete/1
        ,get/1]).

-spec new(binary(), binary()) -> 'ok' | 'false'.
new(Key, Value) ->
    Fun = fun()-> mnesia:write(#acme{key = Key, value = Value}) end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> ok;
        _ -> false
    end.

-spec delete(binary()) -> 'ok'.
delete(Key) ->
    Fun = fun()-> mnesia:delete({'acme', Key}) end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> ok;
        _ -> false
    end.

-spec get(binary()) -> binary().
get(Key)->
    Fun = fun()-> mnesia:read('acme', Key) end,
    case mnesia:transaction(Fun) of
        {atomic, [#acme{value = V}]} -> V;
        _ -> false
    end.
