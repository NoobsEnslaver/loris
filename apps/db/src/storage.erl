%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  11 Aug 2017
%%%-------------------------------------------------------------------
-module(storage).
-include_lib("common/include/tables.hrl").

-compile({no_auto_import,[get/1
                         ,set/2]}).
-export([set/2
        ,delete/1
        ,get/1]).

-spec set(any(), any()) -> 'ok' | 'false'.
set(Key, Value) ->
    Fun = fun()-> mnesia:write(#storage{key = Key, value = Value}) end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> ok;
        _ -> false
    end.

-spec delete(any()) -> 'ok'.
delete(Key) ->
    Fun = fun()-> mnesia:delete({'storage', Key}) end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> ok;
        _ -> false
    end.

-spec get(any()) -> any().
get(Key)->
    Fun = fun()-> mnesia:read(storage, Key) end,
    case mnesia:transaction(Fun) of
        {atomic, [#storage{value = V}]} -> V;
        _ -> false
    end.
