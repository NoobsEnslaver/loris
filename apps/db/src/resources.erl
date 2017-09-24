%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  24 Sep 2017
%%%-------------------------------------------------------------------
-module(resources).
-include_lib("common/include/tables.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([set/3
        ,delete/1
        ,get/1
        ,list/0 ,list/1
        ]).

-spec set(binary(), binary(), any()) -> 'ok' | 'false'.
set(Group, Name, Value) ->
    Fun = fun()->
                  mnesia:write(#resources{name = Name
                                         ,group = Group
                                         ,value = Value})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', 'ok'} -> 'ok';
        _ -> 'false'
    end.

-spec delete(binary()) -> ok.
delete(Name) ->
    Fun = fun()->
                  mnesia:delete({'resources', Name})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', 'ok'} -> 'ok';
        _ -> 'error'
    end.

-spec list()-> [Group :: binary()].
-spec list(binary())-> [Name :: binary()].
list() ->
    Fun = fun()->
                  mnesia:all_keys('resources')
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _ -> []
    end.
list(Group) ->
    Q = qlc:q([Name || #resources{group = G, name = Name} <- mnesia:table('resources'), G == Group]),
    Fun = fun()->
                  qlc:e(Q)
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _ -> 'false'
    end.

-spec get(binary())-> any() | 'false'.
get(Name) ->
    Fun = fun()->
                  mnesia:read('resources', Name)
          end,
    case mnesia:transaction(Fun) of
        {'atomic', [Res]} -> Res;
        _ -> 'false'
    end.
