%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  12 Nov 2017
%%%-------------------------------------------------------------------
-module(tournaments).
-include_lib("common/include/tables.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([set/1
        ,new/5
        ,delete/1
        ,get/1
        ,list/0
        ,detailed_list/0
        ]).

-spec new(binary(), binary(), [non_neg_integer()], non_neg_integer(), map()) -> 'ok' | 'false'.
new(Name, City, Judges, Timestamp, Participants)->
    Id = mnesia:dirty_update_counter('index', 'db_files', 1),
    set(#tournament{id = Id
                   ,city = City
                   ,name = Name
                   ,judges = Judges
                   ,timestamp = Timestamp
                   ,participants = Participants}).

-spec set(#tournament{}) -> 'ok' | 'false'.
set(TournamentRec) ->
    Fun = fun()-> mnesia:write(TournamentRec) end,
    case mnesia:transaction(Fun) of
        {'atomic', 'ok'} -> 'ok';
        _ -> 'false'
    end.

-spec delete(binary()) -> ok.
delete(Name) ->
    Fun = fun()->
                  mnesia:delete({'tournament', Name})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', 'ok'} -> 'ok';
        _ -> 'error'
    end.

-spec list()-> [non_neg_integer()].
list() ->
    Fun = fun()->
                  mnesia:all_keys('tournament')
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _ -> []
    end.

-spec detailed_list()-> map().
detailed_list() ->
    Fun = fun()->
                  mnesia:foldl(fun(#tournament{id = Id, name = Name, city = City, timestamp = Timestamp}, Acc)->
                                       Acc#{Id => #{name => Name, city => City, timestamp => Timestamp}}
                               end, #{}, 'tournament')
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _ -> #{}
    end.

-spec get(binary())-> #tournament{} | 'false'.
get(Id) ->
    Fun = fun()->
                  mnesia:read('tournament', Id)
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _ -> 'false'
    end.
