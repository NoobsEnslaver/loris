%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  25 Jun 2017
%%%-------------------------------------------------------------------
-module(device).
-include_lib("common/include/tables.hrl").
-compile({no_auto_import,[get/1]}).

-export([new/4
        ,delete/1, delete/2
        ,get/1, get/2
        ,extract/2]).

new(MSISDN, DeviceId, Type, PushToken) ->
    Fun = fun()->
                  case mnesia:match_object(#device{msisdn = MSISDN, id = DeviceId, type = '_', push_token = '_'}) of
                      [] ->
                          mnesia:write(#device{msisdn = MSISDN, id = DeviceId, type = Type, push_token = PushToken}),
                          created;
                      [OldDeviceRec] ->
                          mnesia:delete_object(OldDeviceRec),
                          mnesia:write(OldDeviceRec#device{type = Type, push_token = PushToken}),
                          updated
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

delete(MSISDN) ->
    Fun = fun()->
                  mnesia:delete({'device', MSISDN})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

delete(MSISDN, DeviceId) ->
    Fun = fun()->
                  OldDevicesList = mnesia:match_object(#device{msisdn = MSISDN, id = DeviceId, type = '_', push_token = '_'}),
                  lists:foreach(fun(D)->
                                        mnesia:delete_object(D)
                                end, OldDevicesList),
                  {deleted, length(OldDevicesList)}
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

get(MSISDN) ->
    Fun = fun() ->
                  mnesia:read('device', MSISDN)
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

get(MSISDN, DeviceId) ->
    Fun = fun() ->
                  mnesia:match_object(#device{msisdn = MSISDN, id = DeviceId, type = '_', push_token = '_'})
          end,
    case mnesia:transaction(Fun) of
        {atomic, [Res]} -> Res;
        _Error -> 'false'
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#device{}, msisdn|id|type|push_token) -> non_neg_integer() | binary() | atom().
extract(#device{msisdn = MSISDN}, 'msisdn')-> MSISDN;
extract(#device{type = Type}, 'type')-> Type;
extract(#device{id = DeviceId}, 'id')-> DeviceId;
extract(#device{push_token = PushToken}, 'push_token')-> PushToken.
