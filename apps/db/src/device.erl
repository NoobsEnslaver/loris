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
        ,delete_devices_by_token/1
        ,get/1, get/2
        ,get_by_type/1
        ,extract/2]).

new(MSISDN, DeviceId, Type, PushToken) ->
    Fun = fun()->
                  Dublicates = mnesia:match_object(#device{msisdn = MSISDN, id = '_', type = '_', push_token = PushToken}),
                  lists:foreach(fun(D)->
                                        mnesia:delete_object(D)
                                end, Dublicates),
                  case mnesia:match_object(#device{msisdn = MSISDN, id = DeviceId, type = Type, push_token = '_'}) of
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

delete_devices_by_token(Tokens) ->
    Fun = fun()->
                  Devices = lists:flatmap(fun(T)->
                                                  mnesia:match_object(#device{msisdn ='_', id = '_', type = '_', push_token = T})
                                          end, Tokens),
                  lists:foreach(fun(D)->
                                        mnesia:delete_object(D)
                                end, Devices),
                  {deleted, length(Devices)}
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

get(MSISDN) ->
    Fun = fun()-> mnesia:dirty_read('device', MSISDN) end,
    mnesia:sync_dirty(Fun).

get(MSISDN, DeviceId) ->
    Fun = fun() ->
                  mnesia:match_object(#device{msisdn = MSISDN, id = DeviceId, type = '_', push_token = '_'})
          end,
    case mnesia:transaction(Fun) of
        {atomic, [Res]} -> Res;
        _Error -> 'false'
    end.

-spec get_by_type([non_neg_integer()]) -> map().
get_by_type(MSISDNs)->
    Fun = fun(MSISDN, #{ios := OldIosDevices, android := OldAndroidDevices, ios_voip := OldIosVoipDevices})->
                  Devices = device:get(MSISDN),
                  #{ios => OldIosDevices ++ [D || D <- Devices, D#device.type == ios]
                   ,android => OldAndroidDevices ++ [D || D <- Devices, D#device.type == android]
                   ,ios_voip => OldIosVoipDevices ++ [D || D <- Devices, D#device.type == ios_voip]}
          end,
    lists:foldl(Fun, #{ios => [], android => [], ios_voip => []}, MSISDNs).

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#device{}, msisdn|id|type|push_token) -> non_neg_integer() | binary() | atom().
extract(#device{msisdn = MSISDN}, 'msisdn')-> MSISDN;
extract(#device{type = Type}, 'type')-> Type;
extract(#device{id = DeviceId}, 'id')-> DeviceId;
extract(#device{push_token = PushToken}, 'push_token')-> PushToken.
