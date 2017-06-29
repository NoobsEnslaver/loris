%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  27 Jun 2017
%%%-------------------------------------------------------------------
-module(call_offer).
-include_lib("common/include/tables.hrl").
-compile({no_auto_import,[get/1]}).

-export([new/4
        ,delete/1, delete/2
        ,get/1, get/2
        ,extract/2]).

new(MsisdnFrom, MsisdnTo, TurnServer, SdpOffer) ->
    Fun = fun() -> mnesia:write(#call_offer{msisdn_from = MsisdnFrom
                                           ,msisdn_to = MsisdnTo
                                           ,turn_server = TurnServer
                                           ,sdp_offer = SdpOffer
                                           ,timestamp = common:timestamp()})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

delete(MSISDN) ->
    Fun = fun()->
                  mnesia:delete({'call_offer', MSISDN})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

delete(MsisdnFrom, MsisdnTo) ->
    Fun = fun()->
                  Offer = mnesia:match_object(#call_offer{msisdn_from = MsisdnFrom, msisdn_to = MsisdnTo, sdp_offer = '_', timestamp = '_', turn_server = '_'}),
                  mnesia:delete_object(Offer)
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

get(MSISDN) ->
    Fun = fun() ->
                  mnesia:read('call_offer', MSISDN)
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

get(MsisdnFrom, MsisdnTo) ->
    Fun = fun() ->
                  mnesia:match_object(#call_offer{msisdn_from = MsisdnFrom, msisdn_to = MsisdnTo, sdp_offer = '_', timestamp = '_', turn_server = '_'})
          end,
    case mnesia:transaction(Fun) of
        {atomic, [Res]} -> Res;
        _Error -> 'false'
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#call_offer{}, msisdn_from|msisdn_to|sdp_offer|timestamp) -> non_neg_integer() | binary().
extract(#call_offer{msisdn_from = MsisdnFrom}, 'msisdn_from')-> MsisdnFrom;
extract(#call_offer{msisdn_to = MsisdnTo}, 'msisdn_to')-> MsisdnTo;
extract(#call_offer{turn_server = TurnServer}, 'turn_server')-> TurnServer;
extract(#call_offer{sdp_offer = SdpOffer}, 'sdp_offer')-> SdpOffer;
extract(#call_offer{timestamp = TimeStamp}, 'timestamp')-> TimeStamp.
