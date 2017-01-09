%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  12 Dec 2016
%%%-------------------------------------------------------------------
-module(sessions).
-include_lib("common/include/tables.hrl").

-export([get/1
        ,get_by_owner_id/1
        ,get_tokens/0
        ,delete/1
        ,new/3
        ,extract/2
        ]).

-spec get_tokens() -> [binary()].
get_tokens()->
    Fun = fun()->
                  mnesia:all_keys('session')
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Result} -> Result;
        _ -> []
    end.

-spec get(binary()) -> #session{} | 'false'.
get(Token)->
    Fun = fun()->
                  mnesia:read('session', Token)
          end,
    case mnesia:transaction(Fun) of
        {'atomic', [Result]} -> Result;
        _ -> 'false'
    end.

-spec get_by_owner_id(binary()) -> #session{} | 'false'.
get_by_owner_id(OID) ->
    Fun = fun()->
                  mnesia:index_read('session', OID, #session.owner_id)
          end,
    case mnesia:transaction(Fun) of
        {'atomic', [Result]} -> Result;
        _ -> 'false'
    end.

-spec delete(binary()) -> 'ok'.
delete(Token) ->
    Fun = fun()->
                  mnesia:delete({'session', Token})
          end,
    mnesia:transaction(Fun),
    'ok'.

-spec new(non_neg_integer() | #user{}, pid(), non_neg_integer()) -> binary().
new(#user{id=Id}, WSPid, LiveTime) ->
    new(Id, WSPid, LiveTime);
new(Id, WSPid, LiveTime) ->                   %LiveTime in sec
    case get_by_owner_id(Id) of
        #session{token = T} -> delete(T);
        _ -> 'ok'
    end,
    RandBytes = crypto:strong_rand_bytes(32),
    Token = common:bin2hex(RandBytes),
    {MSec, Sec, _} = erlang:timestamp(),
    ExpirationTime = MSec * 1000000 + Sec + LiveTime,
    Session = #session{token = Token
                      ,owner_id = Id
                      ,ws_pid = WSPid
                      ,expiration_time = ExpirationTime},
    Fun = fun()->
                  mnesia:write(Session)
          end,
    mnesia:transaction(Fun),
    Token.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#session{}, token|owner_id|ws_pid|expiration_time) -> binary() | non_neg_integer() | pid().
extract(#session{token = Token}, 'token')-> Token;
extract(#session{owner_id = OwnerId}, 'owner_id')-> OwnerId;
extract(#session{ws_pid = WsPid}, 'ws_pid')-> WsPid;
extract(#session{expiration_time = ExpirationTime}, 'expiration_time')-> ExpirationTime.
