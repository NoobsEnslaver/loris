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
        ,bind_pid_to_session/2
        ,get_ws_pid/1
        ,is_online/1
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

%% TODO: close WS too
-spec delete(binary()) -> 'ok'.
delete(Token) ->
    Fun = fun()->
                  mnesia:delete({'session', Token})
          end,
    mnesia:transaction(Fun),
    'ok'.

-spec new(non_neg_integer() | #user{}, pid(), non_neg_integer()) -> binary().
new(#user{msisdn = MSISDN}, WSPid, LiveTime) ->
    new(MSISDN, WSPid, LiveTime);
new(MSISDN, WSPid, LiveTime) ->                   %LiveTime in sec
    case get_by_owner_id(MSISDN) of
        #session{token = T} ->
            T;
        _ ->
            RandBytes = crypto:strong_rand_bytes(16),
            Token = common:bin2hex(RandBytes),
            ExpirationTime = common:timestamp() + LiveTime,
            User = users:get(MSISDN),
            AccessLevel = users:extract(User, 'access_level'),
            Group = users:extract(User, 'group'),
            Session = #session{token = Token
                              ,owner_id = MSISDN
                              ,group = Group
                              ,ws_pid = WSPid
                              ,access_level = AccessLevel
                              ,expiration_time = ExpirationTime},
            Fun = fun()->
                          mnesia:write(Session)
                  end,
            mnesia:transaction(Fun),
            Token
    end.

bind_pid_to_session(SessionId, Pid) ->
    Session = sessions:get(SessionId),
    mnesia:transaction(fun()->
                            mnesia:write(Session#session{ws_pid = Pid})
                       end).

is_online(MSISDN) ->
    case get_by_owner_id(MSISDN) of
        #session{ws_pid = P} -> is_pid(P);
        _ -> false
    end.

get_ws_pid(MSISDN) ->
    case get_by_owner_id(MSISDN) of
        #session{ws_pid = P} when is_pid(P) -> P;
        _ -> false
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#session{}, access_level|token|owner_id|ws_pid|expiration_time) -> binary() | non_neg_integer() | pid().
extract(#session{token = Token}, 'token')-> Token;
extract(#session{owner_id = OwnerId}, 'owner_id')-> OwnerId;
extract(#session{group = G}, 'group')-> G;
extract(#session{ws_pid = WsPid}, 'ws_pid')-> WsPid;
extract(#session{access_level = AL}, 'access_level')-> AL;
extract(#session{expiration_time = ExpirationTime}, 'expiration_time')-> ExpirationTime.
