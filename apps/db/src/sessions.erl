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
        ,new/4
        ]).

-spec get(binary()) -> #session{}.
get(Token)->
    Fun = fun()->
                  mnesia:read('session', Token)
          end,
    {'atomic', [Result]} = mnesia:transaction(Fun),
    Result.

new(AccessLevel, WSPid, OwnerId, LiveTime) ->
    RandBytes = crypto:strong_rand_bytes(32),
    Token = common:bin2hex(RandBytes),
    {MSec, Sec, _} = erlang:timestamp(),
    ExpirationTime = MSec * 1000000 + Sec + LiveTime,
    Session = #session{token = Token
                      ,access_level = AccessLevel
                      ,ws_pid = WSPid
                      ,owner_id = OwnerId
                      ,expiration_time = ExpirationTime},
    io:format("Token: ~s~n", [Token]),
    Fun = fun()->
                  mnesia:write(Session)
          end,
    mnesia:transaction(Fun),
    Token.
