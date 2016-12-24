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
        ,new/3
        ]).

-spec get(binary()) -> #session{} | 'false'.
get(Token)->
    Fun = fun()->
                  mnesia:read('session', Token)
          end,
    case mnesia:transaction(Fun) of
        {'atomic', [Result]} -> Result;
        _ -> 'false'
    end.

new(User, WSPid, LiveTime) ->                   %LiveTime in sec
    RandBytes = crypto:strong_rand_bytes(32),
    Token = common:bin2hex(RandBytes),
    {MSec, Sec, _} = erlang:timestamp(),
    ExpirationTime = MSec * 1000000 + Sec + LiveTime,
    Session = #session{token = Token
                      ,user = User
                      ,ws_pid = WSPid
                      ,expiration_time = ExpirationTime},
    io:format("Token: ~s~n", [Token]),
    Fun = fun()->
                  mnesia:write(Session)
          end,
    mnesia:transaction(Fun),
    Token.
