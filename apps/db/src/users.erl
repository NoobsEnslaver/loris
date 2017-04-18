%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  21 Dec 2016
%%%-------------------------------------------------------------------
-module(users).
-include_lib("common/include/tables.hrl").
-compile({no_auto_import,[get/1]}).
-export([authorize/2
        ,new/8, new/9
        ,delete/1
        ,get/1
        ,extract/2
        ]).

-spec authorize(non_neg_integer(), binary()) -> #user{} | 'false'.
authorize(MSISDN, Password)->
    HPassword = list_to_binary(string:to_upper(binary_to_list(Password))),
    Fun = fun()->
                  mnesia:read('user', MSISDN)
          end,
    case mnesia:transaction(Fun) of
        {'atomic',  [#user{pwd_hash = HPassword} = User]} -> User;
        _ -> 'false'
    end.

-spec new(non_neg_integer(), binary(), binary(), binary(), non_neg_integer(), boolean(), atom(), non_neg_integer()) -> #user{} | {'aborted', any()} | 'exists'.
new(MSISDN, Pwd, FName, LName, Age, IsMale, Group, AccessLevel) ->
    {MSec, Sec, _} = erlang:timestamp(),
    Created = MSec * 1000000 + Sec,
    case get(MSISDN) of
        #user{} ->
            'exists';
        {'aborted', Reason} ->
            {'aborted', Reason};
        _ ->
            PwdHash = common:bin2hex(crypto:hash('md5', Pwd)),
            User = #user{msisdn = MSISDN
                        ,group = Group
                        ,pwd_hash = PwdHash
                        ,fname = FName
                        ,lname = LName
                        ,age = Age
                        ,is_male = IsMale
                        ,created = Created
                        ,access_level = AccessLevel},
            case mnesia:transaction(fun()-> mnesia:write(User) end) of
                {'atomic', 'ok'} -> User;
                Error -> Error
            end
    end.

-spec new(non_neg_integer(), binary(), binary(), binary(), non_neg_integer(), boolean(), atom(), non_neg_integer(), 'nohash') -> #user{} | {'aborted', any()} | 'exists'.
new(MSISDN, PwdHash, FName, LName, Age, IsMale, Group, AccessLevel, 'nohash') ->
    {MSec, Sec, _} = erlang:timestamp(),
    Created = MSec * 1000000 + Sec,
    case get(MSISDN) of
        #user{} ->
            'exists';
        {'aborted', Reason} ->
            {'aborted', Reason};
        _ ->
            User = #user{msisdn = MSISDN
                        ,group = Group
                        ,pwd_hash = PwdHash
                        ,fname = FName
                        ,lname = LName
                        ,age = Age
                        ,is_male = IsMale
                        ,created = Created
                        ,access_level = AccessLevel},
            case mnesia:transaction(fun()-> mnesia:write(User) end) of
                {'atomic', 'ok'} -> User;
                Error -> Error
            end
    end.


-spec delete(#user{} | binary()) -> 'abort' | 'ok' | 'false'.
delete(#user{msisdn = MSISDN}) ->
    delete(MSISDN);
delete(MSISDN) ->
    Fun = fun()->
                  mnesia:delete({'user', MSISDN})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Result} -> Result;
        _ -> 'false'
    end.

-spec get(binary()) -> #user{} | 'false'.
get(MSISDN)->
    Fun = fun()-> mnesia:read('user', MSISDN) end,
    case mnesia:transaction(Fun) of
        {'atomic', [User]} -> User;
        _ -> 'false'
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#user{}, msisdn|group|pwd_hash|created|fname|lname|age|rooms|chats|is_male|access_level) -> binary() | non_neg_integer() | 'infinity' | access_group().
extract(#user{msisdn = MSISDN}, 'msisdn')-> MSISDN;
extract(#user{group = G}, 'group')-> G;
extract(#user{pwd_hash = PwdHash}, 'pwd_hash')-> PwdHash;
extract(#user{created = Created}, 'created')-> Created;
extract(#user{chats = Chats}, 'chats')-> Chats;
extract(#user{rooms = Rooms}, 'rooms')-> Rooms;
extract(#user{age = Age}, 'age')-> Age;
extract(#user{fname = FName}, 'fname')-> FName;
extract(#user{lname = LName}, 'lname')-> LName;
extract(#user{is_male = IsMale}, 'is_male')-> IsMale;
extract(#user{access_level = AccessLevel}, 'access_level')-> AccessLevel.
