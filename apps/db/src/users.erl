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

-export([authorize/2
        ,new/5
        ]).

-spec authorize(binary(), binary()) -> #user{} | 'false'.
authorize(Login, Password)->
    Fun = fun()->
                  mnesia:read('user', Login)
          end,
    case mnesia:transaction(Fun) of
        {'atomic',  [#user{pwd_hash = Password} = User]} -> User;
        _ -> 'false'
    end.

-spec new(binary(), binary(), binary(), non_neg_integer(), non_neg_integer()) -> #user{} | {'aborted', any()}.
new(Login, Pwd, Name, Created, AccessLevel) ->
    PwdHash = crypto:hash('md5', Pwd),
    GetMaxIdFun = fun (X, _Acc) when X > _Acc -> X;
                      (_X, Acc) -> Acc
                  end,
    {'atomic', MaxId} = mnesia:transaction(fun()-> mnesia:foldl(GetMaxIdFun, 0, 'user') end),
    Id = MaxId + 1,
    User = #user{login = Login
                ,id = Id
                ,pwd_hash = PwdHash
                ,name = Name
                ,created = Created
                ,access_level = AccessLevel},
    case mnesia:transaction(fun()-> mnesia:write(User) end) of
        {'atomic', 'ok'} -> User;
        Error -> Error
    end.
