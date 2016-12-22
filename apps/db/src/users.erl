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
        ,new/4
        ,delete/1
        ,get/1
        ,get_by_id/1
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

-spec new(binary(), binary(), binary(), non_neg_integer()) -> #user{} | {'aborted', any()} | 'exists'.
new(Login, Pwd, Name, AccessLevel) ->
    {MSec, Sec, _} = erlang:timestamp(),
    Created = MSec * 1000000 + Sec,
    case get(Login) of
        #user{} ->
            'exists';
        {'aborted', Reason} ->
            {'aborted', Reason};
        _ ->
            PwdHash = common:bin2hex(crypto:hash('md5', Pwd)),
            Id = mnesia:dirty_update_counter('index', 'user', 1),
            User = #user{login = Login
                        ,id = Id
                        ,pwd_hash = PwdHash
                        ,name = Name
                        ,created = Created
                        ,access_level = AccessLevel},
            case mnesia:transaction(fun()-> mnesia:write(User) end) of
                {'atomic', 'ok'} -> User;
                Error -> Error
            end
    end.

-spec delete(binary()) -> 'abort' | 'ok'.
delete(Login) ->
    mnesia:transaction(fun()->
                               mnesia:delete({'user', Login})
                       end).

-spec get(binary()) -> #user{} | 'false'.
get(Login)->
    Fun = fun()-> mnesia:read('user', Login) end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> 'false';
        {atomic, [User]} -> User;
        _ -> 'false'
    end.

-spec get_by_id(binary()) -> #user{} | 'false'.
get_by_id(Id)->
    Fun = fun()-> mnesia:index_read(user, Id, #user.id) end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> 'false';
        {atomic, [User]} -> User;
        _ -> 'false'
    end.
