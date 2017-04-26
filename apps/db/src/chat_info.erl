%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  18 Apr 2017
%%%-------------------------------------------------------------------
-module(chat_info).
-include("db.hrl").
-compile({no_auto_import,[get/1]}).
-export([new/3
        ,get/1
        ,delete/1
        ,rename/2
        ,add_user/2
        ,remove_user/2
        ,extract/2
        ]).

new(ChatId, Name, OwnerId)->
    Fun = fun() -> mnesia:write(#chat_info{chat_id = ChatId
                                          ,name = Name
                                          ,users = [OwnerId]
                                          ,chat_owner = OwnerId})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

get(ChatId) ->
    Fun = fun()-> mnesia:read('chat_info', ChatId) end,
    case mnesia:transaction(Fun) of
        {atomic, [ChatInfo]} -> ChatInfo;
        _Error -> 'false'
    end.

delete(ChatId) ->
    Fun = fun()->
                  mnesia:delete({'chat_info', ChatId})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Result} -> Result;
        _ -> 'false'
    end.

rename(_ChatId, _Name) ->
    ok.

add_user(ChatId, MSISDN) ->
    Fun = fun()->
                  [ChatInfo] = mnesia:read('chat_info', ChatId), %TODO: handle no table
                  Users = ChatInfo#chat_info.users,
                  case lists:member(MSISDN, Users) of
                      'true' ->
                          'exists';
                      _False ->
                          mnesia:write(ChatInfo#chat_info{users = [MSISDN | Users]})
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> _Error
    end.

remove_user(ChatId, MSISDN) ->
    Fun = fun()->
                  [ChatInfo] = mnesia:read('chat_info', ChatId), %TODO: handle no table
                  Users = ChatInfo#chat_info.users,
                  case lists:member(MSISDN, Users) of
                      'false' ->
                          'not_exists';
                      _True ->
                          mnesia:write(ChatInfo#chat_info{users = lists:delete(MSISDN, Users)})
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> _Error
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#chat_info{}, chat_id|name|users|chat_owner) -> binary() | [binary()] | non_neg_integer().
extract(#chat_info{chat_id = C}, 'chat_id')-> C;
extract(#chat_info{name = N}, 'name')-> N;
extract(#chat_info{users = U}, 'users')-> U;
extract(#chat_info{chat_owner = CO}, 'chat_owner')-> CO.
