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
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).
-export([authorize/2
        ,new/9, new/10
        ,delete/1
        ,get/1
        ,extract/2
        ,invite_to_chat/3
        ,accept_invatation/2
        ,reject_invatatoin/2
        ,leave_chat/2
        ,search/2
        ,mute_chat/2
        ,unmute_chat/2
        ,update_last_visit_timestamp/1
        ,set_info/2
        ,subscribe/2
        ,unsubscribe/1, unsubscribe/2
        ,notify/2
        ,set_pid/2
        ,get_pid/1
        ,delete_pid/1
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

-spec new(non_neg_integer(), binary(), binary(), binary(), non_neg_integer(), boolean(), atom(), binary(), non_neg_integer()) -> #user{} | 'false' | 'exists'.
new(MSISDN, Pwd, FName, LName, Age, IsMale, Group, City, AccessLevel) ->
    Created = common:timestamp(),
    case get(MSISDN) of
        #user{} ->
            'exists';
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
                        ,city = City
                        ,access_level = AccessLevel},
            case mnesia:transaction(fun()-> mnesia:write(User) end) of
                {'atomic', 'ok'} -> User;
                _Error -> 'false'
            end
    end.

-spec new(non_neg_integer(), binary(), binary(), binary(), non_neg_integer(), boolean(), atom(), binary(), non_neg_integer(), 'nohash') -> #user{} | 'false' | 'exists'.
new(MSISDN, PwdHash, FName, LName, Age, IsMale, City, Group, AccessLevel, 'nohash') ->
    Created = common:timestamp(),
    case get(MSISDN) of
        #user{} ->
            'exists';
        _ ->
            User = #user{msisdn = MSISDN
                        ,group = Group
                        ,pwd_hash = list_to_binary(string:to_upper(binary_to_list(PwdHash)))
                        ,fname = FName
                        ,lname = LName
                        ,age = Age
                        ,is_male = IsMale
                        ,created = Created
                        ,city = City
                        ,access_level = AccessLevel},
            case mnesia:transaction(fun()-> mnesia:write(User) end) of
                {'atomic', 'ok'} -> User;
                _Error -> 'false'
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

-spec get(non_neg_integer()) -> #user{} | 'false'.
get(MSISDN)->
    Fun = fun()-> mnesia:read('user', MSISDN) end,
    case mnesia:transaction(Fun) of
        {'atomic', [User]} -> User;
        _ -> 'false'
    end.

invite_to_chat(ChatId, MSISDN, AL) ->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  CI = User#user.chats_invatations,
                  case maps:is_key(ChatId, CI) of
                      'false' ->
                          mnesia:write(User#user{chats_invatations = CI#{ChatId => AL}});
                      _ ->
                          'exists'
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> _Error
    end.

accept_invatation(ChatId, MSISDN) ->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  CI = User#user.chats_invatations,
                  Chats = User#user.chats,
                  case maps:take(ChatId, CI) of
                      'error' ->
                          'not_exists';
                      {AL, NewCI} ->
                          mnesia:write(User#user{chats_invatations = NewCI
                                                ,chats = Chats#{ChatId => AL}}),
                          AL
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> _Error
    end.

reject_invatatoin(ChatId, MSISDN) ->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  CI = User#user.chats_invatations,
                  case maps:take(ChatId, CI) of
                      'error' ->
                          'not_exists';
                      {_AccessGroup, NewCI} ->
                          mnesia:write(User#user{chats_invatations = NewCI})
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> _Error
    end.

leave_chat(ChatId, MSISDN)->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  Chats = User#user.chats,
                  MC = User#user.muted_chats,
                  mnesia:write(User#user{chats = maps:remove(ChatId, Chats), muted_chats = MC -- [ChatId]})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> _Error
    end.

%% TODO: optimize it
%% TODO: search by other fields
search(FName, LName) when byte_size(FName) > 2 andalso byte_size(LName) > 2 ->
    Q = qlc:q([U#user.msisdn || U <- mnesia:table('user'), binary:match(U#user.fname, FName) /= 'nomatch'
                                                         , binary:match(U#user.lname, LName) /= 'nomatch']),
    Fun = common:get_limited_amount_from_query(Q, 20),
    mnesia:sync_dirty(Fun);
search(_, LName) when byte_size(LName) > 2 ->
    Q = qlc:q([U#user.msisdn || U <- mnesia:table('user'), binary:match(U#user.lname, LName) /= 'nomatch']),
    Fun = common:get_limited_amount_from_query(Q, 20),
    mnesia:sync_dirty(Fun);
search(FName, _) when byte_size(FName) > 2 ->
    Q = qlc:q([U#user.msisdn || U <- mnesia:table('user'), binary:match(U#user.fname, FName) /= 'nomatch']),
    Fun = common:get_limited_amount_from_query(Q, 20),
    mnesia:sync_dirty(Fun);
search(_, _) -> [].


mute_chat(MSISDN, ChatId) ->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  MC = User#user.muted_chats,
                  case lists:member(ChatId, MC) of
                      'true' ->
                          'ok';
                      'false'->
                          mnesia:write(User#user{muted_chats = [ChatId | MC]})
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

unmute_chat(MSISDN, ChatId) ->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  MC = User#user.muted_chats,
                  case lists:member(ChatId, MC) of
                      'true' ->
                          mnesia:write(User#user{muted_chats = MC -- [ChatId]});
                      'false'->
                          'ok'
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

update_last_visit_timestamp(MSISDN) ->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  mnesia:write(User#user{last_visit_timestamp = common:timestamp()})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

set_info(MSISDN, Proplist) ->
    case get(MSISDN) of
        User0 when is_record(User0, user) ->
            User1 = lists:foldl(fun({Key, Value}, User)->
                                        case Key of
                                            msisdn -> User#user{msisdn = Value};
                                            group -> User#user{group = Value};
                                            pwd_hash -> User#user{pwd_hash = list_to_binary(string:to_upper(binary_to_list(Value)))};
                                            created -> User#user{created = Value};
                                            chats -> User#user{chats = Value};
                                            chats_invatations -> User#user{chats_invatations = Value};
                                            age -> User#user{age = Value};
                                            rooms -> User#user{rooms = Value};
                                            fname -> User#user{fname = Value};
                                            lname -> User#user{lname = Value};
                                            is_male -> User#user{is_male = Value};
                                            muted_chats -> User#user{muted_chats = Value};
                                            last_visit_timestamp -> User#user{last_visit_timestamp = Value};
                                            city -> User#user{city = Value};
                                            access_level -> User#user{access_level = Value}
                                        end
                                end, User0, Proplist),
            case mnesia:transaction(fun()-> mnesia:write(User1) end) of
                {'atomic', 'ok'} -> User1;
                _Error -> 'false'
            end;
        _ ->
            'not_exists'
    end.

subscribe(MSISDN, SubscriberMSISDN) ->
    Fun = fun()->
                  mnesia:write(#user_subscribe{msisdn = MSISDN, subscriber = SubscriberMSISDN})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.

unsubscribe(MSISDN, SubscriberMSISDN) ->
    Fun = fun()->
                  mnesia:delete_object(#user_subscribe{msisdn = MSISDN, subscriber = SubscriberMSISDN})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.

unsubscribe(SubscriberMSISDN) ->
    Fun = fun()->
                  Subscribers = mnesia:match_object(#user_subscribe{msisdn = '_', subscriber = SubscriberMSISDN}),
                  [mnesia:delete_object(S) || S <- Subscribers],
                  ok
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.

notify(MSISDN, Status) ->
    Fun = fun()->
                  Subscribers = mnesia:read(user_subscribe, MSISDN),
                  Pids = [get_pid(S) || #user_subscribe{subscriber = S} <- Subscribers],
                  [P ! {notify, MSISDN, Status, self()} || P <- Pids, is_pid(P)],
                  ok
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.

set_pid(MSISDN, Pid) ->
    Fun = fun()-> mnesia:write(#pids{msisdn = MSISDN, pid = Pid}) end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.

get_pid(MSISDN) ->
    Fun = fun()-> mnesia:dirty_read('pids', MSISDN) end,
    case mnesia:async_dirty(Fun) of
        [#pids{pid = P}] -> P;
        _ -> 'false'
    end.

delete_pid(MSISDN) ->
    Fun = fun()-> mnesia:delete({'pids', MSISDN}) end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.


%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#user{}, msisdn|group|pwd_hash|created|fname|lname|rooms|age|chats|chats_invatations|is_male|access_level|muted_chats) -> binary() | non_neg_integer() | 'infinity' | access_group().
extract(#user{msisdn = MSISDN}, 'msisdn')-> MSISDN;
extract(#user{group = G}, 'group')-> G;
extract(#user{pwd_hash = PwdHash}, 'pwd_hash')-> PwdHash;
extract(#user{created = Created}, 'created')-> Created;
extract(#user{chats = Chats}, 'chats')-> Chats;
extract(#user{chats_invatations = CI}, 'chats_invatations')-> CI;
extract(#user{age = Age}, 'age')-> Age;
extract(#user{fname = FName}, 'fname')-> FName;
extract(#user{lname = LName}, 'lname')-> LName;
extract(#user{rooms = Rooms}, 'rooms')-> Rooms;
extract(#user{is_male = IsMale}, 'is_male')-> IsMale;
extract(#user{muted_chats = MC}, 'muted_chats')-> MC;
extract(#user{city = C}, 'city')-> C;
extract(#user{last_visit_timestamp = LVTS}, 'last_visit_timestamp')-> LVTS;
extract(#user{access_level = AccessLevel}, 'access_level')-> AccessLevel.
