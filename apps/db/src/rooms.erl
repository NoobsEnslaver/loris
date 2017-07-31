%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  30 Jul 2017
%%%-------------------------------------------------------------------
-module(rooms).
-include_lib("common/include/tables.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).
-export(new/8, new/9
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


new(OwnerId, Name, Description, AccessMap) ->
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
                        ,access_level = AccessLevel},
            case mnesia:transaction(fun()-> mnesia:write(User) end) of
                {'atomic', 'ok'} -> User;
                _Error -> 'false'
            end
    end.

-spec delete(#room{} | non_neg_integer()) -> 'ok' | 'false'.
delete(#room{owner_id = OwnerId}) ->
    delete(OwnerId);
delete(OwnerId) ->
    Fun = fun()->
                  mnesia:delete({'room', OwnerId})
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Result} -> Result;
        _ -> 'false'
    end.

-spec get(non_neg_integer()) -> #room{} | 'false'.
get(OwnerId)->
    Fun = fun()-> mnesia:read('room', OwnerId) end,
    case mnesia:transaction(Fun) of
        {'atomic', [Room]} -> Room;
        _ -> 'false'
    end.

join_to_chat(RoomId, MSISDN, AccessGroup) ->
    Fun = fun()->
                  [User] = mnesia:read('user', MSISDN),
                  CI = User#user.chats_invatations,
                  case proplists:get_value(ChatId, CI) of
                      'undefined' ->
                          mnesia:write(User#user{chats_invatations = [{ChatId, AccessGroup} | CI]});
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
                  case proplists:get_value(ChatId, CI) of
                      'undefined' ->
                          'not_exists';
                      AccessGroup ->
                          mnesia:write(User#user{chats_invatations = proplists:delete(ChatId, CI)
                                                ,chats = [{ChatId, AccessGroup} | Chats]}),
                          AccessGroup
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
                  case proplists:get_value(ChatId, CI) of
                      'undefined' ->
                          'not_exists';
                      _AccessGroup ->
                          mnesia:write(User#user{chats_invatations = proplists:delete(ChatId, CI)})
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
                  mnesia:write(User#user{chats = proplists:delete(ChatId, Chats)})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> _Error
    end.

%% TODO: optimize it
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
                  Chats = User#user.chats,
                  case proplists:get_value(ChatId, Chats) of
                      'undefined' ->
                          'not_exists';
                      _AccessGroup ->
                          case lists:member(ChatId, MC) of
                              'true' ->
                                  'ok';
                              'false'->
                                  mnesia:write(User#user{muted_chats = [ChatId | MC]})
                          end
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
                          mnesia:write(User#user{muted_chats = proplists:delete(ChatId, MC)});
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
                                            rooms -> User#user{rooms = Value};
                                            chats_invatations -> User#user{chats_invatations = Value};
                                            age -> User#user{age = Value};
                                            fname -> User#user{fname = Value};
                                            lname -> User#user{lname = Value};
                                            is_male -> User#user{is_male = Value};
                                            muted_chats -> User#user{muted_chats = Value};
                                            last_visit_timestamp -> User#user{last_visit_timestamp = Value};
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
-spec extract(#user{}, msisdn|group|pwd_hash|created|fname|lname|age|rooms|chats|chats_invatations|is_male|access_level|muted_chats) -> binary() | non_neg_integer() | 'infinity' | access_group().
extract(#user{msisdn = MSISDN}, 'msisdn')-> MSISDN;
extract(#user{group = G}, 'group')-> G;
extract(#user{pwd_hash = PwdHash}, 'pwd_hash')-> PwdHash;
extract(#user{created = Created}, 'created')-> Created;
extract(#user{chats = Chats}, 'chats')-> Chats;
extract(#user{rooms = Rooms}, 'rooms')-> Rooms;
extract(#user{chats_invatations = CI}, 'chats_invatations')-> CI;
extract(#user{age = Age}, 'age')-> Age;
extract(#user{fname = FName}, 'fname')-> FName;
extract(#user{lname = LName}, 'lname')-> LName;
extract(#user{is_male = IsMale}, 'is_male')-> IsMale;
extract(#user{muted_chats = MC}, 'muted_chats')-> MC;
extract(#user{last_visit_timestamp = LVTS}, 'last_visit_timestamp')-> LVTS;
extract(#user{access_level = AccessLevel}, 'access_level')-> AccessLevel.
