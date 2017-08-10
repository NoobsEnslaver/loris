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
-compile({no_auto_import,[get/1
                         ,set/1]}).
-export([new/6
        ,delete/1
        ,get/1
        ,get_tag/1
        ,get_by_owner_id/1
        ,set/1
        ,set_tag/1
        ,get_chat/2
        ,search_by_name/1
        ,search_by_tags/1
        ,send_msg_to_room/4
        ]).

-define(MAY_ADMIN(AL), AL div 4 == 1).
-define(MAY_WRITE(AL), (AL rem 4) div 2 == 1).
-define(MAY_READ(AL), ((AL rem 4) rem 2) == 1).

-spec new(non_neg_integer(), binary(), binary(), map(), map(), #room_tag{}) -> #room{} | 'false'.
new(OwnerId, Name, Description, RoomAccessMap, ChatAccessMap, Tags) ->
    Id = mnesia:dirty_update_counter('index', 'room', 1),
    {ok, ChatId} = chats:new(),
    chat_info:new(ChatId, Name, OwnerId),
    Room = #room{id = Id
                ,name = Name
                ,description = Description
                ,subrooms = []
                ,owner_id = OwnerId
                ,room_access = RoomAccessMap
                ,chat_access = ChatAccessMap
                ,chat_id = ChatId},
    Fun = fun()->
                  mnesia:write(Room),
                  mnesia:write(Tags#room_tag{room_id = Id, name = Name}),
                  User = users:get(OwnerId),
                  RoomList = User#user.rooms,
                  users:set_info(OwnerId, [{rooms, [{Id, 7} | RoomList]}]),
                  Id
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Result} -> Result;
        _Error -> 'false'
    end.

-spec delete(#room{} | non_neg_integer()) -> 'ok' | 'false'.
delete(#room{id = Id}) ->
    delete(Id);
delete(Id) ->
    Fun = fun()->
                  #room{chat_id = ChatId
                       ,owner_id = OwnerId} = get(Id),
                  mnesia:delete({'room', Id}),
                  mnesia:delete({'room_tag', Id}),
                  {ChatId, OwnerId}
          end,
    case mnesia:transaction(Fun) of
        {'atomic', {ChatId, OwnerId}} ->
            chats:delete(ChatId, OwnerId),
            ok;
        _ -> 'false'
    end.

-spec get(non_neg_integer()) -> #room{} | 'false'.
get(Id)->
    Fun = fun()-> mnesia:dirty_read('room', Id) end,
    case mnesia:sync_dirty(Fun) of
        [Res] -> Res;
        _ -> 'false'
    end.

-spec get_tag(non_neg_integer()) -> #room_tag{} | 'false'.
get_tag(Id)->
    Fun = fun()-> mnesia:dirty_read('room_tag', Id) end,
    case mnesia:sync_dirty(Fun) of
        [Res] -> Res;
        _ -> 'false'
    end.

-spec set(#room{}) -> 'ok' | 'false'.
set(Room) when is_record(Room, room)->
    Fun = fun()-> mnesia:write(Room) end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.

-spec set_tag(#room_tag{}) -> 'ok' | 'false'.
set_tag(Tag) when is_record(Tag, room_tag)->
    Fun = fun()-> mnesia:write(Tag) end,
    case mnesia:transaction(Fun) of
        {'atomic', Res} -> Res;
        _Error -> 'false'
    end.

-spec get_by_owner_id(non_neg_integer()) -> [#room{}] | 'false'.
get_by_owner_id(OwnerId) ->
    Fun = fun()-> mnesia:dirty_index_read('room', OwnerId, #room.owner_id) end,
    mnesia:sync_dirty(Fun).

-spec get_chat(non_neg_integer(), non_neg_integer()) -> {binary(), 0..7} | 'not_exists' | 'forbidden'.
get_chat(RoomId, MSISDN) ->
    Fun = fun()->
                  case mnesia:dirty_read('room', RoomId) of
                      [#room{'chat_id' = 'undefined'}] ->
                          'not_exists';
                      [#room{'chat_access'  = #{MSISDN := 0}}] ->
                          'forbidden';
                      [#room{'chat_id' = ChatId
                            ,'chat_access'  = #{MSISDN := AccessLevel}}] ->
                          {ChatId, AccessLevel};
                      [#room{'chat_access'  = #{'default' := 0}}] ->
                          'forbidden';
                      [#room{'chat_id' = ChatId
                            ,'chat_access'  = #{'default' := AccessLevel}}] ->
                          {ChatId, AccessLevel};
                      _ ->
                          'not_exists'
                  end
          end,
    mnesia:sync_dirty(Fun).

-spec send_msg_to_room(non_neg_integer(), binary(), non_neg_integer(), boolean()) -> 'ok' | 'false'.
send_msg_to_room(RoomId, Msg, From, IsRecursive)->
    Fun = fun()->
                  case mnesia:dirty_read('room', RoomId) of
                      [#room{chat_id = 'undefined'}] when IsRecursive == 'false' ->
                          'false';
                      [#room{chat_id = ChatId}] when IsRecursive == 'false' ->
                          chats:send_message(ChatId, Msg, From), ok;
                      [#room{}] when IsRecursive == 'true' ->
                          RoomChatMap = get_subrooms_chats(RoomId),
                          AllowedChats = maps:fold(fun(R, C, Acc)->
                                                           case rooms:get(R) of
                                                               #room{owner_id = From} -> [C | Acc];
                                                               #room{chat_access = #{From := CA}} when ?MAY_WRITE(CA) -> [C | Acc];
                                                               #room{chat_access = #{From := _}} -> Acc;
                                                               #room{chat_access = #{'default' := CA}} when ?MAY_WRITE(CA) -> [C | Acc];
                                                               _ -> Acc
                                                           end
                                                   end, [], RoomChatMap),
                          [chats:send_message(ChatId, Msg, From) || ChatId <- AllowedChats],
                          ok;
                      _ ->
                          'false'
                  end
          end,
    mnesia:sync_dirty(Fun).

%% TODO: optimize it
-spec search_by_name(binary()) -> map().
search_by_name(Name) when  byte_size(Name) > 2 ->
    Q = qlc:q([{R#room.id, R#room.name} || R <- mnesia:table('room'), binary:match(R#room.name, Name) /= 'nomatch']),
    Fun = common:get_limited_amount_from_query(Q, 20),
    maps:from_list(mnesia:sync_dirty(Fun));
search_by_name(_) -> [].

-spec search_by_tags(#room_tag{}) -> map().
search_by_tags(Tag) ->
    Fun = fun()-> mnesia:dirty_match_object(Tag) end,
    Match = mnesia:sync_dirty(Fun),
    maps:from_list([{Id, Name} || #room_tag{room_id = Id, name = Name} <- Match]).


%% --------------------------------------
%% Internal functions
%% --------------------------------------
get_subrooms_chats(RoomId) ->
    get_subrooms_chats(RoomId, #{}).
get_subrooms_chats(RoomId, RoomChatMap) ->
    case mnesia:dirty_read('room', RoomId) of
        [#room{chat_id = ChatId
              ,subrooms = SubRooms}] when is_binary(ChatId) ->
            lists:foldl(fun(R, Map)->
                                case maps:is_key(R, Map) of
                                    'true' -> Map;
                                    'false'-> get_subrooms_chats(R, Map)
                                end
                        end, RoomChatMap#{RoomId => ChatId}, SubRooms);
        _ ->
            RoomChatMap
    end.
