%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  27 Dec 2016
%%%-------------------------------------------------------------------
-module(files).
-include_lib("common/include/tables.hrl").
-compile({no_auto_import,[get/1]}).
-export([extract/2
         ,get/1
        ,save/4
        ,delete/1
        ,get_list/0
        ,get_list_by_owner_id/1
        ]).

-spec save(binary(), binary(), binary(), binary()) -> binary().
save(Name, Type, Data, OwnerId) ->
    Hash = common:bin2hex(crypto:hash('md5', <<Data/binary, Name/binary, OwnerId/integer>>)),
    Fun = fun()->
                  mnesia:write(#file{hash=Hash, name=Name, content_type = Type, data=Data, owner_id = OwnerId, size = byte_size(Data)})
          end,
    mnesia:transaction(Fun),
    Hash.

-spec delete(binary() | #file{}) -> 'abort' | 'ok'.
delete(#file{hash = FileId}) ->
    delete(FileId);
delete(FileId) ->
    mnesia:transaction(fun()->
                               mnesia:delete({'file', FileId})
                       end).

-spec get(binary() | #file{}) -> #file{} | 'false'.
get(#file{hash = FileId})->
    get(FileId);
get(FileId)->
    Fun = fun()-> mnesia:read('file', FileId) end,
    case mnesia:transaction(Fun) of
        {'atomic', [File]} -> File;
        _ -> 'false'
    end.

-spec get_list() -> [#file{}].
get_list()->
    MatchHead = #file{hash = '$1', content_type = '$2', name = '$3', owner_id = '$4', size = '$5', data='_'},
    Result = ['$1', '$2', '$3', '$4', '$5'],
    Fun = fun() ->
                  mnesia:select('file',[{MatchHead, [], [Result]}])
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Files} ->
            [#file{hash = H, content_type = CT, name = N, owner_id = OID, size = S, data = <<>>} || [H, CT, N, OID, S] <- Files];
        _ -> 'false'
    end.

-spec get_list_by_owner_id(binary()) -> [#file{}].
get_list_by_owner_id(Id)->
    MatchHead = #file{hash = '$1', content_type = '$2', name = '$3', owner_id = '$4', size = '$5', data='_'},
    Guard = {'==', Id, '$4'},
    Result = ['$1', '$2', '$3', '$4', '$5'],
    Fun = fun() ->
                  mnesia:select('file',[{MatchHead, [Guard], [Result]}])
          end,
    case mnesia:transaction(Fun) of
        {'atomic', Files} ->
            [#file{hash = H, content_type = CT, name = N, owner_id = OID, size = S, data = <<>>} || [H, CT, N, OID, S] <- Files];
        _ -> 'false'
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#file{}, hash|content_type|name|data|owner_id|size) -> binary() | non_neg_integer().
extract(#file{hash = Hash}, 'hash')-> Hash;
extract(#file{content_type = ContentType}, 'content_type')-> ContentType;
extract(#file{name = Name}, 'name')-> Name;
extract(#file{data = Data}, 'data')-> Data;
extract(#file{owner_id = OwnerId}, 'owner_id')-> OwnerId;
extract(#file{size = Size}, 'size')-> Size.
