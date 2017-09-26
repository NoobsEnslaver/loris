%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  27 Dec 2016
%%%-------------------------------------------------------------------
-module(files).
-include_lib("db.hrl").
-compile({no_auto_import,[get/1]}).
-export([extract/2
        ,get/1
        ,save/4
        ,delete/1
        ,get_list/0
        ,get_list_by_owner_id/1
        ]).

-spec save(binary(), binary(), binary(), binary()) -> non_neg_integer().
save(Name, Type, Data, OwnerId)->
    Hash = common:bin2hex(crypto:hash('md5', Data)),
    case application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'keep_files_in_db', 'false') of
        'true' ->
            Id = mnesia:dirty_update_counter('index', 'db_files', 1),
            Arg = #file{id=Id
                       ,hash=Hash
                       ,name=Name
                       ,content_type = Type
                       ,data=Data
                       ,owner_id = OwnerId
                       ,size = byte_size(Data)},
            Fun = fun(X)-> mnesia:write(X) end,
            mnesia:activity(sync_dirty, Fun, [Arg], mnesia_frag),
            Id;
        'false' ->
            Id = mnesia:dirty_update_counter('index', 'dir_files', 1),
            StorageDir = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'files_dir', "/srv/files"),
            case file:write_file(StorageDir ++ "/" ++ integer_to_list(Id), Data, [raw]) of
                ok ->
                    Arg = #file{id=Id
                               ,hash=Hash
                               ,name=Name
                               ,content_type = Type
                               ,data= <<>>
                               ,owner_id = OwnerId
                               ,size = byte_size(Data)},
                    Fun = fun(X)-> mnesia:write(X) end,
                    mnesia:activity(sync_dirty, Fun, [Arg], mnesia_frag),
                    Id;
                _Error ->
                    lager:error("File upload error: ~p", [_Error]),
                    'false'
            end
        end.

-spec delete(binary() | #file{}) -> 'abort' | 'ok'.
delete(#file{id = Id}) ->
    delete(Id);
delete(Id) ->
    case application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'keep_files_in_db', 'false') of
        'true' -> ok;
        'false'->
            StorageDir = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'files_dir', "/srv/files"),
            file:delete(StorageDir ++ "/" ++ integer_to_list(Id))
    end,
    Fun = fun(X)->
                  mnesia:delete(X)
          end,
    Arg = {'file', Id},
    mnesia:activity(sync_dirty, Fun, [Arg], mnesia_frag).

-spec get(binary() | #file{}) -> #file{} | 'false'.
get(#file{id = FileId})->
    get(FileId);
get(FileId)->
    Fun = fun(X)-> mnesia:read('file', X) end,
    case mnesia:activity(sync_dirty, Fun, [FileId], mnesia_frag) of
        [File] when is_record(File, file) ->
            case application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'keep_files_in_db', 'false') of
                'true' -> File;
                'false'->
                    StorageDir = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'files_dir', "/srv/files"),
                    case file:read_file(StorageDir ++ "/" ++ integer_to_list(FileId)) of
                        {error,enoent} ->
                            laget:error("no file data, file_id: ~p", [FileId]),
                            'false';
                        {ok, FData} ->
                            File#file{data = FData}
                    end
            end;
        _ -> 'false'
    end.

-spec get_list() -> [#file{}].
get_list()->
    MatchHead = #file{hash = '$1', content_type = '$2', name = '$3', owner_id = '$4', size = '$5', data='_', id = '$6'},
    Result = ['$1', '$2', '$3', '$4', '$5', '$6'],
    Fun = fun(X) ->
                  mnesia:select('file', X)
          end,
    Arg = [{MatchHead, [], [Result]}],
    case mnesia:activity(sync_dirty, Fun, [Arg], mnesia_frag) of
        Files when is_list(Files) ->
            [#file{hash = H, content_type = CT, name = N, owner_id = OID, size = S, data = <<>>, id = Id} || [H, CT, N, OID, S, Id] <- Files];
        _ -> 'false'
    end.

-spec get_list_by_owner_id(binary()) -> [#file{}].
get_list_by_owner_id(Id)->
    MatchHead = #file{hash = '$1', content_type = '$2', name = '$3', owner_id = '$4', size = '$5', data='_', id = '$6'},
    Guard = {'==', Id, '$4'},
    Result = ['$1', '$2', '$3', '$4', '$5', '$6'],
    Fun = fun(X) ->
                  mnesia:select('file', X)
          end,
    Arg = [{MatchHead, [Guard], [Result]}],
    case mnesia:activity(sync_dirty, Fun, [Arg], mnesia_frag) of
        Files when is_list(Files) ->
            [#file{hash = H, content_type = CT, name = N, owner_id = OID, size = S, data = <<>>, id = FileId} || [H, CT, N, OID, S, FileId] <- Files];
        _ -> 'false'
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#file{}, id|hash|content_type|name|data|owner_id|size) -> binary() | non_neg_integer().
extract(#file{id = Id}, 'id')-> Id;
extract(#file{hash = Hash}, 'hash')-> Hash;
extract(#file{content_type = ContentType}, 'content_type')-> ContentType;
extract(#file{name = Name}, 'name')-> Name;
extract(#file{data = Data}, 'data')-> Data;
extract(#file{owner_id = OwnerId}, 'owner_id')-> OwnerId;
extract(#file{}, 'access_level')-> 0;           %back compatibility, legacy
extract(#file{size = Size}, 'size')-> Size.
