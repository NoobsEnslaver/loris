%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created :  16 Sep 2017
%%%-------------------------------------------------------------------

-module(db).

-include("db.hrl").

-export([change_files_storage_type/1
        ,change_node_name/4]).

%%====================================================================
%% API
%%====================================================================
-spec change_files_storage_type('db'|'file') -> {'ok', Info :: map()}|{'error', Reason :: string()}|'already'.
change_files_storage_type('db') ->
    case application:get_env('db', 'keep_files_in_db', 'false') of
        'true' -> 'already';
        'false'->
            ok;
        _BadType -> {'error', io_lib:format("Bad current db:keep_files_in_db storage type ~p",[_BadType])}
    end;
change_files_storage_type('file') ->
    case application:get_env('db', 'keep_files_in_db', 'false') of
        'false'-> 'already';
        'true' ->
            ok;
        _BadType -> {'error', io_lib:format("Bad current db:keep_files_in_db storage type ~p",[_BadType])}
    end.

change_node_name(From, To, Source, Target) ->
    Switch = fun (Node) when Node == From ->
                     io:format("     - Replacing nodename: '~p' with: '~p'~n", [From, To]),
                     To;
                 (Node) when Node == To -> throw({error, already_exists});
                 (Node) ->
                     io:format("     - Node: '~p' will not be modified (it is not '~p')~n", [Node, From]),
                     Node
             end,
    Convert = fun ({schema, db_nodes, Nodes}, Acc) ->
                      io:format(" +++ db_nodes ~p~n", [Nodes]),
                      {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
                  ({schema, version, Version}, Acc) ->
                      io:format(" +++ version: ~p~n", [Version]),
                      {[{schema, version, Version}], Acc};
                  ({schema, cookie, Cookie}, Acc) ->
                      io:format(" +++ cookie: ~p~n", [Cookie]),
                      {[{schema, cookie, Cookie}], Acc};
                  ({schema, Tab, CreateList}, Acc) ->
                      io:format("~n * Checking table: '~p'~n", [Tab]),
                      Keys = [ram_copies, disc_copies, disc_only_copies],
                      OptSwitch =
                          fun({Key, Val}) ->
                                  case lists:member(Key, Keys) of
                                      true ->
                                          io:format("   + Checking key: '~p'~n", [Key]),
                                          {Key, lists:map(Switch, Val)};
                                      false-> {Key, Val}
                                  end
                          end,
                      Res = {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc},
                      Res;
                  (Other, Acc) ->
                      {[Other], Acc}
              end,
    mnesia:traverse_backup(Source, Target, Convert, switched).

%%====================================================================
%% Internal functions
%%====================================================================
