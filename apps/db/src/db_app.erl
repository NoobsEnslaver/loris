%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2016
%%%-------------------------------------------------------------------

-module(db_app).

-behaviour(application).
-include("db.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    connect_to_nodes(),
    case nodes() of
        [] ->                                   %first node in cluster
            case mnesia:create_schema([node()]) of
                {'error', {_Node, {'already_exists', _Node}}} ->
                    lager:info("skip creating schema, already exists"),
                    mnesia:start();
                {'error', Reason} ->
                    lager:info("can't create schema: ~p", [Reason]),
                    exit(Reason);
                _Ok ->
                    lager:info("schema successfully created"),
                    mnesia:start(),
                    create_tables(?DEFAULT_SCHEMA)
            end;
        _Nodes ->
            mnesia:start(),
            case mnesia:system_info('db_nodes') of
                OnlyMe when OnlyMe == [node()] ->                     %we are not on mnesia cluster
                    mnesia:change_config(extra_db_nodes, nodes()),
                    mnesia:change_table_copy_type(schema, node(), disc_copies),
                    TablesAndTypes = [{T, element(2, hd(mnesia:table_info(T, where_to_commit)))} || T <- mnesia:system_info(tables)],
                    [mnesia:add_table_copy(Table, node(), Type) || {Table, Type} <- TablesAndTypes];    %TODO: not all replicas needed
                _ ->                                                                                    %TODO: configure fragmented tables
                    ok                          %cluster exists, we are last leave node
            end
        end,
    db_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    'ok'.

%%====================================================================
%% Internal functions
%%====================================================================
create_tables(Tables)->
    lists:foreach(fun({Name, Opts}) ->
                          case mnesia:create_table(Name, Opts) of
                              {'atomic', 'ok'} ->
                                  lager:info("Table '~p' successfuly created with options: ~p", [Name, Opts]);
                              {'aborted', {'already_exists', Name1}} ->
                                  lager:info("Table ~p already exists", [Name1]);
                              {'error', Reason} ->
                                  lager:info("error on create table ~p, reason: ~p~n", [Name, Reason]) %TODO
                          end
                  end, Tables).

connect_to_nodes() ->
    Nodes = application:get_env('db', 'nodes', []),
    [net_kernel:connect_node(Node) || Node <- Nodes],
    ok.
