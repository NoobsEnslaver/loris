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
-export([start/2
        ,stop/1
        ,connect_to_nodes/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    timer:sleep(1000),
    case nodes() of
        [] ->                                   %first node in cluster
            lager:info("no nodes on cluster, trying to create schema"),
            case create_schema([node()]) of
                'already_exists' ->
                    mnesia:start();
                ok ->
                    mnesia:start(),
                    create_tables(?DEFAULT_SCHEMA)
            end;
        _Nodes ->
            case mnesia:system_info(db_nodes) of
                [Me] when Me == node() ->
                    lager:info("have no my own cluster, but cluster contains nodes - discovering biggest island"),
                    ClusterState = common:get_cluster_mnesia_state(),
                    {Node, ClusterSize} = maps:fold(fun(_Node, {false, _}, Acc) -> Acc;
                                                       (_Node, error, Acc) -> Acc;
                                                       (Node1, {true, Nodes}, {Node2, Weight}) ->
                                                            if length(Nodes) > Weight -> {Node1, length(Nodes)};
                                                               true -> {Node2, Weight}
                                                            end
                                                    end, {node(), 1}, ClusterState),
                    case {Node, ClusterSize} of
                        {_, ClusterSize} when ClusterSize < 2 ->                       %there is no cluster
                            lager:info("bigest mnesia island on cluster size is ~w - no cluster, creating", [ClusterSize]),
                            MasterNode = common:select_oldest_node(),
                            lager:info("node ~p selected as master", [MasterNode]),
                            case MasterNode of
                                Me ->                   %init cluster
                                    lager:info("master it's me - creating schema and tables"),
                                    rpc:multicall(mnesia, stop, [[node() | nodes()]]),
                                    mnesia:delete_schema([node() | nodes()]),
                                    create_schema([node() | nodes()]),
                                    rpc:multicall(mnesia, start, []),
                                    timer:sleep(500),
                                    create_tables(?DEFAULT_SCHEMA),
                                    lager:info("schema and tables successfuly created");
                                _ ->
                                    lager:info("i'm slave, waiting for initialization"),
                                    timer:sleep(3000),
                                    mnesia:wait_for_tables(mnesia:system_info(tables), 60000)
                            end;
                        {Node, _} ->
                            lager:info("a non-degenerate island was found on ~p, joining", [Node]),
                            mnesia:start(),
                            ClusterNodes = rpc:call(Node, mnesia, system_info, [db_nodes]),
                            mnesia:change_config(extra_db_nodes, ClusterNodes),
                            timer:sleep(1000),
                            mnesia:change_table_copy_type(schema, node(), disc_copies),
                            timer:sleep(200),
                            copy_remote_tables(Node)
                    end;
                _MnesiaNodes ->
                    lager:info("have my own cluster: ~p, joining", [_MnesiaNodes]),
                    mnesia:start(),
                    timer:sleep(1000),
                    ClusterNodes = [N || N  <- mnesia:system_info(db_nodes), net_adm:ping(N) == 'pong', N /= node()],
                    case ClusterNodes of
                        [] ->
                            lager:info("all cluster nodes offline, skip tables comparison");
                        _ ->
                            lager:info("comparing tables.."),
                            copy_remote_tables(ClusterNodes)
                    end
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

create_schema(Nodes)->
    case mnesia:create_schema(Nodes) of
        {'error', {_Node, {'already_exists', _Node}}} ->
            lager:info("skip creating schema, already exists"),
            'already_exists';
        {'error', Reason} ->
            lager:info("can't create schema: ~p", [Reason]),
            exit(Reason);
        _Ok ->
            lager:info("schema successfully created"),
            'ok'
    end.

copy_remote_tables(Node)->
    RemoteTables = mnesia:system_info(tables),
    [begin
         lager:info("Get table info: ~p from node: ~p", [Table, Node]),
         Holders = rpc:call(Node, mnesia, table_info, [Table, where_to_commit]), %problems with local version
         case proplists:is_defined(node(), Holders) of %if we don't have local table copy
             false ->
                 Type = element(2, hd(Holders)),
                 lager:info("copying table ~p as ~p", [Table, Type]),
                 mnesia:add_table_copy(Table, node(), Type);
             true ->
                 lager:info("table ~p already on this node, skiping", [Table]),
                 ok
         end
     end || Table <- RemoteTables, Table /= schema],                    %TODO: not all replicas needed
    mnesia:wait_for_tables(RemoteTables, 60000).                        %TODO: configure fragmented tables
