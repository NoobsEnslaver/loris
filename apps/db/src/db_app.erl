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
    DbCluster = [node() | application:get_env(?APP_NAME, nodes, [])],
    IsNew = case mnesia:create_schema(DbCluster) of
                {'error', {Node, {'already_exists', _Node}}} ->
                    lager:info("skip creating schema, already exists on: ~p", [Node]),
                    'old';
                {'error', Reason} ->
                    lager:info("can't create schema: ~p", [Reason]),
                    'error';
                _Ok ->
                    lager:info("schema successfully created on nodes ~p", [DbCluster]),
                    'new'
            end,
    mnesia:start(),
    case IsNew of
        'new' ->
            create_tables();
        _ -> 'ok'
    end,
    db_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    'ok'.

%%====================================================================
%% Internal functions
%%====================================================================
create_tables()->
    lists:foreach(fun({Name, Opts}) ->
                          lager:info("~nTry to create table '~p' with options: ~p", [Name, Opts]),
                          case mnesia:create_table(Name, Opts) of
                              {'atomic', 'ok'} -> 'ok';
                              {'error', Reason} ->
                                  lager:info("error on create table ~p, reason: ~p~n", [Name, Reason]) %TODO
                          end
                  end, ?DEFAULT_SCHEMA).
