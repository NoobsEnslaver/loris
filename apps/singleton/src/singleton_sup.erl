%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017
%%% @doc
%%% singleton top level supervisor.
%%% @end
%%% Created :  15 Aug 2017
%%%-------------------------------------------------------------------

-module(singleton_sup).
-behaviour(supervisor).
-include_lib("common/include/otp_definitions.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    Procs = [?WORKER('db_cleaner')],
    SupFlags = #{'strategy' => 'one_for_one'
                ,'intensity'=> 5
                ,'period'   => 10},
    {'ok', {SupFlags, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
