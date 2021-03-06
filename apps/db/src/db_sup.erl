%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%% db top level supervisor.
%%% @end
%%% Created :  8 Dec 2016
%%%-------------------------------------------------------------------

-module(db_sup).

-behaviour(supervisor).
-include("db.hrl").
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
    Procs = [?WORKER('metrics_grabber')],
    SupFlags = #{'strategy' => 'one_for_one'
                ,'intensity'=> 5
                ,'period'   => 10},
    {'ok', {SupFlags, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
