%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2016
%%%-------------------------------------------------------------------
-module(server_sup).

-behaviour(supervisor).
-include("server.hrl").
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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    lager:md([{'appname', ?APP_NAME}]),
    Procs = [],
    SupFlags = #{'strategy' => 'one_for_one'
                ,'intensity'=> 5
                ,'period'   => 10},
    {'ok', {SupFlags, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
