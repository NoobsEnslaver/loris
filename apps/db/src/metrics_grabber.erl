%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 24 Dec 2016
%%%-------------------------------------------------------------------
-module(metrics_grabber).

-behaviour(gen_server).
-include("db.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-define(SERVER, ?MODULE).
-define(GET_METRICS_INTERVAL, 1800000).     %30 min
%% -define(GET_METRICS_INTERVAL, 1800).     %30 min

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Metrics = [<<"cpu">>, <<"ram">>, <<"users_online">>],
    lists:foreach(fun(Metric)->
                          case folsom_metrics:metric_exists(Metric) of
                              'true' -> ok;
                              'false'-> folsom_metrics:new_histogram(Metric)
                          end,
                          erlang:send_after(?GET_METRICS_INTERVAL, self(), Metric)
                  end, Metrics),
    cpu_sup:start(),
    cpu_sup:util(),
    lager:info("metrics_grabber started"),
    {'ok', #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) ->
    lager:debug("unexpected call ~p", [_Request]),
    {'noreply', _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, _State) ->
    lager:debug("unexpected message ~p", [_Msg]),
    {'noreply', _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(<<"users_online">>, _State) ->
    erlang:send_after(?GET_METRICS_INTERVAL, self(), <<"users_online">>),
    folsom_metrics:notify(<<"users_online">>, {common:timestamp(), mnesia:table_info('pids', 'size')}),
    {'noreply', _State};
handle_info(<<"ram">>, _State) ->
    erlang:send_after(?GET_METRICS_INTERVAL, self(), <<"ram">>),
    get_ram_usage(),
    {'noreply', _State};
handle_info(<<"cpu">>, _State) ->
    erlang:send_after(?GET_METRICS_INTERVAL, self(), <<"cpu">>),
    get_cpu_utilization(),
    {'noreply', _State};
handle_info(_Info, _State) ->
    lager:debug("unexpected message ~p", [_Info]),
    {'noreply', _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:info("sessions cleaner turned off"),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_ram_usage() ->
    RamInfo = folsom_vm_metrics:get_memory(),
    folsom_metrics:notify(<<"ram">>, {common:timestamp(), RamInfo}).

get_cpu_utilization() ->
    CpuUtil = cpu_sup:util(),
    folsom_metrics:notify(<<"cpu">>, {common:timestamp(), CpuUtil}).
