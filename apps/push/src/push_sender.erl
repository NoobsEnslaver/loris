%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2017
%%%-------------------------------------------------------------------
-module(push_sender).

-behaviour(gen_server).
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    LoudPushDelay = application:get_env('push', 'loud_push_delay', 60) * 1000,  %default: 1 min
    erlang:send_after(LoudPushDelay, self(), 'send_clean_pushes'),
    {'ok', #{push => LoudPushDelay}}.

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
handle_info('send_clean_pushes', #{push := PushInterval} = Map) ->
    erlang:send_after(PushInterval, self(), 'send_clean_pushes'),
    ExpirationTime = common:timestamp() - PushInterval,
    send_clean_pushes(ExpirationTime),
    {'noreply', Map};
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
send_clean_pushes(ExpirationTime)->
    Pushes = pushes:pull_outdated(ExpirationTime),
    lager:debug("start push cleaning: ~p pushes will be sended", [length(Pushes)]),
    lists:foreach(fun(P)->
                          MSISDN = pushes:extract(P, msisdn),
                          ChatName = pushes:extract(P, chat_name),
                          Msg = pushes:extract(P, last_msg),
                          Badge = pushes:extract(P, count),
                          push_app:notify_msg_loud(MSISDN, ChatName, Msg, Badge)
                  end, Pushes).
