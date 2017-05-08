%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 24 Dec 2016
%%%-------------------------------------------------------------------
-module(sessions_supervisor).

-behaviour(gen_server).
-include("db.hrl").

%% API
-export([start_link/0
        ,cleaning/0]).

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
    lager:md([{'appname', list_to_binary(?MODULE_STRING)}]),
    CleaningInterval = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'sessions_cleaning_interval', 300) * 1000, %5 min
    erlang:send_after(CleaningInterval, self(), 'let_clean'),
    lager:info("sessions cleaner started"),
    case mnesia:transaction(fun()-> mnesia:table_info('users', 'size') end) of
        {'atomic', 0} ->
            erlang:send_after(1000, self(), 'need_administrator');
        _ -> 'ok'
    end,
    {'ok', CleaningInterval}.

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
handle_info('let_clean', CleaningInterval) ->
    erlang:send_after(CleaningInterval, self(), 'let_clean'),
    cleaning(),
    {'noreply', CleaningInterval};
handle_info('need_administrator', _CleaningInterval) ->
    %% User = list_to_binary(io:get_line("Administrator login: ") -- "\n"),
    %% Pwd = list_to_binary(io:get_line("Administrator password: ") -- "\n"),
    %% Name = list_to_binary(io:get_line("Administrator name: ") -- "\n"),
    %% users:new(User, Pwd, Name, 'administrators', 0),
    {'noreply', _CleaningInterval};
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
cleaning() ->
    lager:info("start sessions cleaning"),
    Now = common:timestamp(),
    MatchHead = #session{token = '$1', expiration_time = '$2', owner_id = '$3', ws_pid = '$4'},
    Guard = {'>', Now, '$2'},
    Result = ['$1', '$2', '$3', '$4'],
    Fun = fun() ->
                  List = mnesia:select('session',[{MatchHead, [Guard], [Result]}]),
                  lists:foreach(fun([T,E,OID,W]) ->
                                        mnesia:delete_object(#session{token = T, expiration_time = E, owner_id = OID, ws_pid = W})
                                end, List)
          end,
    mnesia:transaction(Fun).
