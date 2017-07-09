%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 30 Jun 2017
%%%-------------------------------------------------------------------
-module(push_apple_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {push_server_pid :: pid(), voip_server_pid :: pid()}).

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
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], Opts).

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
init([]) ->             %% TODO: increase init dropdown time
    GetFeedbackInterval = application:get_env('push', 'apns_get_feedback_interval', 60) * 1000,  %default: 1 min
    erlang:send_after(GetFeedbackInterval, self(), {'get_feedback', GetFeedbackInterval}),
    PrivDir = code:priv_dir('push'),
    ApplePushConfig = #{'name' => apple_push
                       ,'apple_host' => "api.development.push.apple.com"
                       ,'apple_port' => 443
                       ,'type' => cert
                       ,'certfile' => PrivDir ++ "/dev/apns_push_cert.pem"
                       ,'keyfile' => PrivDir ++ "/dev/apns_push_key.pem"
                       ,'timeout' => 10000},
    AppleVoipPushConfig = #{'name' => apple_voip_push
                           ,'apple_host' => "api.development.push.apple.com"
                           ,'apple_port' => 443
                           ,'type' => cert
                           ,'certfile' => PrivDir ++ "/dev/voip_cert_dev.pem"
                           ,'keyfile' => PrivDir ++ "/dev/voip_key_dev.pem"
                           ,'timeout' => 10000},
    {ok, Pid1} = apns:connect(ApplePushConfig),
    {ok, Pid2} = apns:connect(AppleVoipPushConfig),
    {ok, #state{push_server_pid = Pid1, voip_server_pid = Pid2}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({call, Device, CallerMSISDN}, _State) ->                                  %incoming call
    PushToken = device:extract(Device, 'push_token'),
    Payload = #{<<"aps">> => #{<<"content-available">> => 1}
               ,<<"msisdn">> => CallerMSISDN},
    Headers = #{},
    apns:push_notification(apple_voip_push, PushToken, Payload, Headers),
    {noreply, _State};
handle_cast({msg_silent, Device, 'undefined', 'undefined'}, _State) ->  %chat invatation
    PushToken = device:extract(Device, 'push_token'),
    Payload = #{<<"aps">> => #{<<"content-available">> => 1}},
    apns:push_notification(apple_push, PushToken, Payload),
    {noreply, _State};
handle_cast({msg_silent, Device, ChatId, MsgId}, _State) ->             %new chat msg
    PushToken = device:extract(Device, 'push_token'),
    Payload = #{<<"aps">> => #{<<"content-available">> => 1}
               ,<<"chat_id">> => ChatId
               ,<<"msg_id">> => MsgId},
    apns:push_notification(apple_push, PushToken, Payload),
    {noreply, _State};
handle_cast({msg, Device, ChatName, Msg, Badge}, _State) ->             %loud push msg
    PushToken = device:extract(Device, 'push_token'),
    Payload = #{<<"aps">> => #{<<"alert">> => #{<<"title">> => ChatName,
                                                <<"body">> => Msg}}
               ,<<"badge">> => Badge},     %% number of unread
    apns:push_notification(apple_push, PushToken, Payload),
    {noreply, _State};
handle_cast(_Msg, _State) ->
    lager:info("unexpected msg on ~p:~p: ~p", [?MODULE, ?LINE, _Msg]),
    {noreply, _State}.

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
handle_info({'get_feedback', GetFeedbackInterval}, State) ->
    %% TODO: delete some devices
    %% TODO: metrics
    erlang:send_after(GetFeedbackInterval, self(), {'get_feedback', GetFeedbackInterval}),
    {noreply, State};
handle_info({'timeout', _StreamId}, State) ->
    {noreply, State};
handle_info({apns_response, _ServerPid, _StreamID, _Response}, State) ->
    {noreply, State};
handle_info({reconnecting, _ServerPid}, State) ->
    {noreply, State};
handle_info({connection_up, _ServerPid}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    lager:info("unexpected msg on ~p:~p: ~p", [?MODULE, ?LINE, _Info]),
    {noreply, State}.

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
    apns:stop(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
