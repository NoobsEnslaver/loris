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

-record(state, {push_server_pid :: pid()
               ,voip_server_pid :: pid()
               ,push_feedback_conf :: map()
               ,voip_feedback_conf :: map()
               ,voip_ref, push_ref
               ,get_feedback_interval :: non_neg_integer()}).

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
    erlang:send_after(GetFeedbackInterval, self(), 'get_push_feedback'),
    erlang:send_after(GetFeedbackInterval, self(), 'get_voip_feedback'),
    PrivDir = code:priv_dir('push'),
    Mode = application:get_env('push', 'mode', 'dev'),
    FeedbackTimeout = application:get_env('push', 'apns_get_feedback_timeout', 600) * 1000,      %default: 10 min
    VoipCertFilePath = PrivDir ++"/" ++ atom_to_list(Mode) ++ "/voip_cert.pem",
    VoipKeyFilePath = PrivDir ++ "/" ++ atom_to_list(Mode) ++ "/voip_key.pem",
    PushCertFilePath = PrivDir ++"/" ++ atom_to_list(Mode) ++ "/apns_push_cert.pem",
    PushKeyFilePath = PrivDir ++ "/" ++ atom_to_list(Mode) ++ "/apns_push_key.pem",
    {PushServer, FeedbackServer} = case Mode of
                                       'dev' -> {"api.development.push.apple.com", "feedback.sandbox.push.apple.com"};
                                       'prod'-> {"api.push.apple.com", "feedback.push.apple.com"}
                                   end,
    ApplePushConfig = #{'name' => apple_push
                       ,'apple_host' => PushServer
                       ,'apple_port' => 443
                       ,'type' => cert
                       ,'certfile' => PushCertFilePath
                       ,'keyfile' => PushKeyFilePath
                       ,'timeout' => 10000},
    AppleVoipPushConfig = #{'name' => apple_voip_push
                           ,'apple_host' => PushServer
                           ,'apple_port' => 443
                           ,'type' => cert
                           ,'certfile' => VoipCertFilePath
                           ,'keyfile' => VoipCertFilePath
                           ,'timeout' => 10000},
    PushFeedbackConf = #{certfile => PushCertFilePath
                        ,keyfile => PushKeyFilePath
                        ,host => FeedbackServer
                        ,port => 2196
                        ,timeout => FeedbackTimeout},
    VoipFeedbackConf = #{certfile => VoipCertFilePath
                        ,keyfile => VoipKeyFilePath
                        ,host => FeedbackServer
                        ,port => 2196
                        ,timeout => FeedbackTimeout},
    {ok, Pid1} = apns:connect(ApplePushConfig),
    {ok, Pid2} = apns:connect(AppleVoipPushConfig),
    {ok, #state{push_server_pid = Pid1
               ,voip_server_pid = Pid2
               ,push_feedback_conf = PushFeedbackConf
               ,voip_feedback_conf = VoipFeedbackConf
               ,get_feedback_interval = GetFeedbackInterval}}.

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
handle_cast({call, PushToken, CallerMSISDN}, _State) ->                    %incoming call
    Payload = #{<<"aps">> => #{<<"content-available">> => 1}
               ,<<"msisdn">> => erlang:integer_to_binary(CallerMSISDN)},
    Headers = #{'apns_priority' => <<"10">>},
    apns:push_notification(apple_voip_push, PushToken, Payload, Headers),
    {noreply, _State};
handle_cast({msg, PushToken, 'undefined', 'undefined'}, _State) ->  %chat invatation
    Payload = #{<<"aps">> => #{<<"content-available">> => 1}},
    apns:push_notification(apple_push, PushToken, Payload),
    {noreply, _State};
handle_cast({msg, PushToken, ChatId, MsgId}, _State) ->             %new chat msg
    Payload = #{<<"aps">> => #{<<"content-available">> => 1}
               ,<<"chat_id">> => ChatId
               ,<<"msg_id">> => MsgId},
    apns:push_notification(apple_push, PushToken, Payload),
    {noreply, _State};
handle_cast({msg_loud, PushToken, ChatName, Msg, Badge}, _State) ->             %loud push msg
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
handle_info('get_voip_feedback', #state{voip_feedback_conf = VoipFeedbackConf} = State) ->
    Self = self(),
    Fun = fun()-> Self ! {'feedback', apns:get_feedback(VoipFeedbackConf)} end,
    {_Pid, Ref} = erlang:spawn_monitor(Fun),
    {noreply, State#state{voip_ref = Ref}};
handle_info('get_push_feedback', #state{push_feedback_conf = PushFeedbackConf} = State) ->
    Self = self(),
    Fun = fun()-> Self ! {'feedback', apns:get_feedback(PushFeedbackConf)} end,
    {_Pid, Ref} = erlang:spawn_monitor(Fun),
    {noreply, State#state{push_ref = Ref}};
handle_info({'DOWN', Ref, _Type, _Pid, _Info}, #state{voip_ref = Ref, get_feedback_interval = GetFeedbackInterval} = State) ->
    erlang:send_after(GetFeedbackInterval, self(), 'get_voip_feedback'),
    {noreply, State#state{voip_ref = 'undefined'}};
handle_info({'DOWN', Ref, _Type, _Pid, _Info}, #state{push_ref = Ref, get_feedback_interval = GetFeedbackInterval} = State) ->
    erlang:send_after(GetFeedbackInterval, self(), 'get_push_feedback'),
    {noreply, State#state{push_ref = 'undefined'}};
handle_info({'feedback', 'timeout'}, _State) ->
    lager:info("Feedback: ~p~n", ['timeout']),
    {noreply, _State};
handle_info({'feedback', Feedback}, _State) when is_list(Feedback) ->
    lager:info("Feedback: ~p~n", [Feedback]),
    Resp = device:delete_devices_by_token([list_to_binary(Token) || {_Time, Token} <- Feedback]),
    lager:info("feedback handle result: ~p", [Resp]),
    %% TODO: metrics
    {noreply, _State};
handle_info({'timeout', _StreamId}, State) ->
    lager:debug("apns timeout on ~p", [_StreamId]),
    {noreply, State};
handle_info({apns_response, _ServerPid, _StreamID, _Response}, State) ->
    lager:debug("apns response on ~p:~p - ~p", [_ServerPid, _StreamID, _Response]),
    {noreply, State};
handle_info({reconnecting, _ServerPid}, State) ->
    lager:debug("apns reconnecting server ~p", [_ServerPid]),
    {noreply, State};
handle_info({connection_up, _ServerPid}, State) ->
    lager:debug("apns ~p connection_up", [_ServerPid]),
    {noreply, State};
handle_info(_Info, State) ->
    lager:info("unexpected msg ~p", [_Info]),
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
