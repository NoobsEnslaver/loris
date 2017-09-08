%%%-------------------------------------------------------------------
%%% @author ne <ne@ne>
%%% @copyright (C) 2017, ne
%%% @doc
%%%
%%% @end
%%% Created : 25 Aug 2017 by ne <ne@ne>
%%%-------------------------------------------------------------------
-module(tester_load).

-include_lib("common/include/transport_lib.hrl").
-include("apps/server/include/ws_chat_protocol_v1_messages.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1
        ,user_behaviour_1/1]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {user_intensity = 200 :: non_neg_integer()
               ,pids = #{} :: map()
               ,refs = #{} :: map()
               ,num_of_friends = 10 :: non_neg_integer()}).

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
%% OptsMap: port, pin, server_address, transport, msg_intensity
start_link(OptsMap) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, OptsMap, []).

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
init(#{user_intensity := UIntensity, num_of_friends := NumOfFriends} = Args) ->
    case folsom_metrics:metric_exists(sended_msg) of
        'true' -> ok;
        'false'-> folsom_metrics:new_counter(sended_msg)
    end,
    case folsom_metrics:metric_exists(users) of
        'true' -> ok;
        'false'-> folsom_metrics:new_counter(users)
    end,
    erlang:send_after(1000, self(), 'tick'),
    {ok, {#state{user_intensity = UIntensity, num_of_friends = NumOfFriends}, Args}}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info('tick', {#state{user_intensity = UIntensity, num_of_friends = NumOfFriends, pids = Pids, refs = Refs} = State, OptsMap}) ->
    erlang:send_after(60000, self(), 'tick'),
    Base = rand:uniform(899999999999) + 100000000000,
    NewUsers = lists:seq(Base, Base + UIntensity),
    NewPidsRefs = lists:map(fun(U)->
                                Bias = rand:uniform(UIntensity - NumOfFriends),
                                Friens = lists:sublist(NewUsers, Bias, NumOfFriends),
                                {U, spawn_monitor(?MODULE, user_behaviour_1, [OptsMap#{msisdn => U, friends => Friens}])}
                        end, NewUsers),
    NewPids = lists:foldl(fun({U, {Pid, _Ref}}, Acc) ->
                                  Acc#{U => Pid}
                          end, Pids, NewPidsRefs),
    NewRefs = lists:foldl(fun({U, {_Pid, Ref}}, Acc) ->
                                  Acc#{Ref => U}
                          end, Refs, NewPidsRefs),
    lager:info("Users count: ~p", [length(maps:keys(NewPids))]),
    {noreply,  {State#state{pids = NewPids, refs = NewRefs}, OptsMap}};
handle_info({'DOWN', Ref, _Type, _Pid, _ErrorReason}, {#state{pids = Pids, refs = Refs} = State, _Args}) ->
    MSISDN = maps:get(Ref, Refs),
    {noreply, {State#state{refs = maps:remove(Ref, Refs), pids = maps:remove(MSISDN, Pids)}, _Args}};
handle_info(Info, State) ->
    lager:info("unexpected message ~p", [Info]),
    {noreply,  State}.

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
user_behaviour_1(#{msisdn := MSISDN} = Args)->
    timer:sleep(1000 + rand:uniform(10000)),
    ServerAddress = maps:get(server_address, Args, "127.0.0.1"),
    ServerPort = maps:get(port, Args, 8080),
    MasterPin = maps:get(pin, Args, 6666),
    Transport = maps:get(transport, Args, <<"msgpack">>),
    {ok, Token} = tester:authorize(MSISDN, ServerAddress, ServerPort, MasterPin),
    {ok, ConPid} = tester:connect_to_ws(ServerAddress, ServerPort, "/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport),
    folsom_metrics:notify(users, {inc, 1}),
    timer:sleep(20000),                         %waiting for others
    user_behaviour_1(Args#{token => Token, con_pid => ConPid, transport := Transport}, rand:uniform(1000)).
user_behaviour_1(#{friends := FriendsList, msg_intensity := MsgIntensity, con_pid := ConPid, transport := Transport} = Args, _BehaviourNum) ->
    MSISDN2 = lists:nth(rand:uniform(length(FriendsList)), FriendsList),
    R = crypto:strong_rand_bytes(8),
    ChatCreateCommand = ?R2M(#c2s_chat_create{name = <<"test_chat">>, users = [MSISDN2], is_p2p = true}, c2s_chat_create),
    tester:send_packet(ConPid, ChatCreateCommand#{<<"ref">> => R}, Transport),
    [#{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} | _] = tester:receive_packets_until(ConPid, Transport, get_predicate_by_ref(R)),
    RawMessages = [{?R2M(#c2s_message_send{chat_id = ChatId, msg_body = <<"Hello Joe?">>}, c2s_message_send), crypto:strong_rand_bytes(8)} || lists:seq(1, MsgIntensity)],
    [begin
         tester:send_packet(ConPid, Msg#{<<"ref">> => Ref}, Transport),
         [#{<<"msg_type">> := ?S2C_MESSAGE_SEND_RESULT_TYPE, <<"chat_id">> := ChatId} | _] = tester:receive_packets_until(ConPid, Transport, get_predicate_by_ref(Ref)),
         folsom_metrics:notify(sended_msg, {inc, 1}),
         timer:sleep(60000/MsgIntensity)
     end || {Msg, Ref} <- RawMessages],
    user_behaviour_1(Args, rand:uniform(1000)).

get_predicate_by_ref(Ref1)->
    fun(#{<<"ref">> := Ref2}) when Ref1 == Ref2 -> 'true';
       (_) -> 'false'
    end.
