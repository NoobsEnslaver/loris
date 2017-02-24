%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2016
%%%-------------------------------------------------------------------
-module(server_ws_handler).
-behaviour(cowboy_websocket).
-include("server.hrl").

-export([init/2
        ,terminate/3
        ,websocket_init/1
        ,websocket_handle/2
        ,websocket_info/2
        ]).

-record(state, {transport = <<>>
               ,async_works = #{}
               ,user_state
               ,protocol :: module()}).

-type call_result(State) :: {'ok', State}
                          | {'ok', State, 'hibernate'}
                          | {'reply', cow_ws:frame() | [cow_ws:frame()], State}
                          | {'reply', cow_ws:frame() | [cow_ws:frame()], State, 'hibernate'}
                          | {'stop', State}.

%%%===================================================================
%%% TCP handlers
%%%===================================================================
-spec init(cowboy_req:req(), map()) -> {'cowboy_websocket', cowboy_req:req(), #state{}, timeout()} | {'ok', cowboy_req:req(), #state{}}.
init(Req, _Opts) ->
    lager:md([{'appname', ?APP_NAME}]),
    Protocol = cowboy_req:binding('protocol', Req, <<"default">>),
    Ver = cowboy_req:binding('version', Req, <<"v1">>),
    Token = cowboy_req:binding('token', Req),
    ProtocolModule = binary_to_existing_atom(<<"ws_", Protocol/binary, "_protocol_", Ver/binary>>, 'utf8'),
    Transport = ws_utils:supported_transport(Req),
    AL = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'ws_access_level', 10),
    case Transport /= [] andalso ws_utils:is_going_upgrade_to(Req, <<"websocket">>) of
        'true' ->
            case application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'ws_secured', 'false') of
                'true' when Token == 'undefined'->
                    Resp = cowboy_req:reply(403, #{}, <<"">>, Req),
                    {'ok', Resp, #state{}};                                %close connection, forbidden
                'true' ->
                    Session = sessions:get(Token),
                    UAL = sessions:extract(Session, 'access_level'),
                    if
                        AL < UAL ->
                            Resp = cowboy_req:reply(403, #{}, <<"">>, Req),
                            {'ok', Resp, #state{}};                                %close connection, forbidden;
                        'true' ->
                            US = ProtocolModule:default_user_state(Session),
                            Resp = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, hd(Transport), Req),
                            {'cowboy_websocket', Resp, #state{transport = hd(Transport), protocol = ProtocolModule, user_state = US}, ?TIMEOUT}         %open authorized websocket connection
                    end;
                _False ->
                    US = ProtocolModule:default_user_state(),
                    Resp = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, hd(Transport), Req),                                                %open unauthorized websocket connection
                    {'cowboy_websocket', Resp, #state{transport = hd(Transport), protocol = ProtocolModule, user_state = US}, ?TIMEOUT}                 %TODO: test timeout influence to peroformance
            end;
        _False ->
            Resp = cowboy_req:reply(501, #{}, <<"">>, Req),
            {'ok', Resp, #state{}}                                %close connection, not implemented
    end.

terminate(_Reason, _Req, _State)->
    'ok'.


%%%===================================================================
%%% Websocket handlers
%%%===================================================================
-spec websocket_init(#state{}) -> call_result(#state{}).
websocket_init(_State) ->
    lager:md([{'appname', ?APP_NAME}]),
    {'ok', _State, 'hibernate'}.        %TODO: research hibernation effect to CPU & RAM

-spec websocket_handle(cow_ws:frame(), #state{}) -> call_result(#state{}).
websocket_handle(_Frame = {'binary', BinData}, #state{transport = T, user_state = US, async_works = AW, protocol = Protocol} = State) ->
    RawMsg = ws_utils:decode_message(BinData, T),
    Msg = Protocol:construct_msg(RawMsg),
    case Protocol:do_action(Msg, US) of
        {Type, RawResp, NewUS} ->
            Resp = Protocol:wrap_data(Type, RawResp, T),
            {'reply', {'binary', Resp}, State#state{user_state = NewUS}, 'hibernate'};
        {'async', Pid, Ref, NewUS} ->
            Resp = Protocol:wrap_data(<<"async_start">>, list_to_binary(erlang:ref_to_list(Ref)), <<>>, T),
            TimerRef = erlang:send_after(?ASYNC_WORK_TIMEOUT, self(), {'async_timeout', Pid}),
            {'reply', {'binary', Resp}, State#state{async_works = AW#{Pid => {Ref, TimerRef}}, user_state = NewUS}, 'hibernate'};
        {'ok', NewUS} ->
            {'ok', State#state{user_state = NewUS}, 'hibernate'}
    end;
websocket_handle(Frame = {'text', _Data}, State) ->
    {'reply', Frame, State, 'hibernate'};
websocket_handle(Frame = {'ping', _Data}, State) ->
    {'reply', Frame, State, 'hibernate'};
websocket_handle(_Frame, State) ->
    {'ok', State, 'hibernate'}.

-spec websocket_info({'async_timeout', pid()} | {'async_done', pid(), map()} | {'DOWN', reference(), atom(), pid(), any()}, #state{}) -> call_result(#state{}).
websocket_info({'async_timeout', Pid}, State) ->
    exit(Pid, 'timeout'),
    {'ok', State, 'hibernate'};
websocket_info({'async_done', Pid, RawResp}, #state{async_works = AW, transport = T, protocol = Protocol} = State) ->
    {'ok', {Ref, TimerRef}} = maps:find(Pid, AW),
    Resp = Protocol:wrap_data(<<"async_done">>, list_to_binary(erlang:ref_to_list(Ref)), RawResp, T),
    erlang:cancel_timer(TimerRef),
    NewAW = maps:remove(Pid, AW),
    {'reply', {'binary', Resp}, State#state{async_works = NewAW}, 'hibernate'};
websocket_info({'DOWN', _MonitorRef, _Type, _Pid, 'normal'}, State) ->
    {'ok', State, 'hibernate'};
websocket_info({'DOWN', MonitorRef, _Type, Pid, ErrorReason}, #state{async_works = AW, transport = T, protocol = Protocol} = State) ->
    lager:info("async request failed with reason ~p", [ErrorReason]),
    case maps:find(Pid, AW) of
        {'ok', {_MonitorRef, TimerRef}} ->
            erlang:cancel_timer(TimerRef),
            NewAW = maps:remove(Pid, AW),
            Resp = Protocol:wrap_data(<<"async_error">>, list_to_binary(erlang:ref_to_list(MonitorRef)), <<>>, T),
            {'reply', {'binary', Resp}, State#state{async_works = NewAW}, 'hibernate'};
        _ -> {'ok', State, 'hibernate'}
    end;
websocket_info(_Info, State) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
