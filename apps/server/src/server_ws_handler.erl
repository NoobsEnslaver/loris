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
               ,protocol :: module()
               ,token}).

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
    Protocol = cowboy_req:binding('protocol', Req, <<"default">>), %TODO: может, default не нужен?
    Ver = cowboy_req:binding('version', Req, <<"v1">>),
    Token = cowboy_req:binding('token', Req),
    Transport = ws_utils:supported_transport(Req),
    case Transport /= [] andalso ws_utils:is_going_upgrade_to(Req, <<"websocket">>) of  %Если это вебсокет и мы поддерживаем транспорт..
        'true' ->                                                                       % и такой модуль есть
            try binary_to_existing_atom(<<"ws_", Protocol/binary, "_protocol_", Ver/binary>>, 'utf8') of %try, чтобы получать код ошибки 404 вместо 500
                Module ->
                    AllowedGroups = Module:allowed_groups(),
                    Session = sessions:get(Token),
                    GroupAccessGranted = case Session of                                % начинаем проверку доступов по группе и уровню доступа
                                             'false' ->
                                                 lists:member('guests', AllowedGroups);
                                             _ ->
                                                 Group = sessions:extract(Session, 'group'),
                                                 lists:member(Group, AllowedGroups)
                                         end,
                    case GroupAccessGranted of
                        'false' ->
                            Resp = cowboy_req:reply(403, #{}, <<"">>, Req),
                            {'ok', Resp, #state{}};                               %close connection, forbidden
                        'true' ->
                            case Module:access_level() of
                                'infinity' ->
                                    Resp = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, hd(Transport), Req),                %отвечаем, что готовы использовать один из транспортов
                                    {'cowboy_websocket', Resp, #state{transport = hd(Transport), protocol = Module, token = Token}, #{idle_timeout => ?TIMEOUT}};         %open websocket connection
                                _ModuleAL when Session == 'false' ->
                                    Resp = cowboy_req:reply(403, #{}, <<"">>, Req),
                                    {'ok', Resp, #state{}};                               %close connection, forbidden
                                ModuleAL ->
                                    AL = sessions:extract(Session, 'access_level'),
                                    if  ModuleAL >= AL ->
                                            Resp = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, hd(Transport), Req),
                                            {'cowboy_websocket', Resp, #state{transport = hd(Transport), protocol = Module, token = Token}, #{idle_timeout => ?TIMEOUT}};         %open websocket connection
                                        'true'         ->
                                            Resp = cowboy_req:reply(403, #{}, <<"">>, Req),
                                            {'ok', Resp, #state{}}                               %close connection, forbidden
                                    end
                            end
                    end
            catch
                _:_ ->
                    Resp = cowboy_req:reply(404, #{}, <<"">>, Req),
                    {'ok', Resp, #state{}}                                %close connection, protocol not found
            end;
        _False ->
            Resp = cowboy_req:reply(501, #{}, <<"">>, Req),
            {'ok', Resp, #state{}}                                %close connection, not implemented
    end.

terminate(_Reason, _Req, _State)->              %TODO: close ws when session expired
    'ok'.


%%%===================================================================
%%% Websocket handlers
%%%===================================================================
-spec websocket_init(#state{}) -> call_result(#state{}).
websocket_init(#state{token = Token, protocol = Module} = State) ->
    lager:md([{'appname', ?APP_NAME}]),
    US = Module:default_user_state(Token),                    %инициализируем начальный стейт протокола
    sessions:bind_pid_to_session(Token, self()),
    {'ok', State#state{user_state = US}, 'hibernate'}.        %TODO: research hibernation effect to CPU & RAM

-spec websocket_handle(cow_ws:frame(), #state{}) -> call_result(#state{}).
websocket_handle(_Frame = {'binary', BinData}, #state{transport = T, user_state = US, async_works = AW, protocol = Protocol} = State) ->
    RawMsg = ws_utils:decode_message(BinData, T),
    Msg = Protocol:unwrap_msg(RawMsg),
    case Protocol:do_action(Msg, US) of
        {'ok', NewUS} ->
            {'ok', State#state{user_state = NewUS}, 'hibernate'};
        {RawResp, NewUS} ->
            Resp = Protocol:wrap_msg(RawResp, T),
            {'reply', {'binary', Resp}, State#state{user_state = NewUS}, 'hibernate'};
        {'async', Pid, Ref, NewUS} ->           %старт асинхронной работы (процесс, запущенный с помощью ws_utils:do_async_work)
            Resp = Protocol:wrap_msg(#async_start{work_id = list_to_binary(erlang:ref_to_list(Ref))}, T),
            TimerRef = erlang:send_after(?ASYNC_WORK_TIMEOUT, self(), {'async_timeout', Pid}), %устанавилваем таймаут на выполнение работы
            {'reply', {'binary', Resp}, State#state{async_works = AW#{Pid => {Ref, TimerRef}}, user_state = NewUS}, 'hibernate'} %запоминаем Pid и timestamp в стэйте процесса
    end;
websocket_handle(Frame = {'text', _Data}, State) ->
    {'reply', Frame, State, 'hibernate'};       %TODO: может, стоит ввести общение и без транспорта?
websocket_handle(Frame = {'ping', _Data}, State) ->
    {'reply', Frame, State, 'hibernate'};       %TODO: несеклюрно работать в режиме зеркала с неподдерживаемыми сообщениями
websocket_handle(_Frame, State) ->
    {'ok', State, 'hibernate'}.

-spec websocket_info({'async_timeout', pid()} | {'async_done', pid(), map()} | {'DOWN', reference(), atom(), pid(), any()}, #state{}) -> call_result(#state{}).
websocket_info({'async_timeout', Pid}, State) -> %Таймаут асинхронной работы
    exit(Pid, 'timeout'),                        %убиваем процесс, передаётся управление на обработчик падения процесса DOWN
    {'ok', State, 'hibernate'};
websocket_info({'async_done', Pid, RawResp}, #state{async_works = AW, transport = T, protocol = Protocol} = State) ->
    {'ok', {Ref, TimerRef}} = maps:find(Pid, AW),
    Resp = Protocol:wrap_msg(#async_done{work_id = list_to_binary(erlang:ref_to_list(Ref)), result = RawResp}, T),
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
            Resp = Protocol:wrap_msg(#async_error{work_id = list_to_binary(erlang:ref_to_list(MonitorRef))}, T), %TODO: return error code
            {'reply', {'binary', Resp}, State#state{async_works = NewAW}, 'hibernate'};
        _ -> {'ok', State, 'hibernate'}
    end;
websocket_info(Msg, #state{transport = T, user_state = US, async_works = AW, protocol = Protocol} = State) ->
    case Protocol:do_action(Msg, US) of
        {'ok', NewUS} ->
            {'ok', State#state{user_state = NewUS}, 'hibernate'};
        {RawResp, NewUS} ->
            Resp = Protocol:wrap_msg(RawResp, T),
            {'reply', {'binary', Resp}, State#state{user_state = NewUS}, 'hibernate'};
        {'async', Pid, Ref, NewUS} ->           %старт асинхронной работы (процесс, запущенный с помощью ws_utils:do_async_work)
            Resp = Protocol:wrap_msg(#async_start{work_id = list_to_binary(erlang:ref_to_list(Ref))}, T),
            TimerRef = erlang:send_after(?ASYNC_WORK_TIMEOUT, self(), {'async_timeout', Pid}), %устанавилваем таймаут на выполнение работы
            {'reply', {'binary', Resp}, State#state{async_works = AW#{Pid => {Ref, TimerRef}}, user_state = NewUS}, 'hibernate'} %запоминаем Pid и timestamp в стэйте процесса
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
