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

terminate(_Reason, _Req, #state{protocol = Module, user_state = UserState} = _State)->
    Module:terminate(UserState),
    'ok'.


%%%===================================================================
%%% Websocket handlers
%%%===================================================================
-spec websocket_init(#state{}) -> call_result(#state{}).
websocket_init(#state{token = Token, protocol = Module} = State) ->
    lager:md([{'appname', ?APP_NAME}]),
    sessions:bind_pid_to_session(Token, self()),
    US = Module:default_user_state(Token),                    %инициализируем начальный стейт протокола
    {'ok', State#state{user_state = US}, 'hibernate'}.        %TODO: research hibernation effect to CPU & RAM

-spec websocket_handle(cow_ws:frame(), #state{}) -> call_result(#state{}).
websocket_handle({DataType, Data}, #state{transport = T, user_state = US, protocol = Protocol} = State) when DataType == 'binary' orelse DataType == 'text' ->
    RawMsg = ws_utils:decode_message(Data, T),
    Msg = Protocol:unwrap_msg(RawMsg),
    case Protocol:do_action(Msg, US) of
        {'ok', NewUS} ->
            {'ok', State#state{user_state = NewUS}, 'hibernate'};
        {RawResp, NewUS} ->
            Resp = Protocol:wrap_msg(RawResp, T),
            {'reply', {DataType, Resp}, State#state{user_state = NewUS}, 'hibernate'}
    end;
websocket_handle({'ping', _Data}, State) ->
    {'reply', {'pong', <<>>}, State, 'hibernate'};
websocket_handle(_Frame, State) ->
    {'ok', State, 'hibernate'}.

websocket_info(Msg, #state{transport = T, user_state = US, protocol = Protocol} = State) ->
    case Protocol:do_action(Msg, US) of
        {'ok', NewUS} ->
            {'ok', State#state{user_state = NewUS}, 'hibernate'};
        {RawResp, NewUS} ->
            Resp = Protocol:wrap_msg(RawResp, T),
            {'reply', {'binary', Resp}, State#state{user_state = NewUS}, 'hibernate'}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
