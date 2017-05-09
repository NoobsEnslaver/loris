%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  14 Dec 2016
%%%-------------------------------------------------------------------
-module(ws_protocol_behaviour).
-include("server.hrl").

%% Возвращает уровень доступа к модулю
-callback access_level() -> non_neg_integer() | 'infinity'.

%% Возвращает список групп, имеющих доступ к модулю
-callback allowed_groups() -> [atom()].

%% Преобразовывает ответ сервера в бинарный формат согласно оговоренному транспорту
-callback wrap_msg(tuple(), binary()) -> binary().

%% Описывает действия, основанные на полученном от клиента сообщении и стэйте пользователя
-callback do_action(Msg :: tuple(), UserState :: tuple()) -> {'ok', UserState :: tuple()} | {'async', pid(), reference(), UserState :: tuple()} | {Msg :: tuple(), UserState :: tuple()}.

%% Преобразовывает сообщение клиента из бинарного формата в типизированный внутренний формат сервера
-callback unwrap_msg(Msg :: map()) -> Msg2 :: tuple().

%% Конструктор начального стэйта пользователя на основании данных о сессии
-callback default_user_state('false' | tuple()) -> tuple().

%% Хэндлер закрытия сокета
-callback terminate(UserState :: tuple()) -> ok.
