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

-callback access_level() -> non_neg_integer() | 'infinity'.

-callback allowed_groups() -> [atom()].

-callback wrap_data(binary(), map(), binary()) -> binary().

-callback do_action(MsgType :: tuple(), UserState :: tuple()) -> {'ok', UserState :: tuple()} | {'async', pid(), reference(), UserState :: tuple()} | {Type :: binary(), Msg :: map() | integer(), UserState :: tuple()}.

-callback construct_msg(map()) -> MsgType :: tuple().

-callback default_user_state('false' | tuple()) -> tuple().
