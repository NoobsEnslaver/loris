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

-callback wrap_msg(tuple(), binary()) -> binary().

-callback do_action(Msg :: tuple(), UserState :: tuple()) -> {'ok', UserState :: tuple()} | {'async', pid(), reference(), UserState :: tuple()} | {Msg :: tuple(), UserState :: tuple()}.

-callback unwrap_msg(Msg :: map()) -> Msg2 :: tuple().

-callback default_user_state('false' | tuple()) -> tuple().
