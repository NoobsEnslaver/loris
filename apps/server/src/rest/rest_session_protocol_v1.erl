%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  23 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_session_protocol_v1).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").
-export([handle/4
        ,access_level/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(_Method, _Req, #q_state{tmp_state = TmpState} = State, [Token | _Other]) ->
    Session = sessions:get(Token),
    if Session == 'false' ->
                   {IP, _Port} = cowboy_req:peer(_Req),
                   lager:md([{'appname', <<"server->session">>}]),
                   lager:info("try to get unexisting session from IP: ~p", [IP]);
              'true' -> 'ok'
           end,
    {_Req, State#q_state{tmp_state = TmpState#{'session' => Session}}, _Other}.

access_level(_Method) ->
    'infinity'.
