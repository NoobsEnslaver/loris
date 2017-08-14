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
        ,access_level/1
        ,allowed_groups/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(_Method, _Req, #q_state{tmp_state = TmpState} = State, [Token | _Other]) ->
    Session = sessions:get(Token),
    Md = lager:md(),
    if Session == 'false' -> lager:md([{'msisdn', 'guest'} | Md]);
       'true' -> lager:md([{'msisdn', sessions:extract(Session, owner_id)} | Md])
    end,
    {_Req, State#q_state{tmp_state = TmpState#{'session' => Session}}, _Other}.

access_level(_Method) ->
    'infinity'.

allowed_groups(_Method) ->
    ['users', 'administrators', 'guests', 'company'].
