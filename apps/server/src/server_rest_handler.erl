%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  14 Dec 2016
%%%-------------------------------------------------------------------
-module(server_rest_handler).
-behaviour(cowboy_handler).
-include("server.hrl").

-export([init/2
        ,terminate/3
        ]).


%%%===================================================================
%%% TCP handlers
%%%===================================================================
-spec init(cowboy_req:req(), list()) -> {'ok', cowboy_req:req(), []}.
init(Req, _Opts) ->
    lager:md([{'appname', ?APP_NAME}]),
    Query = cowboy_req:path_info(Req),
    Ver = cowboy_req:binding('version', Req, <<"v1">>),
    {Req1, State} = fold(Req, Ver, #q_state{}, Query),
    Resp = cowboy_req:reply(State#q_state.code, State#q_state.headers, State#q_state.body, Req1),
    {'ok', Resp, []}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State)->
    'ok'.

-spec fold(cowboy_req:req(), binary(), map(), [binary()]) -> cowboy_req:req().
fold(Req, _Ver, State, []) ->
    {Req, State};
fold(Req, Ver, State, [Mod | Args]) ->
    Module = binary_to_existing_atom(<<"rest_", Mod/binary, "_protocol_", Ver/binary>>, 'utf8'),
    {Req1, State1, Query1} = case Module:required_auth() of
                                 'true' ->
                                     QState = State#q_state.tmp_state,
                                     Session = maps:get('session', QState, 'false'),
                                     case Session of
                                         'false' ->
                                             {Req, State#q_state{code = 403}, []};
                                         _Else ->
                                             AL = sessions:extract(Session, 'access_level'),
                                             case cowboy_req:method(Req) of
                                                 <<"GET">>    ->
                                                     case Module:get_access_level() >= AL of
                                                         'true' ->
                                                             Module:get(Req, State, Args);
                                                         _False ->
                                                             {Req, State#q_state{code = 403}, []}
                                                     end;
                                                 <<"HEAD">>   ->
                                                     case Module:head_access_level() >= AL of
                                                         'true' ->
                                                             Module:head(Req, State, Args);
                                                         _False ->
                                                             {Req, State#q_state{code = 403}, []}
                                                     end;
                                                 <<"POST">>   ->
                                                     case Module:post_access_level() >= AL of
                                                         'true' ->
                                                             Module:post(Req, State, Args);
                                                         _False ->
                                                             {Req, State#q_state{code = 403}, []}
                                                     end;
                                                 <<"PUT">>    ->
                                                     case Module:put_access_level() >= AL of
                                                         'true' ->
                                                             Module:put(Req, State, Args);
                                                         _False ->
                                                             {Req, State#q_state{code = 403}, []}
                                                     end;
                                                 <<"PATCH">>  ->
                                                     case Module:patch_access_level() >= AL of
                                                         'true' ->
                                                             Module:patch(Req, State, Args);
                                                         _False ->
                                                             {Req, State#q_state{code = 403}, []}
                                                     end;
                                                 <<"DELETE">> ->
                                                     case Module:delete_access_level() >= AL of
                                                         'true' ->
                                                             Module:delete(Req, State, Args);
                                                         _False ->
                                                             {Req, State#q_state{code = 403}, []}
                                                     end;
                                                 <<"OPTIONS">>->
                                                     case Module:options_access_level() >= AL of
                                                         'true' ->
                                                             Module:options(Req, State, Args);
                                                         _False ->
                                                             {Req, State#q_state{code = 403}, []}
                                                     end
                                             end
                                     end;
                                 _False ->
                                     case cowboy_req:method(Req) of
                                         <<"GET">>    -> Module:get(Req, State, Args);
                                         <<"HEAD">>   -> Module:head(Req, State, Args);
                                         <<"POST">>   -> Module:post(Req, State, Args);
                                         <<"PUT">>    -> Module:put(Req, State, Args);
                                         <<"PATCH">>  -> Module:patch(Req, State, Args);
                                         <<"DELETE">> -> Module:delete(Req, State, Args);
                                         <<"OPTIONS">>-> Module:options(Req, State, Args)
                                     end
                             end,
    fold(Req1, Ver, State1, Query1).
