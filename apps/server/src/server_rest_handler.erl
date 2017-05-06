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
    InitState = #q_state{req_body = common:get_body_data(Req)},
    {Req1, State} = fold(Req, Ver, InitState, Query),
    Resp = cowboy_req:reply(State#q_state.code, State#q_state.headers, State#q_state.body, Req1),
    {'ok', Resp, []}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State)->
    'ok'.

%%%===================================================================
%%% internal functions
%%%===================================================================
-spec fold(cowboy_req:req(), binary(), map(), [binary()]) -> cowboy_req:req().
fold(Req, _Ver, State, []) ->
    {Req, State};
fold(Req, Ver, State, [Mod | Args]) ->
    try binary_to_existing_atom(<<"rest_", Mod/binary, "_protocol_", Ver/binary>>, 'utf8') of
        Module ->
            Method = cowboy_req:method(Req),
            QState = State#q_state.tmp_state,
            Session = maps:get('session', QState, 'false'),
            AllowedGroups = Module:allowed_groups(Method),
            GroupAccessGranted = case Session of
                                     'false' ->
                                         lists:member('guests', AllowedGroups);
                                     _ ->
                                         Group = sessions:extract(Session, 'group'),
                                         lists:member(Group, AllowedGroups)
                                 end,
            case GroupAccessGranted of
                'false' ->
                    {Req, State#q_state{code = 403}};
                'true' ->
                    {Req1, State1, Query1} = case Module:access_level(Method) of
                                                 'infinity' ->
                                                     Module:handle(Method, Req, State, Args);
                                                 _ModuleAL when Session == 'false' ->
                                                     {Req, State#q_state{code = 403}, []};
                                                 ModuleAL ->
                                                     AL = sessions:extract(Session, 'access_level'),
                                                     if  ModuleAL >= AL -> Module:handle(Method, Req, State, Args);
                                                         'true'         -> {Req, State#q_state{code = 403}, []}
                                                     end
                                             end,
                    fold(Req1, Ver, State1, Query1)
            end
    catch
        _:_ ->
            {Req, State#q_state{code = 404}}
    end.
