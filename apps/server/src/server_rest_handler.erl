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
    State = maps:new(),
    Resp = fold(Req, Ver, State, Query),
    {'ok', Resp, []}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State)->
    'ok'.

-spec fold(cowboy_req:req(), binary(), map(), [binary()]) -> cowboy_req:req().
fold(Req, _Ver, _State, []) ->
    Req;
fold(Req, Ver, State, [Mod | Args]) ->
    Module = binary_to_existing_atom(<<"rest_", Mod/binary, "_protocol_", Ver/binary>>, 'utf8'),
    {Req1, State1, Query1} = case cowboy_req:method(Req) of
                                 <<"GET">>    -> Module:get(Req, State, Args);
                                 <<"HEAD">>   -> Module:head(Req, State, Args);
                                 <<"POST">>   -> Module:post(Req, State, Args);
                                 <<"PUT">>    -> Module:put(Req, State, Args);
                                 <<"PATCH">>  -> Module:patch(Req, State, Args);
                                 <<"DELETE">> -> Module:delete(Req, State, Args);
                                 <<"OPTIONS">>-> Module:options(Req, State, Args)
                             end,
    fold(Req1, Ver, State1, Query1).
