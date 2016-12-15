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
init(Req, Opts) ->
    lager:md([{'appname', ?APP_NAME}]),
    Protocol = cowboy_req:binding('protocol', Req, <<"default">>),
    Ver = cowboy_req:binding('version', Req, <<"v1">>),
    ProtocolModule = binary_to_existing_atom(<<"rest_", Protocol/binary, "_protocol_", Ver/binary>>, 'utf8'),
    lager:info("using rest protocol ~p", [ProtocolModule]),
    Resp = case cowboy_req:method(Req) of
               <<"GET">>    -> ProtocolModule:get(Req, Opts);
               <<"HEAD">>   -> ProtocolModule:head(Req, Opts);
               <<"POST">>   -> ProtocolModule:post(Req, Opts);
               <<"PUT">>    -> ProtocolModule:put(Req, Opts);
               <<"PATCH">>  -> ProtocolModule:patch(Req, Opts);
               <<"DELETE">> -> ProtocolModule:delete(Req, Opts);
               <<"OPTIONS">>->ProtocolModule:options(Req, Opts)
           end,
    {'ok', Resp, []}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State)->
    'ok'.
