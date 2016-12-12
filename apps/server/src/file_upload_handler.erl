%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2016
%%%-------------------------------------------------------------------
-module(file_upload_handler).
-behaviour(cowboy_handler).
-include("server.hrl").
-include_lib("common/include/tables.hrl").

-export([init/2
        ,terminate/3
        ]).


%%%===================================================================
%%% TCP handlers
%%%===================================================================
-spec init(cowboy_req:req(), list()) -> {'ok', cowboy_req:req(), []}.
init(Req, [MaxFileSize]) ->
    Token = cowboy_req:binding('token', Req, <<>>),
    {OwnerId, _UserName, _Pid} = get_session(Token),            %TODO
    IsAuthorized = is_authorized(Token),                        %TODO
    Resp = case cowboy_req:body_length(Req) of
               'undefined' ->
                   cowboy_req:reply(411, #{}, <<>>, Req); %Length Required error
               _Mastodonic when _Mastodonic > MaxFileSize ->
                   cowboy_req:reply(413, #{}, <<>>, Req); %Request Entity Too Large error
               _NormalSize when IsAuthorized /= 'true' ->
                   cowboy_req:reply(401, #{}, <<>>, Req); %Unauthorized error
               _NormalSize ->
                   {Name, ContentType, Data, Req1} = receive_file(Req, MaxFileSize),
                   io:format("Name: ~p~nContentType: ~p~nDataLength: ~p~nToken: ~p~n", [Name, ContentType, byte_size(Data), Token]),
                   InDBId = save_file(Name, ContentType, Data, OwnerId),
                   io:format("~nFile ~s saved with Id: ~p~n", [Name, InDBId]),
                   cowboy_req:reply(201, #{<<"content-type">> => <<"text/html">>}, InDBId, Req1)
           end,
    {'ok', Resp, []}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State)->
    'ok'.

-spec receive_file(cowboy_req:req(), non_neg_integer()) -> {binary(), binary(), binary(), cowboy_req:req()}.
-spec receive_file(cowboy_req:req(), binary(), non_neg_integer()) -> {binary(), cowboy_req:req()}.
receive_file(Req, MaxFileSize) ->
    {'ok', Headers, Req2} = cowboy_req:read_part(Req),
    {'file', <<"inputfile">>, Filename, ContentType, _TE}
        = cow_multipart:form_data(Headers),
    {Data, Req3} = receive_file(Req2, <<>>, MaxFileSize),
    {Filename, ContentType, Data, Req3}.
receive_file(Req, Buffer, MaxFileSize) ->
    case cowboy_req:read_part_body(Req) of
        {'ok'  , Data, Req1} when byte_size(Buffer) =< MaxFileSize ->
            {<<Buffer/binary, Data/binary>>, Req1};
        {'more', Data, Req1} when byte_size(Buffer) =< MaxFileSize ->
            NewBuf = <<Buffer/binary, Data/binary>>,
            receive_file(Req1, NewBuf, MaxFileSize)
    end.

-spec save_file(binary(), binary(), binary(), binary()) -> binary().
save_file(Name, Type, Data, OwnerId) ->
    Hash = crypto:hash('md5', <<Data/binary, Name/binary, OwnerId/binary>>),
    Fun = fun()->
                  mnesia:write(#files{hash=Hash, name=Name, content_type = Type, data=Data, owner_id = OwnerId})
          end,
    mnesia:transaction(Fun),
    Hash.

is_authorized(_Token)->
    'true'.

-spec get_session(binary()) -> {binary(), binary(), pid()}.
get_session(_Token)->
    {<<>>, <<>>, self()}.
