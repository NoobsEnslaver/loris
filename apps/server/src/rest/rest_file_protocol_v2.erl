%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  27 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_file_protocol_v2).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").
-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"POST">>, Req, #q_state{headers = H, tmp_state = #{'session' := Session}} = State, _Args) ->
    UserId = sessions:extract(Session, 'owner_id'),
    MaxFileSize = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'max_file_size', 16777216),
    case cowboy_req:body_length(Req) of
        'undefined' ->
            {Req, State#q_state{code = 411}, _Args};                   %Length Required error
        _Mastodonic when _Mastodonic > MaxFileSize ->
            {Req, State#q_state{code = 413}, _Args};                   %Request Entity Too Large error
        _NormalSize ->
            {Req1, Files} = receive_files(Req, MaxFileSize),           %TODO: optimize to one-for-one upload\save, streaming responcing
            [{Data, Name, ContentType} | _Rest] = Files,
            lager:debug("Rest files: ~p", [[N || {_, N, _} <- _Rest]]),
            NewState = case files:save(Name, ContentType, Data, UserId) of
                           'false'->
                               State#q_state{code = 500};
                           InDBId ->
                               lager:debug("File ~s saved with Id: ~p~n", [Name, InDBId]),
                               Hash = common:bin2hex(crypto:hash('md5', Data)),
                               RespBody = transport_lib:encode(#{<<"id">> => InDBId, <<"secret">> => Hash}, ?JSON),
                               State#q_state{code = 201, headers = H#{<<"content-type">> => <<"text/html">>}, body = RespBody}
                       end,
            {Req1, NewState, _Args}
    end;
handle(<<"GET">>, Req, #q_state{headers = H, tmp_state = #{'session' := Session}} = State, [FileId | [Secret | _Args]]) ->
    UserId = sessions:extract(Session, 'owner_id'),
    NewState = case files:get(common:to_integer(FileId)) of
                   'false' ->
                       State#q_state{code = 404};
                   File ->
                       FileOwnerId = files:extract(File, 'owner_id'),
                       FileSecret = files:extract(File, 'hash'),
                       FileName = files:extract(File, 'name'),
                       HSecret = list_to_binary(string:to_upper(binary_to_list(Secret))),
                       case HSecret == FileSecret orelse FileOwnerId == UserId of
                           'true' ->
                               NewHeaders = H#{<<"content-type">> => files:extract(File, 'content_type')
                                              ,<<"content-disposition">> => <<"attachment; filename=\"", FileName/binary, "\"">>},
                               State#q_state{code = 200, body = files:extract(File, 'data'), headers = NewHeaders};
                           'false'->
                               State#q_state{code = 403}
                       end
               end,
    {Req, NewState, _Args};
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.

access_level(_Method)->
    5.

%%%===================================================================
%%% internal functions
%%%===================================================================
-spec receive_files(cowboy_req:req(), non_neg_integer()) -> {cowboy_req:req(), [{Data :: binary(), Name :: binary(), Type :: binary()}]}.
receive_files(Req, MaxFileSize) ->
    receive_files(Req, [], MaxFileSize).
receive_files(Req, Files, MaxFileSize) ->
    case cowboy_req:read_part(Req) of
        {'ok', Headers, Req1} ->
            {Req2, Result} = case cow_multipart:form_data(Headers) of
                                 {data, FieldName} ->
                                     {ok, Body, Req3} = cowboy_req:read_body(Req1),
                                     {Req3, {Body, FieldName, <<>>}};
                                 {file, _FieldName, FileName1, CType} ->
                                     case stream_file(Req1, MaxFileSize) of
                                         {Req3, 'error'} -> {Req3, 'error'};
                                         {Req3, Data} -> {Req3, {Data, FileName1, CType}}
                                     end
                             end,
            receive_files(Req2, [Result | Files], MaxFileSize);
        {'done', Req1} ->
            {Req1, [F || F <- Files, is_tuple(F)]}
    end.


stream_file(Req, MaxFileSize)->
    stream_file(Req, <<>>, MaxFileSize).
stream_file(Req, Buffer, MaxFileSize) ->
    case cowboy_req:read_part_body(Req,  #{'length' => 64000}) of
        {'ok'  , Data, Req1} when byte_size(Buffer) =< MaxFileSize ->
            {Req1, <<Buffer/binary, Data/binary>>};
        {'more', Data, Req1} when byte_size(Buffer) =< MaxFileSize ->
            NewBuf = <<Buffer/binary, Data/binary>>,
            stream_file(Req1, NewBuf, MaxFileSize);
        _ -> {Req, 'error'}
    end.

allowed_groups(_Method) ->
    ['users', 'administrators', 'company'].
