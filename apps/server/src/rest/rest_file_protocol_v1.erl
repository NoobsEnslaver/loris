%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  27 Dec 2016
%%%-------------------------------------------------------------------
-module(rest_file_protocol_v1).
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
            {Name, ContentType, Data, Req1} = receive_file(Req, MaxFileSize),
            NewState = case files:save(Name, ContentType, Data, UserId) of
                           'false'->
                               State#q_state{code = 500};
                           InDBId ->
                               lager:info("File ~s saved with Id: ~p~n", [Name, InDBId]),
                               State#q_state{code = 201, headers = H#{<<"content-type">> => <<"text/html">>}, body = erlang:integer_to_binary(InDBId)}
                       end,
            {Req1, NewState, _Args}
    end;
handle(<<"GET">>, Req, #q_state{headers = H, tmp_state = #{'session' := Session}} = State, [FileId | _Args]) ->
    UserId = sessions:extract(Session, 'owner_id'),
    User = users:get(UserId),
    UserAccessLevel = users:extract(User, 'access_level'),
    NewState = case files:get(common:to_integer(FileId)) of
                   'false' ->
                       State#q_state{code = 404};
                   File ->
                       FileOwnerId = files:extract(File, 'owner_id'),
                       FileAccessLevel = files:extract(File, 'access_level'),
                       case FileAccessLevel =< UserAccessLevel orelse FileOwnerId == UserId of
                           'true' ->
                               NewHeaders = H#{<<"Content-Type">> => files:extract(File, 'content_type')
                                              ,<<"Content-Length">> => integer_to_binary(files:extract(File, 'size'))},
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

allowed_groups(_Method) ->
    ['users', 'administrators'].
