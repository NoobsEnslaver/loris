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
-export([get/3
        ,head/3
        ,post/3
        ,put/3
        ,patch/3
        ,delete/3
        ,options/3]).

-spec post(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
post(Req, #q_state{headers = H, body = B, tmp_state = #{'session' := Session}} = State, _Args) ->
    UserId = sessions:extract(Session, 'owner_id'),
    MaxFileSize = application:get_env(binary_to_atom(?APP_NAME, 'utf8'), 'max_file_size', 16777216),
    case cowboy_req:body_length(Req) of
        'undefined' ->
            {Req, State#q_state{code = 411}, _Args};                   %Length Required error
        _Mastodonic when _Mastodonic > MaxFileSize ->
            {Req, State#q_state{code = 413}, _Args};                   %Request Entity Too Large error
        _NormalSize ->
            {Name, ContentType, Data, Req1} = receive_file(Req, MaxFileSize),
            io:format("Name: ~p~nContentType: ~p~nDataLength: ~p~n", [Name, ContentType, byte_size(Data)]),
            InDBId = files:save(Name, ContentType, Data, UserId),
            io:format("~nFile ~s saved with Id: ~p~n", [Name, InDBId]),
            NewState = State#q_state{code = 201, headers = H#{<<"content-type">> => <<"text/html">>}, body = <<B/binary, InDBId/binary>>},
            {Req1, NewState, _Args}
    end;
post(Req, State, _Args) ->
    {Req, State#q_state{code = 401}, _Args}.                          %Unauthorized error

-spec get(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
get(Req, #q_state{body = B, headers = H, tmp_state = #{'session' := Session}} = State, [FileId | _Args]) ->
    UserId = sessions:extract(Session, 'owner_id'),
    User = users:get_by_id(UserId),
    UserAccessLevel = users:extract(User, 'access_level'),
    File = files:get(FileId),
    FileOwnerId = files:extract(File, 'owner_id'),
    FileAccessLevel = files:extract(File, 'access_level'),
    case FileAccessLevel =< UserAccessLevel orelse FileOwnerId == UserId of
        'true' ->
            Data = files:extract(File, 'data'),
            NewHeaders = H#{<<"Content-Type">> => files:extract(File, 'content_type')
                           ,<<"Content-Length">> => integer_to_binary(files:extract(File, 'size'))},
            {Req, State#q_state{code = 200, body = <<B/binary,Data/binary>>, headers = NewHeaders}, _Args};
        _ ->
                {Req, State#q_state{code = 403}, _Args}
    end;
get(Req, State, [_|_Args]) ->
    {Req, State#q_state{code = 401}, _Args}.

-spec head(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
head(Req, State, Args) ->
    post(Req, State, Args).

-spec put(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
put(Req, State, Args) ->
    post(Req, State, Args).

-spec patch(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
patch(Req, State, Args) ->
    post(Req, State, Args).

-spec delete(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
delete(Req, State, Args) ->
    post(Req, State, Args).

-spec options(cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
options(Req, State, Args) ->
    post(Req, State, Args).


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
