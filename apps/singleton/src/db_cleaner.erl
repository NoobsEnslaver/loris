%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 24 Dec 2016
%%%-------------------------------------------------------------------
-module(db_cleaner).

-behaviour(gen_server).
-include_lib("common/include/tables.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0
        ,sms_cleaning/0
        ,sessions_cleaning/0]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SessionsCleaningInterval = application:get_env('singleton', 'sessions_cleaning_interval', 3600) * 1000, %default: 1h
    SmsCleaningInterval = application:get_env('singleton', 'sms_cleaning_interval', 900) * 1000,  %default: 15 min
    ResourcesUpdateInterval = application:get_env('singleton', 'resources_update_interval', 900) * 1000,  %default: 15 min
    erlang:send_after(SessionsCleaningInterval, self(), 'clean_sessions'),
    erlang:send_after(SmsCleaningInterval, self(), 'clean_sms'),
    erlang:send_after(ResourcesUpdateInterval, self(), 'update_resources'),
    lager:info("db cleaner started"),
    {'ok', #{sms => SmsCleaningInterval, session => SessionsCleaningInterval, resources_update_interval => ResourcesUpdateInterval}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) ->
    lager:debug("unexpected call ~p", [_Request]),
    {'noreply', _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, _State) ->
    lager:debug("unexpected message ~p", [_Msg]),
    {'noreply', _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info('update_resources', #{resources_update_interval := ResourcesUpdateInterval} = Map) ->
    erlang:send_after(ResourcesUpdateInterval, self(), 'update_resources'),
    update_resources(),
    {'noreply', Map};
handle_info('clean_sessions', #{session := CleaningInterval} = Map) ->
    erlang:send_after(CleaningInterval, self(), 'clean_sessions'),
    sessions_cleaning(),
    {'noreply', Map};
handle_info('clean_sms', #{sms := CleaningInterval} = Map) ->
    erlang:send_after(CleaningInterval, self(), 'clean_sms'),
    sms_cleaning(),
    {'noreply', Map};
handle_info(_Info, _State) ->
    lager:debug("unexpected message ~p", [_Info]),
    {'noreply', _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:info("sessions cleaner turned off"),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sessions_cleaning() ->
    lager:debug("start sessions cleaning"),
    Now = common:timestamp(),
    MatchHead = #session{token = '$1', expiration_time = '$2', owner_id = '$3'}, %TODO: will close websocket
    Guard = {'>', Now, '$2'},
    Result = ['$1'],
    Fun = fun() ->
                  List = mnesia:select('session',[{MatchHead, [Guard], [Result]}]),
                  lists:foreach(fun([T]) -> mnesia:delete({session, T}) end, List)
          end,
    mnesia:transaction(Fun).

sms_cleaning() ->
    lager:debug("start sms cleaning"),
    Now = common:timestamp(),
    SmsLiveTime = application:get_env('db', 'sms_live_time', 3600) * 1000, %1 hour
    Q = qlc:q([M#sms.msisdn || M <- mnesia:table(sms), Now - M#sms.timestamp  > SmsLiveTime]),
    mnesia:transaction(fun()->
                               ExpiredSms = qlc:e(Q),
                               lists:foreach(fun(S)->
                                                     mnesia:delete({sms, S})
                                             end, ExpiredSms)
                       end).

update_resources()->
    ResourcesPath = case application:get_env('singleton', 'resources_path', 'priv') of
                        'priv' -> code:priv_dir('singleton') ++ "/resources/";
                        Path -> Path            %TODO: if no / in the end - add
                    end,
    update_specific_resources(),
    {ok, RootFileList} = file:list_dir(ResourcesPath),
    lists:foreach(fun(File)->
                          case file:read_file_info(ResourcesPath ++ File) of
                              {ok, #file_info{type = directory}} ->     %directory
                                  {ok, DirFileList} = file:list_dir(ResourcesPath ++ File),
                                  lists:foreach(fun(SubDirFile)->
                                                        case file:read_file_info(ResourcesPath ++ File ++ "/" ++ SubDirFile) of
                                                            {ok, #file_info{type = directory}} ->
                                                                ok;
                                                            {ok, #file_info{}} ->
                                                                maybe_update_resource(erlang:list_to_binary(File), erlang:list_to_binary(SubDirFile), ResourcesPath ++ File ++ "/");
                                                            _Else ->
                                                                ok
                                                        end
                                                end, DirFileList);
                              {ok, _} ->                                %file
                                  maybe_update_resource(<<"common">>, erlang:list_to_binary(File), ResourcesPath);
                              _ ->                                         %read_file_info error, ignore
                                  ok
                          end
                  end, RootFileList).

update_specific_resources() ->
    SpecificResourcesSpec = application:get_env('singleton', 'specific_resources', []),
    lists:foreach(fun({Group, Name, {M,F,A}})->
                          Value = erlang:apply(M,F,A),
                          resources:set(Group, Name, Value);
                     ({Group, Name, Value}) ->
                          resources:set(Group, Name, Value)
                  end, SpecificResourcesSpec).

-spec get_file_type(binary()) -> binary().
get_file_type(FileName) ->
    case binary:split(FileName, <<".">>) of
        [_, <<"jpg">> = X] -> <<"image/", X/binary>>;
        [_, <<"jpeg">> = X] -> <<"image/", X/binary>>;
        [_, <<"png">> = X] -> <<"image/", X/binary>>;
        [_, <<"gif">> = X] -> <<"image/", X/binary>>;
        [_, <<"bmp">> = X] -> <<"image/", X/binary>>;
        [_, <<"avi">> = X] -> <<"video/", X/binary>>;
        [_, <<"mkv">> = X] -> <<"video/", X/binary>>;
        [_, <<"flv">> = X] -> <<"video/", X/binary>>;
        [_, <<"mp4">> = X] -> <<"video/", X/binary>>;
        [_, <<"3gp">> = X] -> <<"video/", X/binary>>;
        [_, <<"wav">> = X] -> <<"audio/", X/binary>>;
        [_, <<"mp3">>] -> <<"audio/mpeg">>;
        [_, <<"flac">> = X] -> <<"audio/", X/binary>>;
        _ -> <<"application/x-www-form-urlencoded">>
    end.

maybe_update_resource(Group, FileName, Cwd) ->
    {'ok', Data} = file:read_file(Cwd ++ erlang:binary_to_list(FileName)),
    DataHash = common:bin2hex(crypto:hash('md5', Data)),
    case resources:get(FileName) of
        'false' -> %resource not loaded
            case files:save(FileName, get_file_type(FileName), Data, 0) of
                'false'  -> ok;
                NewFileId->
                    resources:set(Group, FileName, #{file_id => NewFileId, secret => DataHash})
            end;
        #{secret := DataHash} -> %resource loaded, actual version, ignore
            ok;
        #{file_id := FileId} -> %resource loaded, need update
            files:delete(FileId),
            resources:delete(FileName),
            case files:save(FileName, get_file_type(FileName), Data, 0) of
                'false'  -> ok;
                NewFileId->
                    resources:set(Group, FileName, #{file_id => NewFileId, secret => DataHash})
            end;
        _ ->      %undefined resource format, ignore (maybe rewrite?)
            ok
    end.
