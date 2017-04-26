%%%-------------------------------------------------------------------
%%% @author ne <ne@ne>
%%% @copyright (C) 2017, ne
%%% @doc
%%%
%%% @end
%%% Created : 19 Apr 2017 by ne <ne@ne>
%%%-------------------------------------------------------------------
-module(chat_protocol_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ws_chat_protocol_v1_messages.hrl").
-include("server.hrl").
-import(tester,[connect_to_ws/2
               ,authorize/2
               ,send_packet/3
               ,receive_packet/2]).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    [application:ensure_all_started(App) || App <- [db, server, common, tester]],
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Env = init(),
    [{env, Env} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Env = proplists:get_value(env, Config),
    deinit(Env),
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{chats, [], [chat_get_list_test
                 ,chat_get_info_unexists_chat_test
                 ,chat_create_test
                 ,chat_create_and_get_info_test
                 ,chat_invite_user_test
                 %% ,chat_leave_test
                 %% ,chat_delete_test

                 %% ,chat_accept_invatation_test
                 %% ,chat_reject_invatation_test
                 %% ,chat_mute_test
                 %% ,chat_unmute_test
                 %% ,chat_typing_test
                 ]}
    ,{messages, [], [message_send_test
                    ,message_get_list_test
                    ,message_update_test
                    ,message_update_status_test]}
    ,{users, [], [user_get_info_test
                 ,user_get_status_test
                 ,user_set_info_test
                 ,user_search_test]}
    ,{rooms, [], [room_get_tree_test
                 ,room_get_info_test
                 ,room_rename_test
                 ,room_add_user_test
                 ,room_del_user_test
                 ,room_add_subroom_test
                 ,room_create_test
                 ,room_delete_test
                 ,room_enter_to_chat_test
                 ,room_send_message_test]}
    ,{system, [], [system_logout_test]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, chats}
    %% ,{group, messages}
    %% ,{group, users}
    %% ,{group, rooms}
    %% ,{group, system}
    ].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
chat_get_list_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:map(fun(#{transport := Transport, connection := ConPid})->
                      send_packet(ConPid, maps:put(<<"msg_type">>, 1, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list)), Transport),
                      timer:sleep(100),
                      #{<<"msg_type">> := 102, <<"chat_id">> := []} = receive_packet(ConPid, Transport),
                      timer:sleep(300),
                      {error, timeout} = receive_packet(ConPid, Transport)
              end, Env).

chat_get_info_unexists_chat_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:map(fun(#{transport := Transport, connection := ConPid})->
                      send_packet(ConPid, maps:put(<<"msg_type">>, 2, ?R2M(#c2s_chat_get_info{chat_id = <<"qweqwe">>}, c2s_chat_get_info)), Transport),
                      timer:sleep(100),
                      #{<<"msg_type">> := 126, <<"code">> := 404} = receive_packet(ConPid, Transport),
                      timer:sleep(300),
                      {error, timeout} = receive_packet(ConPid, Transport)
              end, Env).

chat_create_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:map(fun(#{transport := Transport, connection := ConPid})->
                      send_packet(ConPid, maps:put(<<"msg_type">>, 3, ?R2M(#c2s_chat_create{name = <<"test_chat">>, users = []}, c2s_chat_create)), Transport),
                      timer:sleep(50),
                      #{<<"msg_type">> := 104, <<"chat_id">> := ChatId} = receive_packet(ConPid, Transport),
                      timer:sleep(50),
                      send_packet(ConPid, maps:put(<<"msg_type">>, 1, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list)), Transport),
                      timer:sleep(50),
                      #{<<"msg_type">> := 102, <<"chat_id">> := [ChatId]} = receive_packet(ConPid, Transport),
                      timer:sleep(100),
                      {error, timeout} = receive_packet(ConPid, Transport)
              end, Env).

chat_create_and_get_info_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:map(fun(#{user := User, transport := Transport, connection := ConPid})->
                      ChatName = <<"test_chat">>,
                      MSISDN = users:extract(User, msisdn),
                      send_packet(ConPid, maps:put(<<"msg_type">>, 3, ?R2M(#c2s_chat_create{name = ChatName, users = []}, c2s_chat_create)), Transport),
                      timer:sleep(50),
                      #{<<"msg_type">> := 104, <<"chat_id">> := ChatId} = receive_packet(ConPid, Transport),
                      timer:sleep(50),
                      send_packet(ConPid, maps:put(<<"msg_type">>, 2, ?R2M(#c2s_chat_get_info{chat_id = ChatId}, c2s_chat_get_info)), Transport),
                      timer:sleep(50),
                      #{<<"msg_type">> := 103
                       ,<<"chat_id">> := ChatId
                       ,<<"name">> := ChatName
                       ,<<"users">> := [MSISDN]
                       ,<<"is_muted">> := 'false'
                       ,<<"chat_owner">> := MSISDN
                       ,<<"access_group">> := <<"administrators">>} = receive_packet(ConPid, Transport),
                      timer:sleep(100),
                      {error, timeout} = receive_packet(ConPid, Transport)
              end, Env).

chat_invite_user_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
init()->
    lists:map(fun(Transport)->
                      MSISDN = crypto:rand_uniform(1000000, 99999999),
                      User = users:new(MSISDN, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                      {ok, Token} = authorize(MSISDN, <<"121">>),
                      {ok, ConPid} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport),
                      #{user => User, token => Token, connection => ConPid, transport => Transport}
              end, ?SUPPORTED_TRANSPORT).

deinit([])->
    ok;
deinit([#{user := User, token := Token, connection := ConPid} | Tail])->
    ok = tester:disconnect(ConPid),
    ok = sessions:delete(Token),
    ok = users:delete(User),
    deinit(Tail).
