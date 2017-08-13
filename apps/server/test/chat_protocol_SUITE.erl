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
               ,authorize/1
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
                 ,chat_create_p2p_test
                 ,chat_create_and_get_info_test
                 ,chat_invite_accept_test
                 ,chat_invite_accept_on_chat_creation_test
                 ,chat_invite_reject_on_chat_creation_test
                 ,chat_leave_test
                 ,chat_delete_test
                 ,chat_typing_mute_unmute_test
                 ]}
    ,{messages, [], [message_send_test
                    ,message_send_p2p_test
                    ,message_get_list_test
                    ,message_update_test
                    ,message_update_status_test
                    ]}
    ,{users, [], [user_get_info_test
                 ,user_get_info_bulk_test
                 ,user_get_status_test
                 ,user_subscribe_unsubscribe_test
                 ,user_set_info_test
                 ,user_search_test
                 ]}
    ,{rooms, [], [room_create_test
                 ,room_get_info_test
                 ,room_set_info_test
                 ,room_join_to_chat_test
                 ,room_delete_test
                 ,room_add_del_subroom_test
                 ,room_send_recursive_message_test
                 ]}
    ,{calls, [], [call_normal_answer_test
                 ,call_reject_call_test
                 ,call_to_busy_test
                 ,call_to_bad_msisdn_test
                 ,call_to_offline_test
                 ]}
    ,{system, [], [%% system_logout_test,
                  storage_test]}].

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
    ,{group, messages}
    ,{group, users}
    ,{group, rooms}
    ,{group, system}
    ,{group, calls}
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

%%--------------------------------------------------------------------
%%      CHAT
%%--------------------------------------------------------------------
chat_get_list_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid})->
                          send_packet(ConPid, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list), Transport),
                          timer:sleep(100),
                          #{<<"msg_type">> := ?S2C_CHAT_LIST_TYPE, <<"chats">> := #{}} = receive_packet(ConPid, Transport),
                          timer:sleep(300),
                          {error, timeout} = receive_packet(ConPid, Transport)
                  end, Env).

chat_get_info_unexists_chat_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid})->
                          send_packet(ConPid, ?R2M(#c2s_chat_get_info{chat_id = <<"qweqwe">>}, c2s_chat_get_info), Transport),
                          timer:sleep(100),
                          #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid, Transport),
                          timer:sleep(300),
                          {error, timeout} = receive_packet(ConPid, Transport)
                  end, Env).

chat_create_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid})->
                          send_packet(ConPid, ?R2M(#c2s_chat_create{name = <<"test_chat">>, users = []}, c2s_chat_create), Transport),
                          timer:sleep(50),
                          #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid, Transport),
                          timer:sleep(50),
                          send_packet(ConPid, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list), Transport),
                          timer:sleep(50),
                          #{<<"msg_type">> := ?S2C_CHAT_LIST_TYPE, <<"chats">> := #{ChatId := <<"test_chat">>}} = receive_packet(ConPid, Transport),
                          timer:sleep(100),
                          {error, timeout} = receive_packet(ConPid, Transport)
                  end, Env).

chat_create_and_get_info_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{user := User, transport := Transport, connection := ConPid})->
                          ChatName = <<"test_chat">>,
                          MSISDN = users:extract(User, msisdn),
                          send_packet(ConPid, ?R2M(#c2s_chat_create{name = ChatName, users = []}, c2s_chat_create), Transport),
                          timer:sleep(50),
                          #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid, Transport),
                          timer:sleep(50),
                          send_packet(ConPid, ?R2M(#c2s_chat_get_info{chat_id = ChatId}, c2s_chat_get_info), Transport),
                          timer:sleep(50),
                          #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
                           ,<<"chat_id">> := ChatId
                           ,<<"name">> := ChatName
                           ,<<"users">> := [MSISDN]
                           ,<<"is_muted">> := 'false'
                           ,<<"chat_owner">> := MSISDN} = receive_packet(ConPid, Transport),
                          timer:sleep(100),
                          {error, timeout} = receive_packet(ConPid, Transport)
                  end, Env).

chat_create_p2p_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = <<"test_chat">>, users = [MSISDN2], is_p2p = true}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    timer:sleep(50),
    send_packet(ConPid1, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list), Transport2),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_LIST_TYPE, <<"chats">> := #{ChatId := <<"test_chat">>}} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_LIST_TYPE, <<"chats">> := #{ChatId := <<"test_chat">>}} = receive_packet(ConPid2, Transport2),
    timer:sleep(100),
    send_packet(ConPid2, ?R2M(#c2s_chat_create{name = <<"test_chat">>, users = [MSISDN1], is_p2p = true}, c2s_chat_create), Transport2),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    timer:sleep(50),

    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.

chat_invite_accept_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = []}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    %% Get chat info as admin and user before accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    %% Send invataton
    send_packet(ConPid1, ?R2M(#c2s_chat_invite_user{chat_id = ChatId, user_msisdn = MSISDN2, access_level = 3}, c2s_chat_invite_user), Transport1),
    timer:sleep(50),
    %% User1 receive message about invatation in chat
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:invite_to_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User2 receive invatation
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    %% User1 receive message in chat about User2 accepting invatation
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:accept_invatation">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Get chat info as admin and user after accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    'true' = lists:member(MSISDN1, BothUsers),
    'true' = lists:member(MSISDN2, BothUsers),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{ChatId := ChatName} = get_chats_list(ConPid2, Transport2),
    timer:sleep(50),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.


chat_invite_accept_on_chat_creation_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User1 receive message about invatation in chat
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:invite_to_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User2 receive invatation
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    %% Get chat info as admin and user before accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    %% User1 receive message in chat about User2 accepting invatation
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:accept_invatation">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Get chat info as admin and user after accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    'true' = lists:member(MSISDN1, BothUsers),
    'true' = lists:member(MSISDN2, BothUsers),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{ChatId := ChatName} = get_chats_list(ConPid2, Transport2),
    timer:sleep(50),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.


chat_invite_reject_on_chat_creation_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User1 receive message about invatation in chat
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:invite_to_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User2 receive invatation
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    %% Reject invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_reject_invatation{chat_id = ChatId}, c2s_chat_reject_invatation), Transport2),
    timer:sleep(50),
    %% User1 receive message in chat about User2 reject invatation
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:reject_invatation">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Get chat info as admin and user after accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    timer:sleep(50),
    %% Try accept rejected invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_accept_invatation), Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid2, Transport2),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.


chat_leave_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User1 receive message about invatation in chat
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:invite_to_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User2 receive invatation
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    %% User1 receive message in chat about User2 accepting invatation
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:accept_invatation">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{ChatId := ChatName} = get_chats_list(ConPid2, Transport2),
    timer:sleep(50),
    %% Get chat info as admin and user after accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    'true' = lists:member(MSISDN1, BothUsers),
    'true' = lists:member(MSISDN2, BothUsers),
    %% User2 leave chat
    send_packet(ConPid2, ?R2M(#c2s_chat_leave{chat_id = ChatId}, c2s_chat_leave), Transport2),
    timer:sleep(50),
    %% User1 receive message about leaver
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:leave_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    %% Get chat info as admin and user after accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    ok.

chat_delete_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User1 receive message about invatation in chat
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:invite_to_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% User2 receive invatation
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    %% User1 receive message in chat about User2 accepting invatation
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"@system:accept_invatation">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{ChatId := ChatName} = get_chats_list(ConPid2, Transport2),
    timer:sleep(50),
    %% Get chat info as admin and user after accept invatation
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1} = get_chat_info(ConPid2, Transport2, ChatId),
    'true' = lists:member(MSISDN1, BothUsers),
    'true' = lists:member(MSISDN2, BothUsers),
    %% User2 try to delete chat
    send_packet(ConPid2, ?R2M(#c2s_chat_delete{chat_id = ChatId}, c2s_chat_delete), Transport2),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid2, Transport2), %Error: forbidden,
    %% User1 delete chat
    send_packet(ConPid1, ?R2M(#c2s_chat_delete{chat_id = ChatId}, c2s_chat_delete), Transport1),
    timer:sleep(50),
    %% User2 receive message in chat about User1 deleting chat
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN1, <<"msg_body">> := <<"@system:delete_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    %% Get my chats list
    #{} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    %% Get chat info as admin and user after accept invatation
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404}  = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404}  = get_chat_info(ConPid2, Transport2, ChatId),
    ok.


%%--------------------------------------------------------------------
%%      MESSAGE
%%--------------------------------------------------------------------
message_send_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id, invite User2
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    tester:flush_messages(),
    %% Start chating
    {_, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN1, <<"msg_body">> := <<"Hello Joe?">>, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    {_, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"Hello Mike!">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Try to send to unexisting chat, get error
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = send_message(ConPid1, Transport1, <<"SOME_UNEXISTING_CHAT_ID">>, <<"Hello world?">>),
    %% Delete chat and try to send to deleted chat
    send_packet(ConPid1, ?R2M(#c2s_chat_delete{chat_id = ChatId}, c2s_chat_delete), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN1, <<"msg_body">> := <<"@system:delete_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    #{} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    ok.

message_send_p2p_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id, invite User2
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2], is_p2p = 'true'}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    %% Start chating
    {_, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN1, <<"msg_body">> := <<"Hello Joe?">>, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    {_, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"Hello Mike!">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Try to send to unexisting chat, get error
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = send_message(ConPid1, Transport1, <<"SOME_UNEXISTING_CHAT_ID">>, <<"Hello world?">>),
    %% Delete chat and try to send to deleted chat
    send_packet(ConPid1, ?R2M(#c2s_chat_delete{chat_id = ChatId}, c2s_chat_delete), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN1, <<"msg_body">> := <<"@system:delete_chat">>, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    #{} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    ok.

message_get_list_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id, invite User2
    send_packet(ConPid2, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN1]}, c2s_chat_create), Transport2),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid2, Transport2),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_id">> := MsgId00} = receive_packet(ConPid2, Transport2),
    %% Accept invatation
    send_packet(ConPid1, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_id">> := MsgId01} = receive_packet(ConPid2, Transport2),
    timer:sleep(50),
    tester:flush_messages(),
    %% Start chating
    {MsgId1, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    receive_packet(ConPid2, Transport2),
    {MsgId2, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    receive_packet(ConPid1, Transport1),
    {MsgId3, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"It's still working?">>),
    receive_packet(ConPid2, Transport2),
    {MsgId4, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Yeah, it's fine.">>),
    receive_packet(ConPid1, Transport1),
    {MsgId5, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"Fine">>),
    receive_packet(ConPid2, Transport2),
    {MsgId6, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Yeah, fine">>),
    receive_packet(ConPid1, Transport1),
    {MsgId7, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"Good  bye">>),
    receive_packet(ConPid2, Transport2),
    {MsgId8, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Bye">>),
    receive_packet(ConPid1, Transport1),
    %% Requests chat history
    MsgHistory = [#{<<"from">> => MSISDN1, <<"msg_id">> => MsgId00, <<"msg_body">> => <<"@system:invite_to_chat">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN1, <<"msg_id">> => MsgId01, <<"msg_body">> => <<"@system:accept_invatation">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN1, <<"msg_id">> => MsgId1, <<"msg_body">> => <<"Hello Joe?">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN2, <<"msg_id">> => MsgId2, <<"msg_body">> => <<"Hello Mike!">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN1, <<"msg_id">> => MsgId3, <<"msg_body">> => <<"It's still working?">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN2, <<"msg_id">> => MsgId4, <<"msg_body">> => <<"Yeah, it's fine.">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN1, <<"msg_id">> => MsgId5, <<"msg_body">> => <<"Fine">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN2, <<"msg_id">> => MsgId6, <<"msg_body">> => <<"Yeah, fine">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN1, <<"msg_id">> => MsgId7, <<"msg_body">> => <<"Good  bye">>, <<"status">> => <<"pending">>}
                 ,#{<<"from">> => MSISDN2, <<"msg_id">> => MsgId8, <<"msg_body">> => <<"Bye">>, <<"status">> => <<"pending">>}],
    send_packet(ConPid1, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = 0, count = 30, direction = 'down'}, c2s_message_get_list), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := MsgHistory} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid1, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = MsgId8, count = 30, direction = 'up'}, c2s_message_get_list), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := MsgList1} = receive_packet(ConPid1, Transport1),
    MsgList1 = tl(lists:reverse(MsgHistory)),
    send_packet(ConPid1, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = MsgId8, count = 3, direction = 'up'}, c2s_message_get_list), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := MsgList2} = receive_packet(ConPid1, Transport1),
    MsgList2 = lists:sublist(lists:reverse(MsgHistory),2,3),
    send_packet(ConPid1, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = 0, count = 3, direction = 'down'}, c2s_message_get_list), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := MsgList3} = receive_packet(ConPid1, Transport1),
    MsgList3 = lists:sublist(MsgHistory,1,3),
    send_packet(ConPid1, #{<<"chat_id">> => ChatId, <<"msg_type">> => ?C2S_MESSAGE_GET_LIST_TYPE}, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := MsgList4} = receive_packet(ConPid1, Transport1),
    MsgList4 = lists:reverse(MsgHistory),
    send_packet(ConPid1, #{<<"chat_id">> => ChatId, <<"msg_type">> => ?C2S_MESSAGE_GET_LIST_TYPE, <<"count">> => 3}, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := MsgList5} = receive_packet(ConPid1, Transport1),
    MsgList5 = lists:sublist(lists:reverse(MsgHistory),1,3),
    ok.

chat_typing_mute_unmute_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id, invite User2
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    tester:flush_messages(),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"is_muted">> := 'false'} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"is_muted">> := 'false'} = get_chat_info(ConPid2, Transport2, ChatId),
    send_packet(ConPid1, ?R2M(#c2s_chat_typing{chat_id = ChatId}, c2s_chat_typing), Transport1),        %user1 typing
    #{<<"msg_type">> := ?S2C_CHAT_TYPING_TYPE, <<"chat_id">> := ChatId, <<"user_msisdn">> := MSISDN1} = receive_packet(ConPid2, Transport2), %user2 receive typing msg
    send_packet(ConPid2, ?R2M(#c2s_chat_mute{chat_id = ChatId}, c2s_chat_mute), Transport2),            %user2 mute chat
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"is_muted">> := 'false'} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"is_muted">> := 'true'} = get_chat_info(ConPid2, Transport2, ChatId),
    send_packet(ConPid1, ?R2M(#c2s_chat_typing{chat_id = ChatId}, c2s_chat_typing), Transport1),        %user1 typing
    {error, timeout} = receive_packet(ConPid2, Transport2),                                             %user2 not receive typing msg
    send_packet(ConPid2, ?R2M(#c2s_chat_typing{chat_id = ChatId}, c2s_chat_typing), Transport2),        %user2 typing
    #{<<"msg_type">> := ?S2C_CHAT_TYPING_TYPE, <<"chat_id">> := ChatId, <<"user_msisdn">> := MSISDN2} = receive_packet(ConPid1, Transport1), %user1 still receive typing msg
    send_packet(ConPid2, ?R2M(#c2s_chat_unmute{chat_id = ChatId}, c2s_chat_unmute), Transport2),        %user2 unmute chat
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"is_muted">> := 'false'} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"is_muted">> := 'false'} = get_chat_info(ConPid2, Transport2, ChatId),
    send_packet(ConPid1, ?R2M(#c2s_chat_typing{chat_id = ChatId}, c2s_chat_typing), Transport1),        %user1 typing
    #{<<"msg_type">> := ?S2C_CHAT_TYPING_TYPE, <<"chat_id">> := ChatId, <<"user_msisdn">> := MSISDN1} = receive_packet(ConPid2, Transport2), %user2 receive typing msg
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.

message_update_status_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id, invite User2
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    tester:flush_messages(),
    %% Start chating
    {MsgId1, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN1, <<"msg_body">> := <<"Hello Joe?">>, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId1, <<"status">> := <<"pending">>} = receive_packet(ConPid2, Transport2),
    {MsgId2, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"Hello Mike!">>, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId2, <<"status">> := <<"pending">>} = receive_packet(ConPid1, Transport1),
    %% User1 update msg1 and msg2 statuses
    send_packet(ConPid1, ?R2M(#c2s_message_update_status{chat_id = ChatId, msg_id = [MsgId1, MsgId2]}, c2s_message_update_status), Transport1),
    %% Both users receive messages about msg1 and msg2 update status, order not defined
    #{<<"msg_type">> := ?S2C_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId3} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId4} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId5} = receive_packet(ConPid2, Transport2),
    #{<<"msg_type">> := ?S2C_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId6} = receive_packet(ConPid2, Transport2),
    true = (MsgId3 == MsgId1) or (MsgId3 == MsgId2),
    true = (MsgId4 == MsgId1) or (MsgId4 == MsgId2),
    true = (MsgId5 == MsgId1) or (MsgId5 == MsgId2),
    true = (MsgId6 == MsgId1) or (MsgId6 == MsgId2),
    %% no other messages
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    %% User2 update msg1 only status
    send_packet(ConPid2, ?R2M(#c2s_message_update_status{chat_id = ChatId, msg_id = [MsgId1]}, c2s_message_update_status), Transport2),
    %% receive msg about it
    #{<<"msg_type">> := ?S2C_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId1} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_UPDATE_STATUS_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId1} = receive_packet(ConPid2, Transport2),
    %% get messages list, check statuses
    send_packet(ConPid1, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = 0, count = 30, direction = down}, c2s_message_get_list), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := [#{<<"from">> := MSISDN2, <<"msg_body">> := <<"@system:invite_to_chat">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_body">> := <<"@system:accept_invatation">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := MsgId1, <<"msg_body">> := <<"Hello Joe?">>, <<"status">> := <<"read">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := MsgId2, <<"msg_body">> := <<"Hello Mike!">>, <<"status">> := <<"delivered">>}]} = receive_packet(ConPid1, Transport1),
    ok.

message_update_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id, invite User2
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    timer:sleep(50),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    tester:flush_messages(),
    %% Start chating
    {MsgId1, ChatId} = send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN1, <<"msg_body">> := <<"Hello Joe?">>, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId1, <<"status">> := <<"pending">>} = receive_packet(ConPid2, Transport2),
    {MsgId2, ChatId} = send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"from">> := MSISDN2, <<"msg_body">> := <<"Hello Mike!">>, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId2, <<"status">> := <<"pending">>} = receive_packet(ConPid1, Transport1),
    %% User1 update msg1
    send_packet(ConPid1, ?R2M(#c2s_message_update{chat_id = ChatId, msg_body = <<"Hello world!">>, msg_id = MsgId1}, c2s_message_update), Transport1),
    %% User2 receive message about it
    #{<<"msg_type">> := ?S2C_MESSAGE_UPDATE_TYPE, <<"chat_id">> := ChatId, <<"msg_id">> := MsgId1, <<"msg_body">> := <<"Hello world!">>} = receive_packet(ConPid2, Transport2),
    %% get messages list
    send_packet(ConPid2, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = 0, count = 30, direction = down}, c2s_message_get_list), Transport2),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := [#{<<"from">> := MSISDN2, <<"msg_body">> := <<"@system:invite_to_chat">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_body">> := <<"@system:accept_invatation">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := MsgId1, <<"msg_body">> := <<"Hello world!">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := MsgId2, <<"msg_body">> := <<"Hello Mike!">>}]} = receive_packet(ConPid2, Transport2),
    %% User1 try to update msg2
    send_packet(ConPid1, ?R2M(#c2s_message_update{chat_id = ChatId, msg_body = <<"Hello world!">>, msg_id = MsgId2}, c2s_message_update), Transport1),
    %% User1 receive error
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid1, Transport1),
    %% User2 receive nothing
    {error, timeout} = receive_packet(ConPid2, Transport2),
    %% User1 try to update msg in unexisting chat
    send_packet(ConPid1, ?R2M(#c2s_message_update{chat_id = <<"SOME_UNEXISTING_CHAT">>, msg_body = <<"Hello world!">>, msg_id = MsgId2}, c2s_message_update), Transport1),
    %% User1 receive error
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid1, Transport1),
    %% User1 try to update msg with unexisting msg_id
    send_packet(ConPid1, ?R2M(#c2s_message_update{chat_id = ChatId, msg_body = <<"Hello world!">>, msg_id = 999}, c2s_message_update), Transport1),
    %% User1 receive error
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.

%%--------------------------------------------------------------------
%%      USERS
%%--------------------------------------------------------------------
user_get_status_test(Config)->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid})->
                          %% Register new user
                          MSISDN = rand:uniform(89999999) + 1000000,
                          _User = users:new(MSISDN, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                          timer:sleep(50),
                          %% Get new user status
                          send_packet(ConPid, ?R2M(#c2s_user_get_status{user_msisdn = MSISDN}, c2s_user_get_status), Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN, <<"status">> := <<"offline">>} = receive_packet(ConPid, Transport),
                          {ok, Token} = authorize(MSISDN),
                          {ok, ConPid2} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport),
                          timer:sleep(50),
                          send_packet(ConPid, ?R2M(#c2s_user_get_status{user_msisdn = MSISDN}, c2s_user_get_status), Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN, <<"status">> := <<"online">>} = receive_packet(ConPid, Transport),
                          gun:close(ConPid2),
                          TimeStamp1 = common:timestamp(),
                          timer:sleep(2000),
                          send_packet(ConPid, ?R2M(#c2s_user_get_status{user_msisdn = MSISDN}, c2s_user_get_status), Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN, <<"status">> := <<"offline">>, <<"last_visit_timestamp">> := TimeStamp2} = receive_packet(ConPid, Transport),
                          true = (TimeStamp2 - TimeStamp1) < 1000
                  end, Env).

user_subscribe_unsubscribe_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid})->
                          %% Register new user
                          MSISDN1 = rand:uniform(89999999) + 1000000,
                          _User1 = users:new(MSISDN1, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                          MSISDN2 = rand:uniform(89999999) + 1000000,
                          _User2 = users:new(MSISDN2, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                          timer:sleep(50),
                          %% subscribe
                          send_packet(ConPid, ?R2M(#c2s_user_subscribe{msisdn = [MSISDN1, MSISDN2]}, c2s_user_subscribe), Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN1, <<"status">> := <<"offline">>} = receive_packet(ConPid, Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN2, <<"status">> := <<"offline">>} = receive_packet(ConPid, Transport),
                          {ok, Token1} = authorize(MSISDN1),
                          {ok, ConPid2} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token1) ++ "/ws/v1/chat", Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN1, <<"status">> := <<"online">>} = receive_packet(ConPid, Transport),
                          {ok, Token2} = authorize(MSISDN2),
                          {ok, ConPid3} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token2) ++ "/ws/v1/chat", Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN2, <<"status">> := <<"online">>} = receive_packet(ConPid, Transport),
                          timer:sleep(50),
                          gun:close(ConPid2),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN1, <<"status">> := <<"offline">>} = receive_packet(ConPid, Transport),
                          {ok, ConPid4} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token1) ++ "/ws/v1/chat", Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"msisdn">> := MSISDN1, <<"status">> := <<"online">>} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_user_unsubscribe{msisdn = [MSISDN1, MSISDN2]}, c2s_user_unsubscribe), Transport),
                          timer:sleep(1000),
                          gun:close(ConPid3),
                          gun:close(ConPid4),
                          timer:sleep(2000),
                          {error, timeout} = receive_packet(ConPid, Transport)
                  end, Env).

user_get_info_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid, user := User})->
                          FName = users:extract(User, fname),
                          LName = users:extract(User, lname),
                          Age = users:extract(User, age),
                          IsMale = users:extract(User, is_male),
                          MSISDN = users:extract(User, msisdn),
                          send_packet(ConPid, ?R2M(#c2s_user_get_info{user_msisdn = MSISDN}, c2s_user_get_info), Transport),
                          #{<<"msg_type">> := ?S2C_USER_INFO_TYPE, <<"user_msisdn">> := MSISDN, <<"fname">> := FName , <<"lname">> := LName, <<"age">> := Age, <<"is_male">> := IsMale} = receive_packet(ConPid, Transport)
                  end, Env),
    ok.

user_get_info_bulk_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2} | _] = proplists:get_value(env, Config),
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    FName1 = users:extract(User1, fname),
    FName2 = users:extract(User2, fname),
    LName1 = users:extract(User1, lname),
    LName2 = users:extract(User2, lname),
    Age1 = users:extract(User1, age),
    Age2 = users:extract(User2, age),
    IsMale1 = users:extract(User1, is_male),
    IsMale2 = users:extract(User2, is_male),
    %% Get info about both users
    send_packet(ConPid1, ?R2M(#c2s_user_get_info_bulk{msisdns = [MSISDN1, MSISDN2]}, c2s_user_get_info_bulk), Transport1),
    #{<<"msg_type">> := ?S2C_USER_INFO_BULK_TYPE, <<"users">> := Users} = receive_packet(ConPid1, Transport1),
    [#{<<"user_msisdn">> := MSISDN1, <<"fname">> := FName1, <<"lname">> := LName1, <<"age">> := Age1, <<"is_male">> := IsMale1}
    ,#{<<"user_msisdn">> := MSISDN2, <<"fname">> := FName2, <<"lname">> := LName2, <<"age">> := Age2, <<"is_male">> := IsMale2}] = Users,
    ok.

user_set_info_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid, user := User})->
                          MSISDN = users:extract(User, msisdn),
                          send_packet(ConPid, ?R2M(#c2s_user_set_info{fname = <<"Joe">>, lname = <<"Armstrong">>, is_male = 'true', age = 70}, c2s_user_set_info), Transport),
                          send_packet(ConPid, ?R2M(#c2s_user_get_info{user_msisdn = MSISDN}, c2s_user_get_info), Transport),
                          #{<<"msg_type">> := ?S2C_USER_INFO_TYPE, <<"user_msisdn">> := MSISDN, <<"fname">> := <<"Joe">> , <<"lname">> := <<"Armstrong">>, <<"age">> := 70, <<"is_male">> := 'true'} = receive_packet(ConPid, Transport)
                  end, Env),
    ok.

user_search_test(Config) ->
    [#{user := User1, transport := Transport, connection := ConPid}
    ,#{user := User2} | _] = proplists:get_value(env, Config),
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    send_packet(ConPid, ?R2M(#c2s_user_search{fname = <<"Nik">>, lname = <<>>}, c2s_user_search), Transport),
    #{<<"msg_type">> := ?S2C_USER_SEARCH_RESULT_TYPE, <<"users">> := Users1} = receive_packet(ConPid, Transport),
    'true' = lists:member(MSISDN1, Users1) and lists:member(MSISDN2, Users1),
    send_packet(ConPid, ?R2M(#c2s_user_search{fname = <<>>, lname = <<"Voron">>}, c2s_user_search), Transport),
    #{<<"msg_type">> := ?S2C_USER_SEARCH_RESULT_TYPE, <<"users">> := Users2} = receive_packet(ConPid, Transport),
    'true' = lists:member(MSISDN1, Users2) and lists:member(MSISDN2, Users2),
    send_packet(ConPid,#{<<"msg_type">> => ?C2S_USER_SEARCH_TYPE, <<"lname">> => <<"Vor">>, <<"fname">> => <<"Nik">>}, Transport),
    #{<<"msg_type">> := ?S2C_USER_SEARCH_RESULT_TYPE, <<"users">> := Users3} = receive_packet(ConPid, Transport),
    'true' = lists:member(MSISDN1, Users3) and lists:member(MSISDN2, Users3),
    send_packet(ConPid,#{<<"msg_type">> => ?C2S_USER_SEARCH_TYPE, <<"lname">> => <<"V">>, <<"fname">> => <<"N">>}, Transport),
    #{<<"msg_type">> := ?S2C_USER_SEARCH_RESULT_TYPE, <<"users">> := []} = receive_packet(ConPid, Transport),
    ok.

%%--------------------------------------------------------------------
%%      ROOMS
%%--------------------------------------------------------------------
room_create_test(Config) ->
    [#{transport := Transport1, connection := ConPid1}
    ,#{transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    %% MSISDN1 = users:extract(User1, msisdn),
    %% MSISDN2 = users:extract(User2, msisdn),
    send_packet(ConPid1, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport1),
    timer:sleep(100),
    Room = #c2s_room_create{name = <<"My Room">>, description = <<"My Own Room">>, room_access = #{}, chat_access = #{}, tags = #{<<"tag1">> => 'true'}},
    send_packet(ConPid1, ?R2M(Room, c2s_room_create), Transport1),      %create as company
    send_packet(ConPid2, ?R2M(Room, c2s_room_create), Transport2),      %create as user
    #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := _} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid2, Transport2),
    ok.

room_get_info_test(Config) ->
    [#{transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    send_packet(ConPid1, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport1),
    timer:sleep(100),
    MSISDN2 = users:extract(User2, msisdn),
    NewMSISDNs = [MSISDN3, MSISDN4, MSISDN5, MSISDN6] = [MSISDN || MSISDN <- lists:seq(100000, 100003)],
    RoomAccess = #{MSISDN2 => 0, MSISDN3 => 7, <<"default">> => 1},
    ChatAccess = #{MSISDN4 => 7, MSISDN5 => 0, <<"default">> => 3},
    Room = #c2s_room_create{name= <<"My Room">>
                           ,description= <<"My Own Room">>
                           ,room_access= RoomAccess
                           ,chat_access= ChatAccess
                           ,tags= #{<<"tag1">> => 'true'}},
    send_packet(ConPid1, ?R2M(Room, c2s_room_create), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := RoomId} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport2),
    [{MSISDN3, ConPid3}
    ,{MSISDN4, ConPid4}
    ,{MSISDN5, ConPid5}
    ,{MSISDN6, ConPid6}] = [begin
                                users:new(MSISDN, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                                timer:sleep(100),
                                {ok, Token} = authorize(MSISDN),
                                {ok, ConPid} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport1),
                                send_packet(ConPid, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
                                {MSISDN, ConPid}
                            end || MSISDN <- NewMSISDNs],
    FullRoomInfo = #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
                    ,<<"room_id">> := RoomId
                    ,<<"name">> := <<"My Room">>
                    ,<<"description">> := <<"My Own Room">>
                    ,<<"tags">> := #{<<"tag1">> := 'true'}
                    ,<<"subrooms">> := []
                    ,<<"chat_access">> := ChatAccess
                    ,<<"room_access">> := RoomAccess
                    ,<<"chat_id">> := _} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid2, Transport2),
    FullRoomInfo = receive_packet(ConPid3, Transport1),
    Tmp1 = maps:remove(<<"room_access">>, FullRoomInfo),
    Tmp1 = receive_packet(ConPid4, Transport1),
    Tmp2 = maps:remove(<<"chat_id">>,
                       maps:remove(<<"chat_access">>,
                                   maps:remove(<<"room_access">>, FullRoomInfo))),
    Tmp2 = receive_packet(ConPid5, Transport1),
    Tmp3 = maps:remove(<<"chat_access">>,
                       maps:remove(<<"room_access">>, FullRoomInfo)),
    Tmp3 = receive_packet(ConPid6, Transport1),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = 123}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid1, Transport1),
    ok.

room_set_info_test(Config) ->
    [#{transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    send_packet(ConPid1, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport1),
    timer:sleep(100),
    MSISDN2 = users:extract(User2, msisdn),
    NewMSISDNs = [MSISDN3, MSISDN4, MSISDN5, MSISDN6] = [MSISDN || MSISDN <- lists:seq(100000, 100003)],
    RoomAccess = #{MSISDN2 => 0, MSISDN3 => 7, <<"default">> => 1},
    ChatAccess = #{MSISDN4 => 7, MSISDN5 => 0, <<"default">> => 3},
    Room = #c2s_room_create{name= <<"My Room">>
                           ,description= <<"My Own Room">>
                           ,room_access= RoomAccess
                           ,chat_access= ChatAccess
                           ,tags= #{<<"tag1">> => 'true'}},
    send_packet(ConPid1, ?R2M(Room, c2s_room_create), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := RoomId} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
    [{MSISDN3, ConPid3}
    ,{MSISDN4, ConPid4}
    ,{MSISDN5, ConPid5}
    ,{MSISDN6, ConPid6}] = [begin
                                users:new(MSISDN, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                                timer:sleep(100),
                                {ok, Token} = authorize(MSISDN),
                                {ok, ConPid} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport1),
                                {MSISDN, ConPid}
                            end || MSISDN <- NewMSISDNs],
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId
     ,<<"name">> := <<"My Room">>
     ,<<"description">> := <<"My Own Room">>
     ,<<"tags">> := #{<<"tag1">> := 'true'}
     ,<<"subrooms">> := []
     ,<<"chat_access">> := ChatAccess
     ,<<"room_access">> := RoomAccess
     ,<<"chat_id">> := _} = receive_packet(ConPid1, Transport1),
    RoomAccess1 = RoomAccess#{123 => 0},
    ChatAccess1 = ChatAccess#{123 => 0},
    send_packet(ConPid1, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"tags">> => #{<<"tag2">> => 'true'}
                          ,<<"room_access">> => RoomAccess1
                          ,<<"chat_access">> => ChatAccess1
                          ,<<"name">> => <<"Name2">>
                          ,<<"description">> => <<"Desc2">>
                          ,<<"room_id">> => RoomId}, Transport1),
    timer:sleep(200),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId
     ,<<"name">> := <<"Name2">>
     ,<<"description">> := <<"Desc2">>
     ,<<"tags">> := #{<<"tag2">> := 'true'}
     ,<<"subrooms">> := []
     ,<<"chat_access">> := ChatAccess1
     ,<<"room_access">> := RoomAccess1
     ,<<"chat_id">> := _} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid2, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"name">> => <<"Name3">>
                          ,<<"room_id">> => RoomId}, Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid2, Transport2),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"name">> := <<"Name2">>} = receive_packet(ConPid1, Transport1),
    RoomAccess2 = maps:remove(123, RoomAccess1),
    ChatAccess2 = maps:remove(123, ChatAccess1),
    send_packet(ConPid3, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"tags">> => #{<<"tag3">> => 'true'}
                          ,<<"room_access">> => RoomAccess2
                          ,<<"chat_access">> => ChatAccess2
                          ,<<"name">> => <<"Name3">>
                          ,<<"description">> => <<"Desc3">>
                          ,<<"room_id">> => RoomId}, Transport1),
    timer:sleep(200),
    send_packet(ConPid3, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId
     ,<<"name">> := <<"Name3">>
     ,<<"description">> := <<"Desc3">>
     ,<<"tags">> := #{<<"tag3">> := 'true'}
     ,<<"subrooms">> := []
     ,<<"chat_access">> := ChatAccess2
     ,<<"room_access">> := RoomAccess2
     ,<<"chat_id">> := _} = receive_packet(ConPid3, Transport1),
    RoomAccess3 = RoomAccess#{321 => 0},
    ChatAccess3 = ChatAccess#{321 => 0},
    send_packet(ConPid4, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"tags">> => #{<<"tag4">> => 'true'}
                          ,<<"room_access">> => RoomAccess3
                          ,<<"chat_access">> => ChatAccess3
                          ,<<"name">> => <<"Name4">>
                          ,<<"description">> => <<"Desc4">>
                          ,<<"room_id">> => RoomId}, Transport1),
    timer:sleep(200),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId
     ,<<"name">> := <<"Name3">>
     ,<<"description">> := <<"Desc3">>
     ,<<"tags">> := #{<<"tag3">> := 'true'}
     ,<<"subrooms">> := []
     ,<<"chat_access">> := ChatAccess3
     ,<<"room_access">> := RoomAccess2
     ,<<"chat_id">> := _} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid5, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"name">> => <<"Name5">>
                          ,<<"room_id">> => RoomId}, Transport1),
    send_packet(ConPid6, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"name">> => <<"Name6">>
                          ,<<"room_id">> => RoomId}, Transport1),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid5, Transport1),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid6, Transport1),
    send_packet(ConPid6, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"name">> => <<"Name7">>
                          ,<<"room_id">> => 123}, Transport1),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid6, Transport1),
    ok.

room_delete_test(Config) ->
    [#{transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    send_packet(ConPid1, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport1),
    timer:sleep(100),
    MSISDN2 = users:extract(User2, msisdn),
    RoomAccess = #{MSISDN2 => 7, <<"default">> => 1},
    ChatAccess = #{MSISDN2 => 7, <<"default">> => 3},
    Room = #c2s_room_create{name= <<"My Room">>
                           ,description= <<"My Own Room">>
                           ,room_access= RoomAccess
                           ,chat_access= ChatAccess
                           ,tags= #{<<"tag1">> => 'true'}},
    send_packet(ConPid1, ?R2M(Room, c2s_room_create), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := RoomId} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid2, ?R2M(#c2s_room_delete{room_id = RoomId}, c2s_room_delete), Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid2, Transport2),
    send_packet(ConPid2, ?R2M(#c2s_room_delete{room_id = 123}, c2s_room_delete), Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid2, Transport2),
    send_packet(ConPid1, ?R2M(#c2s_room_delete{room_id = RoomId}, c2s_room_delete), Transport1),
    timer:sleep(100),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid1, Transport1),
    ok.

room_add_del_subroom_test(Config)->
    [#{transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    send_packet(ConPid1, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport2),
    timer:sleep(100),
    MSISDN2 = users:extract(User2, msisdn),
    Room = #c2s_room_create{name= <<"Room1">>
                           ,description= <<"">>
                           ,room_access= #{}
                           ,chat_access= #{}
                           ,tags= #{}},
    send_packet(ConPid1, ?R2M(Room, c2s_room_create), Transport1),
    send_packet(ConPid2, ?R2M(Room#c2s_room_create{name = <<"Room2">>}, c2s_room_create), Transport2),
    #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := RoomId1} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := RoomId2} = receive_packet(ConPid2, Transport2),
    send_packet(ConPid2, ?R2M(#c2s_room_add_subroom{room_id = RoomId1, subroom_id = RoomId2}, c2s_room_add_subroom), Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid2, Transport2),
    send_packet(ConPid1, ?R2M(#c2s_room_add_subroom{room_id = RoomId1, subroom_id = RoomId2}, c2s_room_add_subroom), Transport1),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId1}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId1
     ,<<"name">> := <<"Room1">>
     ,<<"subrooms">> := [RoomId2]} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid1, ?R2M(#c2s_room_del_subroom{room_id = RoomId1, subroom_id = RoomId2}, c2s_room_add_subroom), Transport1),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId1}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId1
     ,<<"name">> := <<"Room1">>
     ,<<"subrooms">> := []} = receive_packet(ConPid1, Transport1),
    RoomAccess = #{MSISDN2 => 7},
    send_packet(ConPid1, #{<<"msg_type">> => ?C2S_ROOM_SET_INFO_TYPE
                          ,<<"room_access">> => RoomAccess
                          ,<<"room_id">> => RoomId1}, Transport1),
    timer:sleep(100),
    send_packet(ConPid2, ?R2M(#c2s_room_add_subroom{room_id = RoomId1, subroom_id = RoomId2}, c2s_room_add_subroom), Transport2),
    timer:sleep(100),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId1}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId1
     ,<<"name">> := <<"Room1">>
     ,<<"subrooms">> := [RoomId2]} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid2, ?R2M(#c2s_room_del_subroom{room_id = RoomId1, subroom_id = RoomId2}, c2s_room_add_subroom), Transport2),
    timer:sleep(100),
    send_packet(ConPid1, ?R2M(#c2s_room_get_info{room_id = RoomId1}, c2s_room_get_info), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_INFO_TYPE
     ,<<"room_id">> := RoomId1
     ,<<"name">> := <<"Room1">>
     ,<<"subrooms">> := []} = receive_packet(ConPid1, Transport1),
    ok.

room_join_to_chat_test(Config) ->
    [#{transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    send_packet(ConPid1, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport1),
    timer:sleep(100),
    MSISDN2 = users:extract(User2, msisdn),
    NewMSISDNs = [MSISDN3, MSISDN4, MSISDN5, MSISDN6, MSISDN7] = [MSISDN || MSISDN <- lists:seq(100000, 100004)],
    RoomAccess = #{MSISDN2 => 0, MSISDN3 => 7, <<"default">> => 1},
    ChatAccess = #{MSISDN4 => 7, MSISDN5 => 0, MSISDN7 => 1, <<"default">> => 3},
    Room = #c2s_room_create{name= <<"My Room">>
                           ,description= <<"My Own Room">>
                           ,room_access= RoomAccess
                           ,chat_access= ChatAccess
                           ,tags= #{<<"tag1">> => 'true'}},
    send_packet(ConPid1, ?R2M(Room, c2s_room_create), Transport1),
    #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := RoomId} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid1, ?R2M(#c2s_room_join_to_chat{room_id = RoomId}, c2s_room_join_to_chat), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_room_join_to_chat{room_id = RoomId}, c2s_room_join_to_chat), Transport2),
    [{MSISDN3, ConPid3}
    ,{MSISDN4, ConPid4}
    ,{MSISDN5, ConPid5}
    ,{MSISDN6, ConPid6}
    ,{MSISDN7, ConPid7}] = [begin
                                users:new(MSISDN, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                                timer:sleep(100),
                                {ok, Token} = authorize(MSISDN),
                                {ok, ConPid} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport1),
                                send_packet(ConPid, ?R2M(#c2s_room_join_to_chat{room_id = RoomId}, c2s_room_join_to_chat), Transport1),
                                {MSISDN, ConPid}
                            end || MSISDN <- NewMSISDNs],
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid2, Transport2),
    #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 403} = receive_packet(ConPid5, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId, <<"access_level">> := 7} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId, <<"access_level">> := 3} = receive_packet(ConPid3, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId, <<"access_level">> := 7} = receive_packet(ConPid4, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId, <<"access_level">> := 3} = receive_packet(ConPid6, Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId, <<"access_level">> := 1} = receive_packet(ConPid7, Transport1),
    send_packet(ConPid1, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_accept_invatation), Transport1),
    send_packet(ConPid3, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_accept_invatation), Transport1),
    send_packet(ConPid4, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_accept_invatation), Transport1),
    send_packet(ConPid6, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_accept_invatation), Transport1),
    send_packet(ConPid7, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_accept_invatation), Transport1),
    timer:sleep(100),
    tester:flush_messages(),
    send_message(ConPid1, Transport1, ChatId, <<"Msg1">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg1">>, <<"chat_id">> := ChatId} = receive_packet(ConPid3, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg1">>, <<"chat_id">> := ChatId} = receive_packet(ConPid4, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg1">>, <<"chat_id">> := ChatId} = receive_packet(ConPid6, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg1">>, <<"chat_id">> := ChatId} = receive_packet(ConPid7, Transport1),
    send_message(ConPid3, Transport1, ChatId, <<"Msg2">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg2">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg2">>, <<"chat_id">> := ChatId} = receive_packet(ConPid4, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg2">>, <<"chat_id">> := ChatId} = receive_packet(ConPid6, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg2">>, <<"chat_id">> := ChatId} = receive_packet(ConPid7, Transport1),
    send_message(ConPid4, Transport1, ChatId, <<"Msg3">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg3">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg3">>, <<"chat_id">> := ChatId} = receive_packet(ConPid3, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg3">>, <<"chat_id">> := ChatId} = receive_packet(ConPid6, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg3">>, <<"chat_id">> := ChatId} = receive_packet(ConPid7, Transport1),
    send_message(ConPid6, Transport1, ChatId, <<"Msg4">>),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg4">>, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg4">>, <<"chat_id">> := ChatId} = receive_packet(ConPid3, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg4">>, <<"chat_id">> := ChatId} = receive_packet(ConPid4, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Msg4">>, <<"chat_id">> := ChatId} = receive_packet(ConPid7, Transport1),
    send_message(ConPid7, Transport1, ChatId, <<"Msg5">>),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid3, Transport1),
    {error, timeout} = receive_packet(ConPid4, Transport1),
    {error, timeout} = receive_packet(ConPid6, Transport1),
    ok.

room_send_recursive_message_test(Config) ->
    [#{transport := Transport1}| _] = proplists:get_value(env, Config),
    Room = #c2s_room_create{name= <<"My Room">>
                           ,description= <<"My Own Room">>
                           ,room_access= #{'default' => 3}
                           ,chat_access= #{'default' => 3, 100002 => 0}
                           ,tags= #{}},
    [{_MSISDN1, ConPid1, RoomId1}
    ,{_MSISDN2, ConPid2, RoomId2}
    ,{_MSISDN3, ConPid3, RoomId3}
    ,{_MSISDN4, ConPid4, RoomId4}
    ,{_MSISDN5, ConPid5, RoomId5}
    ,{_MSISDN6, ConPid6, RoomId6}
    ,{_MSISDN7, ConPid7, RoomId7}] = [begin
                                         users:new(MSISDN, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                                         timer:sleep(100),
                                         {ok, Token} = authorize(MSISDN),
                                         {ok, ConPid} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport1),
                                         send_packet(ConPid, ?R2M(#c2s_user_upgrade_to_company{}, c2s_user_upgrade_to_company), Transport1),
                                         timer:sleep(50),
                                         send_packet(ConPid, ?R2M(Room, c2s_room_create), Transport1),
                                         #{<<"msg_type">> := ?S2C_ROOM_CREATE_RESULT_TYPE, <<"room_id">> := RoomId} = receive_packet(ConPid, Transport1),
                                         timer:sleep(50),
                                         send_packet(ConPid, ?R2M(#c2s_room_join_to_chat{room_id = RoomId}, c2s_room_join_to_chat), Transport1),
                                         #{<<"msg_type">> := ?S2C_CHAT_INVATATION_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid, Transport1),
                                         timer:sleep(50),
                                         send_packet(ConPid, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_accept_invatation), Transport1),
                                         {MSISDN, ConPid, RoomId}
                                     end || MSISDN <- lists:seq(100000, 100006)],
    send_packet(ConPid1, ?R2M(#c2s_room_add_subroom{room_id = RoomId1, subroom_id = RoomId2}, c2s_room_add_subroom), Transport1),
    timer:sleep(50),
    send_packet(ConPid1, ?R2M(#c2s_room_add_subroom{room_id = RoomId1, subroom_id = RoomId3}, c2s_room_add_subroom), Transport1),
    timer:sleep(50),
    send_packet(ConPid2, ?R2M(#c2s_room_add_subroom{room_id = RoomId2, subroom_id = RoomId3}, c2s_room_add_subroom), Transport1), %test cyclic
    timer:sleep(50),
    send_packet(ConPid3, ?R2M(#c2s_room_add_subroom{room_id = RoomId3, subroom_id = RoomId4}, c2s_room_add_subroom), Transport1),
    timer:sleep(50),
    send_packet(ConPid4, ?R2M(#c2s_room_add_subroom{room_id = RoomId4, subroom_id = RoomId5}, c2s_room_add_subroom), Transport1),
    timer:sleep(50),
    send_packet(ConPid5, ?R2M(#c2s_room_add_subroom{room_id = RoomId5, subroom_id = RoomId6}, c2s_room_add_subroom), Transport1),
    timer:sleep(50),
    send_packet(ConPid5, ?R2M(#c2s_room_add_subroom{room_id = RoomId5, subroom_id = RoomId7}, c2s_room_add_subroom), Transport1),
    timer:sleep(50),
    tester:flush_messages(),
    send_packet(ConPid1, ?R2M(#c2s_room_send_recursive_message{room_id = RoomId1, msg = <<"Test">>}, c2s_room_send_recursive_message), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Test">>} = receive_packet(ConPid2, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Test">>} = receive_packet(ConPid3, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Test">>} = receive_packet(ConPid4, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Test">>} = receive_packet(ConPid5, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Test">>} = receive_packet(ConPid6, Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_TYPE, <<"msg_body">> := <<"Test">>} = receive_packet(ConPid7, Transport1),
    send_packet(ConPid3, ?R2M(#c2s_room_send_recursive_message{room_id = RoomId3, msg = <<"Test2">>}, c2s_room_send_recursive_message), Transport1),
    {error, timeout} = receive_packet(ConPid4, Transport1),
    {error, timeout} = receive_packet(ConPid5, Transport1),
    {error, timeout} = receive_packet(ConPid6, Transport1),
    {error, timeout} = receive_packet(ConPid7, Transport1),
    ok.

%%--------------------------------------------------------------------
%%      CALLS
%%--------------------------------------------------------------------
call_normal_answer_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% User1 and User2 locks turn servers
    send_packet(ConPid1, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport2),
    timer:sleep(100),
    #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid2, Transport2),
    %% User1 call to User2
    send_packet(ConPid1, ?R2M(#c2s_call_offer{msisdn = MSISDN2, sdp = <<"sdp1">>}, c2s_call_offer), Transport1),
    #{<<"msg_type">> := ?S2C_CALL_OFFER_TYPE, <<"msisdn">> := MSISDN1, <<"sdp">> := <<"sdp1">>, <<"turn_server">> := _} = receive_packet(ConPid2, Transport2),
    %% User2 send 'ack', then answer
    send_packet(ConPid2, ?R2M(#c2s_call_ack{}, c2s_call_ack), Transport2),
    #{<<"msg_type">> := ?S2C_CALL_ACK_TYPE} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid2, ?R2M(#c2s_call_answer{sdp = <<"sdp2">>}, c2s_call_answer), Transport2),
    #{<<"msg_type">> := ?S2C_CALL_ANSWER_TYPE, <<"sdp">> := <<"sdp2">>} = receive_packet(ConPid1, Transport1),
    %% exchange of ice candidates
    send_packet(ConPid1, ?R2M(#c2s_call_ice_candidate{candidate = <<"candidate1">>}, c2s_call_ice_candidate), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_call_ice_candidate{candidate = <<"candidate2">>}, c2s_call_ice_candidate), Transport2),
    #{<<"msg_type">> := ?S2C_CALL_ICE_CANDIDATE_TYPE, <<"candidate">> := <<"candidate2">>} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_CALL_ICE_CANDIDATE_TYPE, <<"candidate">> := <<"candidate1">>} = receive_packet(ConPid2, Transport2),
    %% User1 hangs up
    send_packet(ConPid1, ?R2M(#c2s_call_bye{code = 200}, c2s_call_bye), Transport1),
    #{<<"msg_type">> := ?S2C_CALL_BYE_TYPE, <<"code">> := 200} = receive_packet(ConPid2, Transport2),
    timer:sleep(100),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.

call_reject_call_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% User1 and User2 locks turn servers
    send_packet(ConPid1, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport2),
    timer:sleep(100),
    #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid2, Transport2),
    %% User1 call to User2
    send_packet(ConPid1, ?R2M(#c2s_call_offer{msisdn = MSISDN2, sdp = <<"sdp1">>}, c2s_call_offer), Transport1),
    #{<<"msg_type">> := ?S2C_CALL_OFFER_TYPE, <<"msisdn">> := MSISDN1, <<"sdp">> := <<"sdp1">>, <<"turn_server">> := _} = receive_packet(ConPid2, Transport2),
    %% User2 reject call
    send_packet(ConPid2, ?R2M(#c2s_call_bye{code = 200}, c2s_call_bye), Transport2),
    #{<<"msg_type">> := ?S2C_CALL_BYE_TYPE, <<"code">> := 200} = receive_packet(ConPid1, Transport1),
    timer:sleep(200),
    %% User1 recall and it's still working
    send_packet(ConPid1, ?R2M(#c2s_call_offer{msisdn = MSISDN2, sdp = <<"sdp1">>}, c2s_call_offer), Transport1),
    #{<<"msg_type">> := ?S2C_CALL_OFFER_TYPE, <<"msisdn">> := MSISDN1, <<"sdp">> := <<"sdp1">>, <<"turn_server">> := _} = receive_packet(ConPid2, Transport2),
    send_packet(ConPid2, ?R2M(#c2s_call_answer{sdp = <<"sdp2">>}, c2s_call_answer), Transport2),
    #{<<"msg_type">> := ?S2C_CALL_ANSWER_TYPE, <<"sdp">> := <<"sdp2">>} = receive_packet(ConPid1, Transport1),
    timer:sleep(100),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    ok.

call_to_busy_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    MSISDN3 = rand:uniform(89999999) + 1000000,
    Transport3 = Transport1,
    _User = users:new(MSISDN3, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
    timer:sleep(50),
    {ok, Token} = authorize(MSISDN3),
    {ok, ConPid3} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport3),
    timer:sleep(50),
    %% User1 and User2 locks turn servers
    send_packet(ConPid1, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport1),
    send_packet(ConPid2, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport2),
    send_packet(ConPid3, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport3),
    timer:sleep(100),
    #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid1, Transport1),
    #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid2, Transport2),
    #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid3, Transport3),
    %% User1 call to User2
    send_packet(ConPid1, ?R2M(#c2s_call_offer{msisdn = MSISDN2, sdp = <<"sdp1">>}, c2s_call_offer), Transport1),
    #{<<"msg_type">> := ?S2C_CALL_OFFER_TYPE, <<"msisdn">> := MSISDN1, <<"sdp">> := <<"sdp1">>, <<"turn_server">> := _} = receive_packet(ConPid2, Transport2),
    %% User2 send 'ack', then answer
    send_packet(ConPid2, ?R2M(#c2s_call_ack{}, c2s_call_ack), Transport2),
    #{<<"msg_type">> := ?S2C_CALL_ACK_TYPE} = receive_packet(ConPid1, Transport1),
    send_packet(ConPid2, ?R2M(#c2s_call_answer{sdp = <<"sdp2">>}, c2s_call_answer), Transport2),
    #{<<"msg_type">> := ?S2C_CALL_ANSWER_TYPE, <<"sdp">> := <<"sdp2">>} = receive_packet(ConPid1, Transport1),
    timer:sleep(100),
    %% User 3 try to call User1
    send_packet(ConPid3, ?R2M(#c2s_call_offer{msisdn = MSISDN1, sdp = <<"sdp3">>}, c2s_call_offer), Transport3),
    #{<<"msg_type">> := ?S2C_CALL_BYE_TYPE, <<"code">> := 486} = receive_packet(ConPid3, Transport3),
    %% User 3 try to call User2
    send_packet(ConPid3, ?R2M(#c2s_call_offer{msisdn = MSISDN2, sdp = <<"sdp3">>}, c2s_call_offer), Transport3),
    #{<<"msg_type">> := ?S2C_CALL_BYE_TYPE, <<"code">> := 486} = receive_packet(ConPid3, Transport3),
    timer:sleep(100),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    {error, timeout} = receive_packet(ConPid2, Transport2),
    {error, timeout} = receive_packet(ConPid3, Transport3),
    ok.

call_to_bad_msisdn_test(Config) ->
    [#{transport := Transport1, connection := ConPid1} | _] = proplists:get_value(env, Config),
    %% User1 try to call to unexisting MSISDN
    send_packet(ConPid1, ?R2M(#c2s_call_offer{msisdn = 123, sdp = <<"sdp1">>}, c2s_call_offer), Transport1),
    #{<<"msg_type">> := ?S2C_CALL_BYE_TYPE, <<"code">> := 404} = receive_packet(ConPid1, Transport1),
    timer:sleep(100),
    {error, timeout} = receive_packet(ConPid1, Transport1),
    ok.

call_to_offline_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport1, connection := ConPid1, user := User1})->
                          MSISDN1 = users:extract(User1, msisdn),
                          MSISDN2 = rand:uniform(89999999) + 1000000,
                          Transport2 = Transport1,
                          _User = users:new(MSISDN2, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
                          timer:sleep(50),
                          send_packet(ConPid1, ?R2M(#c2s_lock_turn_server{}, c2s_lock_turn_server), Transport1),
                          #{<<"msg_type">> := ?S2C_TURN_SERVER_TYPE} = receive_packet(ConPid1, Transport1),
                          send_packet(ConPid1, ?R2M(#c2s_call_offer{msisdn = MSISDN2, sdp = <<"sdp1">>}, c2s_call_offer), Transport1),
                          #{<<"msg_type">> := ?S2C_CALL_ACK_TYPE} = receive_packet(ConPid1, Transport1),
                          {ok, Token} = authorize(MSISDN2),
                          {ok, ConPid2} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport2),
                          timer:sleep(50),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE,<<"msisdn">> := MSISDN2, <<"status">> := <<"online">>} = receive_packet(ConPid1, Transport1),
                          #{<<"msg_type">> := ?S2C_CALL_OFFER_TYPE, <<"msisdn">> := MSISDN1, <<"sdp">> := <<"sdp1">>, <<"turn_server">> := _} = receive_packet(ConPid2, Transport2),
                          %% User2 reject call
                          send_packet(ConPid2, ?R2M(#c2s_call_bye{code = 200}, c2s_call_bye), Transport2),
                          #{<<"msg_type">> := ?S2C_CALL_BYE_TYPE, <<"code">> := 200} = receive_packet(ConPid1, Transport1),
                          timer:sleep(200),
                          %% User1 recall and it's still working
                          send_packet(ConPid1, ?R2M(#c2s_call_offer{msisdn = MSISDN2, sdp = <<"sdp1">>}, c2s_call_offer), Transport1),
                          #{<<"msg_type">> := ?S2C_CALL_OFFER_TYPE, <<"msisdn">> := MSISDN1, <<"sdp">> := <<"sdp1">>, <<"turn_server">> := _} = receive_packet(ConPid2, Transport2),
                          send_packet(ConPid2, ?R2M(#c2s_call_answer{sdp = <<"sdp2">>}, c2s_call_answer), Transport2),
                          #{<<"msg_type">> := ?S2C_CALL_ANSWER_TYPE, <<"sdp">> := <<"sdp2">>} = receive_packet(ConPid1, Transport1),
                          timer:sleep(100),
                          {error, timeout} = receive_packet(ConPid1, Transport1),
                          {error, timeout} = receive_packet(ConPid2, Transport2),
                          ok
                  end, Env).

storage_test(Config) ->
    Env = proplists:get_value(env, Config),
    lists:foreach(fun(#{transport := Transport, connection := ConPid, user := #user{msisdn = MSISDN}})->
                          {ok, StorageCapacity} = application:get_env('server', 'user_storage_capacity'),
                          send_packet(ConPid, ?R2M(#c2s_storage_keys{}, c2s_storage_keys), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_KEYS_TYPE, <<"keys">> := []} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_capacity{}, c2s_storage_capacity), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_CAPACITY_TYPE, <<"used">> := 0, <<"max">> := StorageCapacity} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_set{key = <<"key1">>, value = <<"value1">>}, c2s_storage_set), Transport),
                          timer:sleep(50),
                          send_packet(ConPid, ?R2M(#c2s_storage_get{key = <<"key1">>}, c2s_storage_get), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_GET_RESULT_TYPE, <<"key">> := <<"key1">>,  <<"value">> := <<"value1">>} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_get{key = <<"key2">>}, c2s_storage_get), Transport),
                          #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_set{key = <<"key2">>, value = <<"value2">>}, c2s_storage_set), Transport),
                          timer:sleep(50),
                          send_packet(ConPid, ?R2M(#c2s_storage_get{key = <<"key2">>}, c2s_storage_get), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_GET_RESULT_TYPE, <<"key">> := <<"key2">>,  <<"value">> := <<"value2">>} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_set{key = <<"key1">>, value = <<"value3">>}, c2s_storage_set), Transport),
                          timer:sleep(50),
                          send_packet(ConPid, ?R2M(#c2s_storage_get{key = <<"key1">>}, c2s_storage_get), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_GET_RESULT_TYPE, <<"key">> := <<"key1">>,  <<"value">> := <<"value3">>} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_keys{}, c2s_storage_keys), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_KEYS_TYPE, <<"keys">> := [<<"key1">>, <<"key2">>]} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_capacity{}, c2s_storage_capacity), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_CAPACITY_TYPE, <<"used">> := 2, <<"max">> := StorageCapacity} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_delete{key = <<"key1">>}, c2s_storage_delete), Transport),
                          timer:sleep(50),
                          send_packet(ConPid, ?R2M(#c2s_storage_get{key = <<"key1">>}, c2s_storage_get), Transport),
                          #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_keys{}, c2s_storage_keys), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_KEYS_TYPE, <<"keys">> := [<<"key2">>]} = receive_packet(ConPid, Transport),
                          send_packet(ConPid, ?R2M(#c2s_storage_capacity{}, c2s_storage_capacity), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_CAPACITY_TYPE, <<"used">> := 1, <<"max">> := StorageCapacity} = receive_packet(ConPid, Transport),
                          gun:close(ConPid),
                          timer:sleep(500),
                          {ok, Token} = authorize(MSISDN),
                          {ok, ConPid1} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport),
                          send_packet(ConPid1, ?R2M(#c2s_storage_get{key = <<"key1">>}, c2s_storage_get), Transport),
                          #{<<"msg_type">> := ?S2C_ERROR_TYPE, <<"code">> := 404} = receive_packet(ConPid1, Transport),
                          send_packet(ConPid1, ?R2M(#c2s_storage_get{key = <<"key2">>}, c2s_storage_get), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_GET_RESULT_TYPE, <<"key">> := <<"key2">>,  <<"value">> := <<"value2">>} = receive_packet(ConPid1, Transport),
                          send_packet(ConPid1, ?R2M(#c2s_storage_keys{}, c2s_storage_keys), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_KEYS_TYPE, <<"keys">> := [<<"key2">>]} = receive_packet(ConPid1, Transport),
                          send_packet(ConPid1, ?R2M(#c2s_storage_capacity{}, c2s_storage_capacity), Transport),
                          #{<<"msg_type">> := ?S2C_STORAGE_CAPACITY_TYPE, <<"used">> := 1, <<"max">> := StorageCapacity} = receive_packet(ConPid1, Transport)
                  end, Env).
%%%===================================================================
%%% Internal functions
%%%===================================================================
init()->
    lists:map(fun(Transport)->
                      MSISDN = rand:uniform(89999999) + 1000000,
                      {ok, Token} = authorize(MSISDN),
                      User = users:set_info(MSISDN, [{group, administrators}, {age, 25}, {fname, <<"Nikita">>}, {lname, <<"Vorontsov">>}, {is_male, true}]),
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

get_chats_list(ConnPid, Transport) ->
    send_packet(ConnPid, ?R2M(#c2s_chat_get_list{}, c2s_chat_get_list), Transport),
    #{<<"msg_type">> := ?S2C_CHAT_LIST_TYPE, <<"chats">> := Chats} = receive_packet(ConnPid, Transport),
    Chats.

get_chat_info(ConnPid, Transport, ChatId) ->
    send_packet(ConnPid, ?R2M(#c2s_chat_get_info{chat_id = ChatId}, c2s_chat_get_info), Transport),
    timer:sleep(50),
    receive_packet(ConnPid, Transport).

send_message(ConnPid, Transport, ChatId, MsgBody) ->
    send_packet(ConnPid, ?R2M(#c2s_message_send{chat_id = ChatId, msg_body = MsgBody}, c2s_message_send), Transport),
    timer:sleep(50),
    case receive_packet(ConnPid, Transport) of
        #{<<"msg_type">> := ?S2C_MESSAGE_SEND_RESULT_TYPE, <<"msg_id">> := MsgId, <<"chat_id">> := ChatId} -> {MsgId, ChatId};
        Else -> Else
    end.
