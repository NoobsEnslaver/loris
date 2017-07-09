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
                    ,message_get_list_test
                    ,message_update_test
                    ,message_update_status_test
                    ]}
    ,{users, [], [user_get_info_test
                 ,user_get_info_bulk_test
                 ,user_get_status_test
                 ,user_set_info_test
                 %% ,user_search_test
                 ]}
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
    ,{calls, [], [call_normal_answer_test
                 ,call_reject_call_test
                 ,call_to_busy_test
                 ,call_to_bad_msisdn_test
                 ]}
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
    ,{group, messages}
    ,{group, users}
    %% ,{group, rooms}
    %% ,{group, system}
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
                           ,<<"chat_owner">> := MSISDN
                           ,<<"access_group">> := <<"administrators">>} = receive_packet(ConPid, Transport),
                          timer:sleep(100),
                          {error, timeout} = receive_packet(ConPid, Transport)
                  end, Env).

chat_create_p2p_test(Config) ->
    [#{transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"administrators">>} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"undefined">>} = get_chat_info(ConPid2, Transport2, ChatId),
    %% Get my chats list
    #{ChatId := ChatName} = get_chats_list(ConPid1, Transport1),
    #{} = get_chats_list(ConPid2, Transport2),
    %% Send invataton
    send_packet(ConPid1, ?R2M(#c2s_chat_invite_user{chat_id = ChatId, user_msisdn = MSISDN2}, c2s_chat_invite_user), Transport1),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"administrators">>} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"users">>} = get_chat_info(ConPid2, Transport2, ChatId),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"administrators">>} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"undefined">>} = get_chat_info(ConPid2, Transport2, ChatId),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"administrators">>} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"users">>} = get_chat_info(ConPid2, Transport2, ChatId),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"undefined">>} = get_chat_info(ConPid2, Transport2, ChatId),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"administrators">>} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"users">>} = get_chat_info(ConPid2, Transport2, ChatId),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"administrators">>} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := [MSISDN1]
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"undefined">>} = get_chat_info(ConPid2, Transport2, ChatId),
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
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"administrators">>} = get_chat_info(ConPid1, Transport1, ChatId),
    #{<<"msg_type">> := ?S2C_CHAT_INFO_TYPE
     ,<<"chat_id">> := ChatId
     ,<<"name">> := ChatName
     ,<<"users">> := BothUsers
     ,<<"is_muted">> := 'false'
     ,<<"chat_owner">> := MSISDN1
     ,<<"access_group">> := <<"users">>} = get_chat_info(ConPid2, Transport2, ChatId),
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

message_get_list_test(Config) ->
    [#{user := User1, transport := Transport1, connection := ConPid1}
    ,#{user := User2, transport := Transport2, connection := ConPid2} | _] = proplists:get_value(env, Config),
    ChatName = <<"test_chat">>,
    MSISDN1 = users:extract(User1, msisdn),
    MSISDN2 = users:extract(User2, msisdn),
    %% Crete chat, receive chat_id, invite User2
    send_packet(ConPid1, ?R2M(#c2s_chat_create{name = ChatName, users = [MSISDN2]}, c2s_chat_create), Transport1),
    #{<<"msg_type">> := ?S2C_CHAT_CREATE_RESULT_TYPE, <<"chat_id">> := ChatId} = receive_packet(ConPid1, Transport1),
    %% Accept invatation
    send_packet(ConPid2, ?R2M(#c2s_chat_accept_invatation{chat_id = ChatId}, c2s_chat_get_info), Transport2),
    timer:sleep(50),
    tester:flush_messages(),
    %% Start chating
    send_message(ConPid1, Transport1, ChatId, <<"Hello Joe?">>),
    send_message(ConPid2, Transport2, ChatId, <<"Hello Mike!">>),
    send_message(ConPid1, Transport1, ChatId, <<"It's still working?">>),
    send_message(ConPid2, Transport2, ChatId, <<"Yeah, it's fine.">>),
    send_message(ConPid1, Transport1, ChatId, <<"Fine">>),
    send_message(ConPid2, Transport2, ChatId, <<"Yeah, fine">>),
    send_message(ConPid1, Transport1, ChatId, <<"Good  bye">>),
    send_message(ConPid2, Transport2, ChatId, <<"Bye">>),
    timer:sleep(50),
    tester:flush_messages(),
    %% Requests chat history
    send_packet(ConPid1, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = 0}, c2s_message_get_list), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := [#{<<"from">> := MSISDN2, <<"msg_id">> := 1, <<"msg_body">> := <<"@system:invite_to_chat">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 2, <<"msg_body">> := <<"@system:accept_invatation">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := 3, <<"msg_body">> := <<"Hello Joe?">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 4, <<"msg_body">> := <<"Hello Mike!">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := 5, <<"msg_body">> := <<"It's still working?">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 6, <<"msg_body">> := <<"Yeah, it's fine.">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := 7, <<"msg_body">> := <<"Fine">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 8, <<"msg_body">> := <<"Yeah, fine">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := 9, <<"msg_body">> := <<"Good  bye">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 10, <<"msg_body">> := <<"Bye">>}]} = receive_packet(ConPid1, Transport1),
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
    send_packet(ConPid1, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = 0}, c2s_message_get_list), Transport1),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := [#{<<"from">> := MSISDN2, <<"msg_id">> := 1, <<"msg_body">> := <<"@system:invite_to_chat">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 2, <<"msg_body">> := <<"@system:accept_invatation">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := 3, <<"msg_body">> := <<"Hello Joe?">>, <<"status">> := <<"read">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 4, <<"msg_body">> := <<"Hello Mike!">>, <<"status">> := <<"delivered">>}]} = receive_packet(ConPid1, Transport1),
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
    send_packet(ConPid2, ?R2M(#c2s_message_get_list{chat_id = ChatId, msg_id = 0}, c2s_message_get_list), Transport2),
    #{<<"msg_type">> := ?S2C_MESSAGE_LIST_TYPE
     ,<<"messages">> := [#{<<"from">> := MSISDN2, <<"msg_id">> := 1, <<"msg_body">> := <<"@system:invite_to_chat">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 2, <<"msg_body">> := <<"@system:accept_invatation">>}
                        ,#{<<"from">> := MSISDN1, <<"msg_id">> := 3, <<"msg_body">> := <<"Hello world!">>}
                        ,#{<<"from">> := MSISDN2, <<"msg_id">> := 4, <<"msg_body">> := <<"Hello Mike!">>}]} = receive_packet(ConPid2, Transport2),
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
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"user_msisdn">> := MSISDN, <<"is_online">> := 'false', <<"last_visit_timestamp">> := <<"undefined">>} = receive_packet(ConPid, Transport),
                          {ok, Token} = authorize(MSISDN),
                          {ok, ConPid2} = connect_to_ws("/session/" ++ erlang:binary_to_list(Token) ++ "/ws/v1/chat", Transport),
                          timer:sleep(50),
                          TimeStamp1 = common:timestamp(),
                          send_packet(ConPid, ?R2M(#c2s_user_get_status{user_msisdn = MSISDN}, c2s_user_get_status), Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"user_msisdn">> := MSISDN, <<"is_online">> := 'true', <<"last_visit_timestamp">> := TimeStamp2} = receive_packet(ConPid, Transport),
                          true = (TimeStamp2 - TimeStamp1) < 1000,
                          gun:close(ConPid2),
                          TimeStamp3 = common:timestamp(),
                          timer:sleep(2000),
                          send_packet(ConPid, ?R2M(#c2s_user_get_status{user_msisdn = MSISDN}, c2s_user_get_status), Transport),
                          #{<<"msg_type">> := ?S2C_USER_STATUS_TYPE, <<"user_msisdn">> := MSISDN, <<"is_online">> := 'false', <<"last_visit_timestamp">> := TimeStamp4} = receive_packet(ConPid, Transport),
                          true = (TimeStamp4 - TimeStamp3) < 1000
                  end, Env),
    ok.

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
