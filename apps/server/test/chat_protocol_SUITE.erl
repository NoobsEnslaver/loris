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
    User1 = users:new(89628323872, <<"121">>, <<"Nikita">>, <<"Vorontsov">>, 25, 'true', 'administrators', 0),
    User2 = users:new(89628323873, <<"121">>, <<"Daniil">>, <<"Vorontsov">>, 18, 'true', 'administrators', 0),
    [{user1, User} | [{user2, User} | Config]].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    User1 = proplists:get_value(user1, Config),
    User2 = proplists:get_value(user2, Config),
    users:delete(User1),
    users:delete(User2),
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
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
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
                 ,chat_get_info_test
                 ,chat_create_test
                 ,chat_leave_test
                 ,chat_delete_test
                 ,chat_invite_user_test
                 ,chat_mute_test
                 ,chat_unmute_test
                 ,chat_typing_test]}
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
    ,{group, messages}
    ,{group, users}
    ,{group, rooms}
    ,{group, system}].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
my_test_case(_Config) ->
    ok.
