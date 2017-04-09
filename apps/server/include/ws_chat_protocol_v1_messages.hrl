-ifndef(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL).
-define(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL, 'true').

%% Client-to-Server
-record(c2s_chat_get_list, {}).
-record(c2s_chat_get_info, {chat_id  :: integer()}).
-record(c2s_chat_create, {name :: binary(), user_id :: [integer()]}).
-record(c2s_chat_leave, {}).
-record(c2s_chat_delete, {}).
-record(c2s_chat_invite_user, {}).
-record(c2s_chat_mute, {}).
-record(c2s_chat_unmute, {}).
-record(c2s_chat_typing, {}).
-record(c2s_message_send, {}).
-record(c2s_message_get_list, {}).
-record(c2s_message_update, {}).
-record(c2s_message_update_status, {}).
-record(c2s_system_logout, {}).
-record(c2s_user_get_info, {}).
-record(c2s_user_get_status, {}).
-record(c2s_user_set_info, {}).
-record(c2s_user_search, {}).
-record(c2s_room_get_tree, {}).
-record(c2s_room_get_info, {}).
-record(c2s_room_rename, {}).
-record(c2s_room_add_user, {}).
-record(c2s_room_del_user, {}).
-record(c2s_room_add_subroom, {}).
-record(c2s_room_create, {}).
-record(c2s_room_delete, {}).
-record(c2s_room_enter_to_chat, {}).
-record(c2s_room_send_message, {}).

-type client_msg_type() ::   #c2s_chat_get_list{}
                           | #c2s_chat_get_info{}
                           | #c2s_chat_create{}
                           | #c2s_chat_leave{}
                           | #c2s_chat_delete{}
                           | #c2s_chat_invite_user{}
                           | #c2s_chat_mute{}
                           | #c2s_chat_unmute{}
                           | #c2s_chat_typing{}
                           | #c2s_message_send{}
                           | #c2s_message_get_list{}
                           | #c2s_message_update{}
                           | #c2s_message_update_status{}
                           | #c2s_system_logout{}
                           | #c2s_user_get_info{}
                           | #c2s_user_get_status{}
                           | #c2s_user_set_info{}
                           | #c2s_room_rename{}
                           | #c2s_room_add_user{}
                           | #c2s_room_del_user{}
                           | #c2s_room_add_subroom{}
                           | #c2s_room_create{}
                           | #c2s_room_delete{}
                           | #c2s_room_enter_to_chat{}
                           | #c2s_room_send_message{}.

%% Server-to-Client
-record(s2c_chat_list, {chat_id = [] :: [integer()]}).
-record(s2c_chat_info, {chat_id, name, user_id = [], is_muted, is_writable, chat_owner}).
-record(s2c_chat_create_result, {result_code, chat_id}).
-record(s2c_chat_leave_result, {result_code, chat_id}).
-record(s2c_chat_delete_result, {result_code, chat_id}).
-record(s2c_chat_invite_user_result, {result_code, chat_id, user_id}).
-record(s2c_chat_mute_result, {result_code, chat_id}).
-record(s2c_chat_unmute_result, {result_code, chat_id}).
-record(s2c_chat_typing, {chat_id, user_id}).
-record(s2c_message, {chat_id, msg_body, timestamp, status, msg_id}).
-record(s2c_message_update, {chat_id, msg_id, msg_body}).
-record(s2c_message_update_status, {chat_id, msg_id}).
-record(s2c_user_info, {user_id, fname, lname, age, sex}).
-record(s2c_user_status, {user_id, is_online = 'false', last_visit_timestamp}).
-record(s2c_user_set_info_result, {result_code, fname, lname, age, sex}).
-record(s2c_user_search_result, {user_id = []}).
-record(s2c_room_list, {room_id = []}).
-record(s2c_room_info, {room_id, subroom_id = [], user_id = [], chat_id = []}).
-record(s2c_room_rename_result, {result_code, room_id, name}).
-record(s2c_room_add_user_result, {result_code, room_id, user_id, access_level, is_public = 'false'}).
-record(s2c_room_del_user_result, {result_code, room_id, user_id}).
-record(s2c_room_add_subroom_result, {result_code, room_id, subroom_id}).
-record(s2c_room_create_result, {result_code, room_id, name}).

-type server_msg_type() ::   #s2c_chat_list{}
                           | #s2c_chat_info{}
                           | #s2c_chat_create_result{}
                           | #s2c_chat_leave_result{}
                           | #s2c_chat_delete_result{}
                           | #s2c_chat_invite_user_result{}
                           | #s2c_chat_mute_result{}
                           | #s2c_chat_unmute_result{}
                           | #s2c_chat_typing{}
                           | #s2c_message{}
                           | #s2c_message_update{}
                           | #s2c_message_update_status{}
                           | #s2c_user_info{}
                           | #s2c_user_status{}
                           | #s2c_user_set_info_result{}
                           | #s2c_user_search_result{}
                           | #s2c_room_list{}
                           | #s2c_room_info{}
                           | #s2c_room_rename_result{}
                           | #s2c_room_add_user_result{}
                           | #s2c_room_del_user_result{}
                           | #s2c_room_add_subroom_result{}
                           | #s2c_room_create_result{}.

-type msg_type() :: server_msg_type()
                  | client_msg_type().

-endif.
