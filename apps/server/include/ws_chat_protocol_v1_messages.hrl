-ifndef(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL).
-define(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL, 'true').

%% Client-to-Server
-record(c2s_chat_get_list, {}).
-record(c2s_chat_get_info, {chat_id  :: integer()}).
-record(c2s_chat_create, {name :: binary(), user_msisdn :: [integer()]}).
-record(c2s_chat_leave, {chat_id :: integer()}).
-record(c2s_chat_delete, {chat_id :: integer()}).
-record(c2s_chat_invite_user, {chat_id :: integer(), user_msisdn :: integer() }).
-record(c2s_chat_mute, {chat_id :: integer()}).
-record(c2s_chat_unmute, {chat_id :: integer()}).
-record(c2s_chat_typing, {chat_id :: integer()}).
-record(c2s_message_send, {chat_id :: integer(), msg_body :: binary()}).
-record(c2s_message_get_list, {}).              %TODO
-record(c2s_message_update, {msg_id :: integer(), msg_body :: binary()}).
-record(c2s_message_update_status, {msg_id :: integer()}).
-record(c2s_system_logout, {}).
-record(c2s_user_get_info, {user_msisdn :: integer()}).
-record(c2s_user_get_status, {user_msisdn :: integer()}).
-record(c2s_user_set_info, {user_msisdn :: integer(), fname :: binary(), lname :: binary(), age :: non_neg_integer(), is_male :: boolean()}).
-record(c2s_user_search, {}).                   %TODO: нужно ли? чем отличается от get_info?
-record(c2s_room_get_tree, {}).
-record(c2s_room_get_info, {room_id :: integer(), subroom_id :: list(), user_msisdn :: integer(), chat_id :: integer(), room_name :: binary()}).
-record(c2s_room_rename, {room_id :: integer(), room_name :: binary()}).
-record(c2s_room_add_user, {room_id :: integer(), user_msisdn :: integer()}).
-record(c2s_room_del_user, {room_id :: integer(), user_msisdn :: integer()}).
-record(c2s_room_add_subroom, {room_id :: integer(), subroom_id :: integer()}).
-record(c2s_room_create, {room_name :: binary()}).
-record(c2s_room_delete, {room_id :: integer()}).
-record(c2s_room_enter_to_chat, {room_id :: integer(), chat_id :: integer()}).
-record(c2s_room_send_message, {room_id :: integer(), msg_body :: binary(), is_recursive :: boolean()}).
%% TODO: room_chat operations

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
-record(s2c_chat_info, {chat_id, name, user_msisdn = [], is_muted, is_writable, chat_owner}).
-record(s2c_chat_create_result, {result_code, chat_id}).
-record(s2c_chat_leave_result, {result_code, chat_id}).
-record(s2c_chat_delete_result, {result_code, chat_id}).
-record(s2c_chat_invite_user_result, {result_code, chat_id, user_msisdn}).
-record(s2c_chat_mute_result, {result_code, chat_id}).
-record(s2c_chat_unmute_result, {result_code, chat_id}).
-record(s2c_chat_typing, {chat_id, user_msisdn}).
-record(s2c_message, {chat_id, msg_body, timestamp, status, msg_id}).
-record(s2c_message_update, {chat_id, msg_id, msg_body}).
-record(s2c_message_update_status, {chat_id, msg_id}).
-record(s2c_user_info, {user_msisdn, fname, lname, age, is_male}).
-record(s2c_user_status, {user_msisdn, is_online = 'false', last_visit_timestamp}).
-record(s2c_user_set_info_result, {result_code, fname, lname, age, is_male}).
-record(s2c_user_search_result, {user_msisdn = []}).
-record(s2c_room_list, {room_id = []}).
-record(s2c_room_info, {room_id, subroom_id = [], user_msisdn = [], chat_id = []}).
-record(s2c_room_rename_result, {result_code, room_id, name}).
-record(s2c_room_add_user_result, {result_code, room_id, user_msisdn, access_level, is_public = 'false'}).
-record(s2c_room_del_user_result, {result_code, room_id, user_msisdn}).
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
