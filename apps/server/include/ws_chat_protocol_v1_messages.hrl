-ifndef(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL).
-define(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL, 'true').

%% Client-to-Server
-record(c2s_chat_get_list, {}).
-record(c2s_chat_get_info, {chat_id  :: binary()}).
-record(c2s_chat_create, {name :: binary(), users :: [binary()]}).
-record(c2s_chat_leave, {chat_id :: binary()}).
-record(c2s_chat_delete, {chat_id :: binary()}).
-record(c2s_chat_invite_user, {chat_id :: binary(), user_msisdn :: binary() }).
-record(c2s_chat_accept_invatation, {chat_id :: binary()}).
-record(c2s_chat_reject_invatation, {chat_id :: binary()}).
-record(c2s_chat_mute, {chat_id :: binary()}).
-record(c2s_chat_unmute, {chat_id :: binary()}).
-record(c2s_chat_typing, {chat_id :: binary()}).
-record(c2s_message_send, {chat_id :: binary(), msg_body :: binary()}).
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
-record(s2c_chat_list, {chat_id::[binary()]}).
-record(s2c_chat_info, {chat_id::binary(), name :: binary(), users :: [non_neg_integer()], is_muted :: boolean(), chat_owner :: non_neg_integer(), access_group :: atom()}).
-record(s2c_chat_create_result, {chat_id :: binary()}).
-record(s2c_error, {code :: non_neg_integer()}).
-record(s2c_chat_invatation, {chat_id :: binary()}).
-record(s2c_chat_typing, {chat_id :: binary(), user_msisdn :: non_neg_integer()}).
-record(s2c_message, {chat_id :: binary(), msg_body :: binary(), timestamp :: non_neg_integer(), status, msg_id :: non_neg_integer()}).
-record(s2c_message_update, {chat_id :: binary(), msg_id :: non_neg_integer(), msg_body :: binary()}).
-record(s2c_message_update_status, {chat_id :: binary(), msg_id}).
-record(s2c_user_info, {user_msisdn, fname :: binary(), lname :: binary(), age, is_male}).
-record(s2c_user_status, {user_msisdn, is_online = 'false', last_visit_timestamp}).
-record(s2c_user_search_result, {user_msisdn = []}).
-record(s2c_room_list, {room_id :: binary()}).
-record(s2c_room_tree, {room_id :: binary()}).
-record(s2c_room_info, {room_id :: binary(), subroom_id :: [binary()], user_msisdn :: [binary()], chat_id :: [binary()]}).
-record(s2c_room_create_result, {room_id :: binary()}).

-type server_msg_type() ::   #s2c_chat_list{}
                           | #s2c_chat_info{}
                           | #s2c_chat_create_result{}
                           | #s2c_error{}
                           | #s2c_chat_invatation{}
                           | #s2c_chat_typing{}
                           | #s2c_message{}
                           | #s2c_message_update{}
                           | #s2c_message_update_status{}
                           | #s2c_user_info{}
                           | #s2c_user_status{}
                           | #s2c_user_search_result{}
                           | #s2c_room_list{}
                           | #s2c_room_tree{}
                           | #s2c_room_info{}
                           | #s2c_room_create_result{}.

-type msg_type() :: server_msg_type()
                  | client_msg_type().

-endif.
