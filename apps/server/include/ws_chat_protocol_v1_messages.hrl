-ifndef(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL).
-define(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL, 'true').

%% Client-To-Server message codes
-define(C2S_CHAT_GET_LIST_TYPE, 1).
-define(C2S_CHAT_GET_INFO_TYPE, 2).
-define(C2S_CHAT_CREATE_TYPE, 3).
-define(C2S_CHAT_LEAVE_TYPE, 4).
-define(C2S_CHAT_DELETE_TYPE, 5).
-define(C2S_CHAT_INVITE_USER_TYPE, 6).
-define(C2S_CHAT_MUTE_TYPE, 7).
-define(C2S_CHAT_UNMUTE_TYPE, 8).
-define(C2S_CHAT_TYPING_TYPE, 9).
-define(C2S_MESSAGE_SEND_TYPE, 10).
-define(C2S_MESSAGE_GET_LIST_TYPE, 11).
-define(C2S_MESSAGE_UPDATE_TYPE, 12).
-define(C2S_MESSAGE_UPDATE_STATUS_TYPE, 13).
-define(C2S_SYSTEM_LOGOUT_TYPE, 14).
-define(C2S_USER_GET_INFO_TYPE, 15).
-define(C2S_USER_GET_STATUS_TYPE, 16).
-define(C2S_USER_SET_INFO_TYPE, 17).
-define(C2S_USER_SEARCH_TYPE, 18).
-define(C2S_ROOM_RENAME_TYPE, 19).
-define(C2S_ROOM_ADD_USER_TYPE, 20).
-define(C2S_ROOM_DEL_USER_TYPE, 21).
-define(C2S_ROOM_ADD_SUBROOM_TYPE, 22).
-define(C2S_ROOM_CREATE_TYPE, 23).
-define(C2S_ROOM_DELETE_TYPE, 24).
-define(C2S_ROOM_ENTER_TO_CHAT_TYPE, 25).
-define(C2S_ROOM_SEND_MESSAGE_TYPE, 26).
-define(C2S_ROOM_GET_TREE_TYPE, 27).
-define(C2S_ROOM_GET_INFO_TYPE, 28).
-define(C2S_CHAT_ACCEPT_INVATATION_TYPE, 29).
-define(C2S_CHAT_REJECT_INVATATION_TYPE, 30).
-define(C2S_CALL_INVITE_TYPE, 34).
-define(C2S_CALL_ACK_TYPE, 35).
-define(C2S_CALL_ICE_CANDIDATE_TYPE, 36).
-define(C2S_CALL_BYE_TYPE, 37).

%% Server-To-Client message codes
-define(S2C_CHAT_LIST_TYPE, 101).
-define(S2C_CHAT_INFO_TYPE, 102).
-define(S2C_CHAT_CREATE_RESULT_TYPE, 103).
-define(S2C_ERROR_TYPE, 104).
-define(S2C_CHAT_INVATATION_TYPE, 105).
-define(S2C_CHAT_TYPING_TYPE, 106).
-define(S2C_MESSAGE_TYPE, 107).
-define(S2C_MESSAGE_UPDATE_TYPE, 108).
-define(S2C_MESSAGE_UPDATE_STATUS_TYPE, 109).
-define(S2C_USER_INFO_TYPE, 110).
-define(S2C_USER_STATUS_TYPE, 111).
-define(S2C_USER_SEARCH_RESULT_TYPE, 112).
-define(S2C_ROOM_LIST_TYPE, 113).
-define(S2C_ROOM_TREE_TYPE, 114).
-define(S2C_ROOM_INFO_TYPE, 115).
-define(S2C_ROOM_CREATE_RESULT_TYPE, 116).
-define(S2C_MESSAGE_LIST_TYPE, 117).
-define(S2C_MESSAGE_SEND_RESULT_TYPE, 118).
-define(S2C_CALL_INVITE_TYPE, 119).
-define(S2C_CALL_ACK_TYPE, 120).
-define(S2C_CALL_ICE_CANDIDATE_TYPE, 121).
-define(S2C_CALL_BYE_TYPE, 122).

%% Client-to-Server
-record(c2s_chat_get_list, {msg_type = ?C2S_CHAT_GET_LIST_TYPE}).
-record(c2s_chat_get_info, {msg_type = ?C2S_CHAT_GET_INFO_TYPE, chat_id  :: binary()}).
-record(c2s_chat_create, {msg_type = ?C2S_CHAT_CREATE_TYPE, name :: binary(), users :: [non_neg_integer()]}).
-record(c2s_chat_leave, {msg_type = ?C2S_CHAT_LEAVE_TYPE, chat_id :: binary()}).
-record(c2s_chat_delete, {msg_type = ?C2S_CHAT_DELETE_TYPE, chat_id :: binary()}).
-record(c2s_chat_invite_user, {msg_type = ?C2S_CHAT_INVITE_USER_TYPE, chat_id :: binary(), user_msisdn :: non_neg_integer() }).
-record(c2s_chat_accept_invatation, {msg_type = ?C2S_CHAT_ACCEPT_INVATATION_TYPE, chat_id :: binary()}).
-record(c2s_chat_reject_invatation, {msg_type = ?C2S_CHAT_REJECT_INVATATION_TYPE, chat_id :: binary()}).
-record(c2s_chat_mute, {msg_type = ?C2S_CHAT_MUTE_TYPE, chat_id :: binary()}).
-record(c2s_chat_unmute, {msg_type = ?C2S_CHAT_UNMUTE_TYPE, chat_id :: binary()}).
-record(c2s_chat_typing, {msg_type = ?C2S_CHAT_TYPING_TYPE, chat_id :: binary()}).
-record(c2s_message_send, {msg_type = ?C2S_MESSAGE_SEND_TYPE, chat_id :: binary(), msg_body :: binary()}).
-record(c2s_message_get_list, {msg_type = ?C2S_MESSAGE_GET_LIST_TYPE, chat_id :: binary(), msg_id :: non_neg_integer()}).
-record(c2s_message_update, {msg_type = ?C2S_MESSAGE_UPDATE_TYPE, chat_id :: binary(), msg_id :: integer(), msg_body :: binary()}).
-record(c2s_message_update_status, {msg_type = ?C2S_MESSAGE_UPDATE_STATUS_TYPE, chat_id :: binary(), msg_id :: [non_neg_integer()]}).
-record(c2s_system_logout, {msg_type = ?C2S_SYSTEM_LOGOUT_TYPE}).
-record(c2s_user_get_info, {msg_type = ?C2S_USER_GET_INFO_TYPE, user_msisdn :: non_neg_integer()}).
-record(c2s_user_get_status, {msg_type = ?C2S_USER_GET_STATUS_TYPE, user_msisdn :: non_neg_integer()}).
-record(c2s_user_set_info, {msg_type = ?C2S_USER_SET_INFO_TYPE, fname :: binary(), lname :: binary(), age :: non_neg_integer(), is_male :: boolean()}).
-record(c2s_user_search, {msg_type = ?C2S_USER_SEARCH_TYPE, fname :: binary(), lname :: binary()}).                   %TODO
-record(c2s_room_get_tree, {msg_type = ?C2S_ROOM_GET_TREE_TYPE, room_id :: non_neg_integer()}).
-record(c2s_room_get_info, {msg_type = ?C2S_ROOM_GET_INFO_TYPE, room_id :: non_neg_integer(), subroom_id :: [non_neg_integer()], user_msisdn :: non_neg_integer(), chat_id :: binary(), room_name :: binary()}).
-record(c2s_room_rename, {msg_type = ?C2S_ROOM_RENAME_TYPE, room_id :: non_neg_integer(), room_name :: binary()}).
-record(c2s_room_add_user, {msg_type = ?C2S_ROOM_ADD_USER_TYPE, room_id :: non_neg_integer(), user_msisdn :: non_neg_integer()}).
-record(c2s_room_del_user, {msg_type = ?C2S_ROOM_DEL_USER_TYPE, room_id :: non_neg_integer(), user_msisdn :: non_neg_integer()}).
-record(c2s_room_add_subroom, {msg_type = ?C2S_ROOM_ADD_SUBROOM_TYPE, room_id :: non_neg_integer(), subroom_id :: non_neg_integer()}).
-record(c2s_room_create, {msg_type = ?C2S_ROOM_CREATE_TYPE, room_name :: binary()}).
-record(c2s_room_delete, {msg_type = ?C2S_ROOM_DELETE_TYPE, room_id :: non_neg_integer()}).
-record(c2s_room_enter_to_chat, {msg_type = ?C2S_ROOM_ENTER_TO_CHAT_TYPE, room_id :: non_neg_integer(), chat_id :: non_neg_integer()}).
-record(c2s_room_send_message, {msg_type = ?C2S_ROOM_SEND_MESSAGE_TYPE, room_id :: non_neg_integer(), msg_body :: binary(), is_recursive :: boolean()}).
%% TODO: room_chat operations
-record(c2s_call_invite, {msg_type = ?C2S_CALL_INVITE_TYPE, msisdn :: non_neg_integer(), sdp_offer :: binary()}).
-record(c2s_call_ack, {msg_type = ?C2S_CALL_ACK_TYPE}).
-record(c2s_call_ice_candidate, {msg_type = ?C2S_CALL_ICE_CANDIDATE_TYPE, candidate :: binary()}).
-record(c2s_call_bye, {msg_type = ?C2S_CALL_BYE_TYPE, code :: non_neg_integer()}).

-type client_msg_type() ::   #c2s_chat_get_list{}
                           | #c2s_chat_get_info{}
                           | #c2s_chat_create{}
                           | #c2s_chat_leave{}
                           | #c2s_chat_delete{}
                           | #c2s_chat_invite_user{}
                           | #c2s_chat_mute{}
                           | #c2s_chat_unmute{}
                           | #c2s_chat_typing{}
                           | #c2s_chat_accept_invatation{}
                           | #c2s_chat_reject_invatation{}
                           | #c2s_message_send{}
                           | #c2s_message_get_list{}
                           | #c2s_message_update{}
                           | #c2s_message_update_status{}
                           | #c2s_system_logout{}
                           | #c2s_user_get_info{}
                           | #c2s_user_get_status{}
                           | #c2s_user_set_info{}
                           | #c2s_user_search{}
                           | #c2s_room_rename{}
                           | #c2s_room_add_user{}
                           | #c2s_room_del_user{}
                           | #c2s_room_add_subroom{}
                           | #c2s_room_create{}
                           | #c2s_room_delete{}
                           | #c2s_room_get_tree{}
                           | #c2s_room_get_info{}
                           | #c2s_room_enter_to_chat{}
                           | #c2s_room_send_message{}
                           | #c2s_call_invite{}
                           | #c2s_call_ack{}
                           | #c2s_call_ice_candidate{}
                           | #c2s_call_bye{}.

%% Server-to-Client
-record(s2c_chat_list, {msg_type = ?S2C_CHAT_LIST_TYPE, chats :: map()}).
-record(s2c_chat_info, {msg_type = ?S2C_CHAT_INFO_TYPE, chat_id :: binary(), name :: binary(), users :: [non_neg_integer()], is_muted :: boolean(), chat_owner :: non_neg_integer(), access_group :: atom()}).
-record(s2c_chat_create_result, {msg_type = ?S2C_CHAT_CREATE_RESULT_TYPE, chat_id :: binary()}).
-record(s2c_error, {msg_type = ?S2C_ERROR_TYPE, code :: non_neg_integer()}).
-record(s2c_chat_invatation, {msg_type = ?S2C_CHAT_INVATATION_TYPE, chat_id :: binary()}).
-record(s2c_chat_typing, {msg_type = ?S2C_CHAT_TYPING_TYPE, chat_id :: binary(), user_msisdn :: non_neg_integer()}).
-record(s2c_message, {msg_type = ?S2C_MESSAGE_TYPE, chat_id :: binary(), msg_body :: binary(), timestamp :: non_neg_integer(), status :: 'pending' | 'delivered' | 'read', msg_id :: non_neg_integer(), from :: non_neg_integer()}).
-record(s2c_message_update, {msg_type = ?S2C_MESSAGE_UPDATE_TYPE, chat_id :: binary(), msg_id :: non_neg_integer(), msg_body :: binary()}).
-record(s2c_message_update_status, {msg_type = ?S2C_MESSAGE_UPDATE_STATUS_TYPE, chat_id :: binary(), msg_id}).
-record(s2c_message_list, {msg_type = ?S2C_MESSAGE_LIST_TYPE, messages :: [#s2c_message{}]}).           %TODO: chat_id
-record(s2c_user_info, {msg_type = ?S2C_USER_INFO_TYPE, user_msisdn :: non_neg_integer(), fname :: binary(), lname :: binary(), age :: non_neg_integer(), is_male :: boolean()}).
-record(s2c_user_status, {msg_type = ?S2C_USER_STATUS_TYPE, user_msisdn :: non_neg_integer(), is_online = 'false', last_visit_timestamp :: non_neg_integer()}).
-record(s2c_user_search_result, {msg_type = ?S2C_USER_SEARCH_RESULT_TYPE, users :: [non_neg_integer()]}).
-record(s2c_room_list, {msg_type = ?S2C_ROOM_LIST_TYPE, room_id :: non_neg_integer()}).
-record(s2c_room_tree, {msg_type = ?S2C_ROOM_TREE_TYPE, room_id :: non_neg_integer()}).
-record(s2c_room_info, {msg_type = ?S2C_ROOM_INFO_TYPE, room_id :: non_neg_integer(), subrooms :: [non_neg_integer()], users :: [non_neg_integer()], chats :: [binary()]}).
-record(s2c_room_create_result, {msg_type = ?S2C_ROOM_CREATE_RESULT_TYPE, room_id :: non_neg_integer()}).
-record(s2c_message_send_result, {msg_type = ?S2C_MESSAGE_SEND_RESULT_TYPE, msg_id :: non_neg_integer(), chat_id :: non_neg_integer()}).
-record(s2c_call_invite, {msg_type = ?S2C_CALL_INVITE_TYPE, msisdn :: non_neg_integer(), sdp_offer :: binary()}).
-record(s2c_call_ack, {msg_type = ?S2C_CALL_ACK_TYPE}).
-record(s2c_call_ice_candidate, {msg_type = ?S2C_CALL_ICE_CANDIDATE_TYPE, candidate :: binary()}).
-record(s2c_call_bye, {msg_type = ?S2C_CALL_BYE_TYPE, code :: non_neg_integer()}).

-type server_msg_type() ::   #s2c_chat_list{}
                           | #s2c_chat_info{}
                           | #s2c_chat_create_result{}
                           | #s2c_error{}
                           | #s2c_chat_invatation{}
                           | #s2c_chat_typing{}
                           | #s2c_message{}
                           | #s2c_message_update{}
                           | #s2c_message_update_status{}
                           | #s2c_message_list{}
                           | #s2c_user_info{}
                           | #s2c_user_status{}
                           | #s2c_user_search_result{}
                           | #s2c_room_list{}
                           | #s2c_room_tree{}
                           | #s2c_room_info{}
                           | #s2c_room_create_result{}
                           | #s2c_call_invite{}
                           | #s2c_call_ack{}
                           | #s2c_call_ice_candidate{}
                           | #s2c_call_bye{}.

-type msg_type() :: server_msg_type()
                  | client_msg_type().

-endif.
