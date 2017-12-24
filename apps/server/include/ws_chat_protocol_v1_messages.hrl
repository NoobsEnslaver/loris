-ifndef(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL).
-define(WS_CHAT_PROTOCOL_V1_MESSAGES_HRL, 'true').

-include_lib("common/include/tables.hrl").

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
-define(C2S_ROOM_CREATE_TYPE, 19).
-define(C2S_ROOM_DELETE_TYPE, 20).
-define(C2S_ROOM_SET_INFO_TYPE, 21).
-define(C2S_ROOM_GET_INFO_TYPE, 22).
-define(C2S_ROOM_ADD_SUBROOM_TYPE, 23).
-define(C2S_ROOM_DEL_SUBROOM_TYPE, 24).
-define(C2S_ROOM_JOIN_TO_CHAT_TYPE, 25).
-define(C2S_ROOM_SEARCH_TYPE, 26).
-define(C2S_ROOM_GET_MY_ROOMS, 27).
-define(C2S_CHAT_ACCEPT_INVATATION_TYPE, 29).
-define(C2S_CHAT_REJECT_INVATATION_TYPE, 30).
-define(C2S_CALL_OFFER_TYPE, 34).
-define(C2S_CALL_ACK_TYPE, 35).
-define(C2S_CALL_ICE_CANDIDATE_TYPE, 36).
-define(C2S_CALL_BYE_TYPE, 37).
-define(C2S_CALL_ANSWER_TYPE, 38).
-define(C2S_LOCK_TURN_SERVER_TYPE, 39).
-define(C2S_USER_GET_INFO_BULK_TYPE, 40).
-define(C2S_DEVICE_REGISTER, 41).
-define(C2S_USER_SUBSCRIBE_TYPE, 42).
-define(C2S_USER_UNSUBSCRIBE_TYPE, 43).
-define(C2S_ROOM_SEND_RECURSIVE_MESSAGE_TYPE, 45).
-define(C2S_STORAGE_SET_TYPE, 46).
-define(C2S_STORAGE_GET_TYPE, 47).
-define(C2S_STORAGE_KEYS_TYPE, 48).
-define(C2S_STORAGE_DELETE_TYPE, 49).
-define(C2S_STORAGE_CAPACITY_TYPE, 50).
-define(C2S_RESOURCE_GET_TYPE, 51).
-define(C2S_RESOURCE_SET_TYPE, 52).
-define(C2S_RESOURCE_DELETE_TYPE, 53).
-define(C2S_USER_SET_SPORTSMAN_INFO_TYPE, 54).
-define(C2S_USER_SET_TRAINER_INFO_TYPE, 55).
-define(C2S_USER_SET_PARENT_INFO_TYPE, 56).
-define(C2S_TOURNAMENT_CREATE, 57).
-define(C2S_TOURNAMENT_READ, 58).
-define(C2S_TOURNAMENT_UPDATE, 59).
-define(C2S_TOURNAMENT_DELETE, 60).
-define(C2S_TOURNAMENT_GET_LIST, 61).
-define(C2S_USER_SET_GROUP_TYPE, 62).

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
-define(S2C_ROOM_CREATE_RESULT_TYPE, 113).
-define(S2C_ROOM_INFO_TYPE, 114).
-define(S2C_ROOM_LIST_TYPE, 115).
-define(S2C_ROOM_SEARCH_RESULT_TYPE, 116).
-define(S2C_MESSAGE_LIST_TYPE, 117).
-define(S2C_MESSAGE_SEND_RESULT_TYPE, 118).
-define(S2C_CALL_OFFER_TYPE, 119).
-define(S2C_CALL_ACK_TYPE, 120).
-define(S2C_CALL_ICE_CANDIDATE_TYPE, 121).
-define(S2C_CALL_BYE_TYPE, 122).
-define(S2C_CALL_ANSWER_TYPE, 123).
-define(S2C_TURN_SERVER_TYPE, 124).
-define(S2C_USER_INFO_BULK_TYPE, 125).
-define(S2C_STORAGE_CAPACITY_TYPE, 126).
-define(S2C_STORAGE_GET_RESULT_TYPE, 127).
-define(S2C_STORAGE_KEYS_TYPE, 128).
-define(S2C_RESOURCE_TYPE, 129).
-define(S2C_RESOURCE_LIST_TYPE, 130).
-define(S2C_TOURNAMENT_INFO, 131).
-define(S2C_TOURNAMENT_LIST, 132).
-define(S2C_TOURNAMENT_CREATE_RESULT, 133).

%% Client-to-Server
-record(c2s_chat_get_list, {msg_type = ?C2S_CHAT_GET_LIST_TYPE}).
-record(c2s_chat_get_info, {msg_type = ?C2S_CHAT_GET_INFO_TYPE, chat_id  :: binary()}).
-record(c2s_chat_create, {msg_type = ?C2S_CHAT_CREATE_TYPE, name :: binary(), users :: map(), is_p2p :: boolean()}).
-record(c2s_chat_leave, {msg_type = ?C2S_CHAT_LEAVE_TYPE, chat_id :: binary()}).
-record(c2s_chat_delete, {msg_type = ?C2S_CHAT_DELETE_TYPE, chat_id :: binary()}).
-record(c2s_chat_invite_user, {msg_type = ?C2S_CHAT_INVITE_USER_TYPE, chat_id :: binary(), user_msisdn :: non_neg_integer(), access_level :: 0..7}).
-record(c2s_chat_accept_invatation, {msg_type = ?C2S_CHAT_ACCEPT_INVATATION_TYPE, chat_id :: binary()}).
-record(c2s_chat_reject_invatation, {msg_type = ?C2S_CHAT_REJECT_INVATATION_TYPE, chat_id :: binary()}).
-record(c2s_chat_mute, {msg_type = ?C2S_CHAT_MUTE_TYPE, chat_id :: binary()}).
-record(c2s_chat_unmute, {msg_type = ?C2S_CHAT_UNMUTE_TYPE, chat_id :: binary()}).
-record(c2s_chat_typing, {msg_type = ?C2S_CHAT_TYPING_TYPE, chat_id :: binary()}).
-record(c2s_message_send, {msg_type = ?C2S_MESSAGE_SEND_TYPE, chat_id :: binary(), msg_body :: binary()}).
-record(c2s_message_get_list, {msg_type = ?C2S_MESSAGE_GET_LIST_TYPE, chat_id :: binary(), msg_id :: non_neg_integer(), count :: non_neg_integer(), direction :: up | down}).
-record(c2s_message_update, {msg_type = ?C2S_MESSAGE_UPDATE_TYPE, chat_id :: binary(), msg_id :: integer(), msg_body :: binary()}).
-record(c2s_message_update_status, {msg_type = ?C2S_MESSAGE_UPDATE_STATUS_TYPE, chat_id :: binary(), msg_id :: [non_neg_integer()]}).
-record(c2s_system_logout, {msg_type = ?C2S_SYSTEM_LOGOUT_TYPE}).
-record(c2s_user_get_info, {msg_type = ?C2S_USER_GET_INFO_TYPE, user_msisdn :: non_neg_integer()}).
-record(c2s_user_get_info_bulk, {msg_type = ?C2S_USER_GET_INFO_BULK_TYPE, msisdns :: [non_neg_integer()]}).
-record(c2s_user_get_status, {msg_type = ?C2S_USER_GET_STATUS_TYPE, user_msisdn :: non_neg_integer()}).
-record(c2s_user_set_info, {msg_type = ?C2S_USER_SET_INFO_TYPE, fname :: binary(), lname :: binary(), age :: non_neg_integer(), is_male :: boolean(), city :: binary()}).
-record(c2s_user_set_sportsman_info, {msg_type = ?C2S_USER_SET_SPORTSMAN_INFO_TYPE, msisdn :: non_neg_integer(),height :: non_neg_integer(),weight :: non_neg_integer(),kyu :: non_neg_integer(),affiliate_id :: non_neg_integer(),is_volunteer :: boolean(),is_on_team :: boolean()}).
-record(c2s_user_set_parent_info, {msg_type = ?C2S_USER_SET_PARENT_INFO_TYPE, msisdn :: non_neg_integer(), affiliate_id :: non_neg_integer(), parental_committee :: boolean(),is_volunteer :: boolean()}).
-record(c2s_user_set_trainer_info, {msg_type = ?C2S_USER_SET_TRAINER_INFO_TYPE,msisdn :: non_neg_integer(),affiliate_id :: non_neg_integer(),trainer_committee :: boolean(),is_judge :: boolean(),is_department_head :: boolean()}).
-record(c2s_user_search, {msg_type = ?C2S_USER_SEARCH_TYPE, fname :: binary(), lname :: binary(), city :: binary(), group :: access_group() | undefined}).
-record(c2s_user_subscribe, {msg_type = ?C2S_USER_SUBSCRIBE_TYPE, msisdn :: [non_neg_integer()]}).
-record(c2s_user_unsubscribe, {msg_type = ?C2S_USER_UNSUBSCRIBE_TYPE, msisdn :: [non_neg_integer()]}).
-record(c2s_room_add_subroom, {msg_type = ?C2S_ROOM_ADD_SUBROOM_TYPE, room_id :: non_neg_integer(), subroom_id :: non_neg_integer()}).
-record(c2s_room_del_subroom, {msg_type = ?C2S_ROOM_DEL_SUBROOM_TYPE, room_id :: non_neg_integer(), subroom_id :: non_neg_integer()}).
-record(c2s_room_create, {msg_type = ?C2S_ROOM_CREATE_TYPE, name :: binary(), description :: binary(), room_access :: map(), chat_access :: map(), tags :: #room_tag{}, additional_info :: any()}).
-record(c2s_room_delete, {msg_type = ?C2S_ROOM_DELETE_TYPE, room_id :: non_neg_integer()}).
-record(c2s_room_set_info, {msg_type = ?C2S_ROOM_SET_INFO_TYPE, room_id :: non_neg_integer(), description :: binary(), name :: binary(), tags :: #room_tag{}, room_access :: map(), chat_access :: map(), additional_info :: any()}).
-record(c2s_room_get_info, {msg_type = ?C2S_ROOM_GET_INFO_TYPE, room_id :: non_neg_integer()}).
-record(c2s_room_join_to_chat, {msg_type = ?C2S_ROOM_JOIN_TO_CHAT_TYPE, room_id :: non_neg_integer()}).
-record(c2s_room_search, {msg_type = ?C2S_ROOM_SEARCH_TYPE, room_id :: non_neg_integer(), tags :: #room_tag{}, name :: binary()}).
-record(c2s_room_get_my_rooms, {msg_type = ?C2S_ROOM_GET_MY_ROOMS}).
-record(c2s_room_send_recursive_message, {msg_type = ?C2S_ROOM_SEND_RECURSIVE_MESSAGE_TYPE, msg :: binary(), room_id :: non_neg_integer()}).
-record(c2s_call_offer, {msg_type = ?C2S_CALL_OFFER_TYPE, msisdn :: non_neg_integer(), sdp :: binary()}).
-record(c2s_call_answer, {msg_type = ?C2S_CALL_ANSWER_TYPE, sdp :: binary()}).
-record(c2s_call_ack, {msg_type = ?C2S_CALL_ACK_TYPE}).
-record(c2s_call_ice_candidate, {msg_type = ?C2S_CALL_ICE_CANDIDATE_TYPE, candidate :: binary()}).
-record(c2s_call_bye, {msg_type = ?C2S_CALL_BYE_TYPE, code :: non_neg_integer()}).
-record(c2s_lock_turn_server, {msg_type = ?C2S_LOCK_TURN_SERVER_TYPE}).
-record(c2s_device_register, {msg_type = ?C2S_DEVICE_REGISTER, push_token :: binary(), type :: non_neg_integer(), device_id :: binary()}). %type:: 0 - android, 1 - ios, 2 - ios_voip
-record(c2s_storage_set, {msg_type = ?C2S_STORAGE_SET_TYPE, key :: any(), value :: any()}).
-record(c2s_storage_get, {msg_type = ?C2S_STORAGE_GET_TYPE, key :: any()}).
-record(c2s_storage_keys, {msg_type = ?C2S_STORAGE_KEYS_TYPE}).
-record(c2s_storage_delete, {msg_type = ?C2S_STORAGE_DELETE_TYPE, key :: any()}).
-record(c2s_storage_capacity, {msg_type = ?C2S_STORAGE_CAPACITY_TYPE}).
-record(c2s_resource_get, {msg_type = ?C2S_RESOURCE_GET_TYPE, name :: binary(), group :: binary()}).
-record(c2s_resource_set, {msg_type = ?C2S_RESOURCE_SET_TYPE, name :: binary(), group :: binary(), value :: any()}).
-record(c2s_resource_delete, {msg_type = ?C2S_RESOURCE_DELETE_TYPE, name :: binary()}).
-record(c2s_user_set_group, {msg_type = ?C2S_USER_SET_GROUP_TYPE, msisdn :: non_neg_integer(), group :: binary()}).
-record(c2s_tournament_create, {msg_type = ?C2S_TOURNAMENT_CREATE, city :: binary(), name :: binary(), judges :: [non_neg_integer()], timestamp :: non_neg_integer(), participants :: #{MSISDN :: non_neg_integer() => #participant{}}}).
-record(c2s_tournament_read, {msg_type = ?C2S_TOURNAMENT_READ, id :: non_neg_integer()}).
-record(c2s_tournament_update, {msg_type = ?C2S_TOURNAMENT_UPDATE, id :: non_neg_integer(), city :: binary(), name :: binary(), judges :: [non_neg_integer()], timestamp :: non_neg_integer(), participants :: #{MSISDN :: non_neg_integer() => #participant{}}}).
-record(c2s_tournament_delete, {msg_type = ?C2S_TOURNAMENT_DELETE, id :: non_neg_integer()}).
-record(c2s_tournament_get_list, {msg_type = ?C2S_TOURNAMENT_GET_LIST}).

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
                           | #c2s_room_add_subroom{}
                           | #c2s_room_del_subroom{}
                           | #c2s_room_create{}
                           | #c2s_room_delete{}
                           | #c2s_room_set_info{}
                           | #c2s_room_get_info{}
                           | #c2s_room_join_to_chat{}
                           | #c2s_room_search{}
                           | #c2s_room_get_my_rooms{}
                           | #c2s_call_offer{}
                           | #c2s_call_answer{}
                           | #c2s_call_ack{}
                           | #c2s_call_ice_candidate{}
                           | #c2s_call_bye{}
                           | #c2s_lock_turn_server{}
                           | #c2s_device_register{}
                           | #c2s_user_subscribe{}
                           | #c2s_user_unsubscribe{}
                           | #c2s_storage_set{}
                           | #c2s_storage_get{}
                           | #c2s_storage_delete{}
                           | #c2s_storage_capacity{}
                           | #c2s_storage_keys{}
                           | #c2s_resource_get{}
                           | #c2s_resource_set{}
                           | #c2s_resource_delete{}
                           | #c2s_user_set_group{}
                           | #c2s_tournament_create{}
                           | #c2s_tournament_read{}
                           | #c2s_tournament_update{}
                           | #c2s_tournament_delete{}
                           | #c2s_tournament_get_list{}.

%% Server-to-Client
-record(s2c_chat_list, {msg_type = ?S2C_CHAT_LIST_TYPE, chats :: map()}).
-record(s2c_chat_info, {msg_type = ?S2C_CHAT_INFO_TYPE, chat_id :: binary(), name :: binary(), users :: [non_neg_integer()], is_muted :: boolean(), chat_owner :: non_neg_integer(), last_msg_id :: non_neg_integer(), access_level :: 0..7 | undefined, on_room :: non_neg_integer() | undefined}).
-record(s2c_chat_create_result, {msg_type = ?S2C_CHAT_CREATE_RESULT_TYPE, chat_id :: binary()}).
-record(s2c_error, {msg_type = ?S2C_ERROR_TYPE, code :: non_neg_integer()}).
-record(s2c_chat_invatation, {msg_type = ?S2C_CHAT_INVATATION_TYPE, chat_id :: binary(), access_level :: 0..7}).
-record(s2c_chat_typing, {msg_type = ?S2C_CHAT_TYPING_TYPE, chat_id :: binary(), user_msisdn :: non_neg_integer()}).
-record(s2c_message, {msg_type = ?S2C_MESSAGE_TYPE, chat_id :: binary(), msg_body :: binary(), status :: 'pending' | 'delivered' | 'read', msg_id :: non_neg_integer(), from :: non_neg_integer()}).
-record(s2c_message_update, {msg_type = ?S2C_MESSAGE_UPDATE_TYPE, chat_id :: binary(), msg_id :: non_neg_integer(), msg_body :: binary()}).
-record(s2c_message_update_status, {msg_type = ?S2C_MESSAGE_UPDATE_STATUS_TYPE, chat_id :: binary(), msg_id :: non_neg_integer(), status :: 'pending' | 'delivered' | 'read'}).
-record(s2c_message_list, {msg_type = ?S2C_MESSAGE_LIST_TYPE, chat_id :: binary(), messages :: [#s2c_message{}]}).
-record(s2c_user_info, {msg_type = ?S2C_USER_INFO_TYPE, user_msisdn :: non_neg_integer(), fname :: binary(), lname :: binary(), age :: non_neg_integer(), is_male :: boolean(), city :: binary(), group :: atom(), special_info :: #sportsman_info{}|#trainer_info{}|#parent_info{}|'undefined'}).
-record(s2c_user_info_bulk, {msg_type = ?S2C_USER_INFO_BULK_TYPE, users :: [#s2c_user_info{}]}).
-record(s2c_user_status, {msg_type = ?S2C_USER_STATUS_TYPE, msisdn :: non_neg_integer(), status :: atom(), last_visit_timestamp :: non_neg_integer()}).
-record(s2c_user_search_result, {msg_type = ?S2C_USER_SEARCH_RESULT_TYPE, users :: [non_neg_integer()]}).
-record(s2c_room_create_result, {msg_type = ?S2C_ROOM_CREATE_RESULT_TYPE, room_id :: non_neg_integer()}).
-record(s2c_room_info, {msg_type = ?S2C_ROOM_INFO_TYPE, room_id :: non_neg_integer(), name :: binary(), description :: binary(), tags :: #room_tag{}, subrooms :: [non_neg_integer()], chat_id :: binary(),room_access :: map(), chat_access :: map(), additional_info :: any()}).
-record(s2c_room_list, {msg_type = ?S2C_ROOM_LIST_TYPE, rooms :: [non_neg_integer()]}).
-record(s2c_room_search_result, {msg_type = ?S2C_ROOM_SEARCH_RESULT_TYPE, rooms :: map()}).
-record(s2c_message_send_result, {msg_type = ?S2C_MESSAGE_SEND_RESULT_TYPE, msg_id :: non_neg_integer(), chat_id :: non_neg_integer()}).
-record(s2c_turn_server, {msg_type = ?S2C_TURN_SERVER_TYPE, adress :: binary(), port :: non_neg_integer(), username :: binary(),realm :: binary(), credential :: binary(), credential_type :: binary()}).
-record(s2c_call_offer, {msg_type = ?S2C_CALL_OFFER_TYPE, msisdn :: non_neg_integer(), sdp :: binary(), turn_server :: #s2c_turn_server{}}).
-record(s2c_call_answer, {msg_type = ?S2C_CALL_ANSWER_TYPE, sdp :: binary()}).
-record(s2c_call_ack, {msg_type = ?S2C_CALL_ACK_TYPE}).
-record(s2c_call_ice_candidate, {msg_type = ?S2C_CALL_ICE_CANDIDATE_TYPE, candidate :: binary()}).
-record(s2c_call_bye, {msg_type = ?S2C_CALL_BYE_TYPE, code :: non_neg_integer()}).
-record(s2c_storage_keys, {msg_type = ?S2C_STORAGE_KEYS_TYPE, keys :: [any()]}).
-record(s2c_storage_get_result, {msg_type = ?S2C_STORAGE_GET_RESULT_TYPE, key :: any(), value :: any()}).
-record(s2c_storage_capacity, {msg_type = ?S2C_STORAGE_CAPACITY_TYPE, used :: non_neg_integer(), max :: non_neg_integer()}).
-record(s2c_resource, {msg_type = ?S2C_RESOURCE_TYPE, name :: binary(), value :: any()}).
-record(s2c_resource_list, {msg_type = ?S2C_RESOURCE_LIST_TYPE, group :: binary(), names :: [binary()]}).
-record(s2c_tournament_info, {msg_type = ?S2C_TOURNAMENT_INFO, id :: non_neg_integer(), city :: binary(), name :: binary(), judges :: [non_neg_integer()], timestamp :: non_neg_integer(), participants :: #{MSISDN :: non_neg_integer() => #participant{}}}).
-record(s2c_tournament_list, {msg_type = ?S2C_TOURNAMENT_LIST, tournaments :: map()}). %tournaments :: #{Id :: non_neg_integer() => map(name => binary(), city => binary(), timestamp => non_neg_integer())}
-record(s2c_tournament_create_result,{msg_type = ?S2C_TOURNAMENT_CREATE_RESULT, id :: non_neg_integer()}).

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
                           | #s2c_room_info{}
                           | #s2c_room_create_result{}
                           | #s2c_call_offer{}
                           | #s2c_call_answer{}
                           | #s2c_call_ack{}
                           | #s2c_call_ice_candidate{}
                           | #s2c_call_bye{}
                           | #s2c_turn_server{}
                           | #s2c_storage_keys{}
                           | #s2c_storage_get_result{}
                           | #s2c_storage_capacity{}
                           | #s2c_tournament_info{}
                           | #s2c_tournament_list{}
                           | #s2c_tournament_create_result{}.

-type msg_type() :: server_msg_type()
                  | client_msg_type().

-endif.
