-include("server.hrl").

-ifndef(WS_DEFAULT_PROTOCOL_V1_MESSAGES_HRL).
-define(WS_DEFAULT_PROTOCOL_V1_MESSAGES_HRL, 'true').

-record(some_bad_async_work, {}).
-record(some_good_async_work, {}).
-record(some_long_async_work, {}).
-record(some_good_sync_work, {}).
-record(some_bad_sync_work, {}).
-record(sync_get_socket_info, {}).

-record(some_server_response_1, {field1, field2}).
-record(some_server_response_2, {field3, field4}).
-record(some_server_response_3, {field5, field6, field7}).

-type client_msg_type() :: #some_bad_async_work{}
                         | #some_good_async_work{}
                         | #some_long_async_work{}
                         | #some_good_sync_work{}
                         | #some_bad_sync_work{}
                         | #sync_get_socket_info{}.

-type server_msg_type() :: #some_server_response_1{}
                         | #some_server_response_2{}
                         | #some_server_response_3{}
                         | 'error'
                         | #async_start{}
                         | #async_done{}
                         | #async_error{}.

-endif.
