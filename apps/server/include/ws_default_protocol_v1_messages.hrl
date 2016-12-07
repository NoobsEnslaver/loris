-ifndef(WS_DEFAULT_PROTOCOL_V1_MESSAGES_HRL).
-define(WS_DEFAULT_PROTOCOL_V1_MESSAGES_HRL, 'true').

-record(some_bad_async_work, {}).
-record(some_good_async_work, {}).
-record(some_long_async_work, {}).
-record(some_good_sync_work, {}).
-record(some_bad_sync_work, {}).
-record(sync_get_socket_info, {}).

-type msg_type() :: #some_bad_async_work{}
                  | #some_good_async_work{}
                  | #some_long_async_work{}
                  | #some_good_sync_work{}
                  | #some_bad_sync_work{}
                  | #sync_get_socket_info{}.

-endif.
