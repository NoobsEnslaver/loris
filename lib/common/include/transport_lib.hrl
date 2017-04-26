-ifndef(TRANSPORT_LIB_HRL).

-define(MSGPACK, <<"msgpack">>).
-define(JSON, <<"json">>).

-define(SUPPORTED_TRANSPORT, [?MSGPACK, ?JSON]).

-define(R2M(Record, RecName),
        maps:from_list(lists:zip(record_info(fields, RecName), tl(tuple_to_list(Record))))).

-define(TRANSPORT_LIB_HRL, 'true').
-endif.
