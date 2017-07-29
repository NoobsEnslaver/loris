-ifndef(SERVER_HRL).
-define(SERVER_HRL, 'true').

-include_lib("common/include/transport_lib.hrl").

-define(APP_NAME, <<"server">>).
-define(TIMEOUT, 600000).
-define(ASYNC_WORK_TIMEOUT, 5000).

-ifdef(hardlog).
-define(HARDLOG(Level, Str, Obj), lager:Level(Str, [Obj])).
-else.
-define(HARDLOG(_Level, _Str, _Obj), ok).
-endif.

-record(q_state, {code = 200 :: non_neg_integer()
                 ,headers = #{} :: map()
                 ,body = <<>> :: binary()
                 ,tmp_state = #{} :: map()
                 ,req_body = #{}}).
-record(async_start, {work_id :: binary()}).
-record(async_done, {work_id :: binary()
                    ,result :: tuple()}).
-record(async_error, {work_id :: binary()
                     ,error_code = 500 :: integer()}).

-type method() :: binary().

-endif.
