-ifndef(SERVER_HRL).
-define(SERVER_HRL, 'true').

-include_lib("common/include/transport_lib.hrl").

-define(APP_NAME, <<"server">>).
-define(TIMEOUT, 60000).
-define(ASYNC_WORK_TIMEOUT, 5000).
-record(q_state, {code = 200 :: non_neg_integer()
                 ,headers = #{} :: map()
                 ,body = <<>> :: binary()
                 ,tmp_state = #{} :: map()}).

-type method() :: binary().

-endif.
