-ifndef(TABLES_HRL).
-define(TABLES_HRL, 'true').

-record(file, {hash::binary()
              ,content_type::binary()
              ,name::binary()
              ,data::binary()
              ,owner_id::binary()}).

-record(session, {token::binary()
                 ,access_level::non_neg_integer()
                 ,ws_pid::pid()
                 ,owner_id::binary()
                 ,expiration_time::non_neg_integer()}).

-endif.
