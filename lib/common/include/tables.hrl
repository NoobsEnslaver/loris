-ifndef(TABLES_HRL).
-define(TABLES_HRL, 'true').

-record(file, {hash :: binary()
              ,content_type :: binary()
              ,name :: binary()
              ,data :: binary()
              ,owner_id :: non_neg_integer()
              ,size :: non_neg_integer()
              ,access_level :: non_neg_integer() | 'infinity'}).

-record(user, {login :: binary()
              ,id :: non_neg_integer()
              ,group ::'guests'|'users'|'administrators'
              ,pwd_hash :: binary()
              ,name :: binary()
              ,created :: non_neg_integer()
              ,access_level :: non_neg_integer() | 'infinity'}).

-record(session, {token :: binary()
                 ,owner_id :: non_neg_integer()
                 ,group :: 'guests'|'users'|'administrators'
                 ,ws_pid :: pid()
                 ,access_level = 'infinity' :: non_neg_integer() | 'infinity'
                 ,expiration_time :: non_neg_integer()}).

-record(index, {name :: binary()
               ,value :: non_neg_integer()}).

-endif.
