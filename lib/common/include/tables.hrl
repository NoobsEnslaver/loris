-ifndef(TABLES_HRL).
-define(TABLES_HRL, 'true').

-type access_group() :: 'guests'|'users'|'administrators'.

-record(file, {id :: non_neg_integer()
              ,hash :: binary()
              ,content_type :: binary()
              ,name :: binary()
              ,data :: binary()
              ,owner_id :: non_neg_integer()
              ,size :: non_neg_integer()
              ,access_level :: non_neg_integer() | 'infinity'}).

-record(user, {login :: binary()
              ,id :: non_neg_integer()
              ,group :: access_group()
              ,pwd_hash :: binary()
              ,name :: binary()
              ,created :: non_neg_integer()
              ,access_level :: non_neg_integer() | 'infinity'}).

-record(session, {token :: binary()
                 ,owner_id :: non_neg_integer()
                 ,group :: access_group()
                 ,ws_pid :: pid()
                 ,access_level = 'infinity' :: non_neg_integer() | 'infinity'
                 ,expiration_time :: non_neg_integer()}).

-record(index, {name :: binary()
               ,value :: non_neg_integer()}).

-endif.
