-ifndef(TABLES_HRL).
-define(TABLES_HRL, 'true').

-type access_group() :: 'guests'|'users'|'administrators'.

-record(file, {hash :: binary()
              ,content_type :: binary()
              ,name :: binary()
              ,data :: binary()
              ,owner_id :: non_neg_integer()
              ,size :: non_neg_integer()
              ,access_level :: non_neg_integer() | 'infinity'}).

-record(user, {msisdn :: non_neg_integer()
              ,group :: access_group()
              ,pwd_hash :: binary()
              ,fname :: binary()
              ,lname :: binary()
              ,age :: non_neg_integer()
              ,created :: non_neg_integer()
              ,access_level :: non_neg_integer() | 'infinity'
              ,chats_invatations = [] :: [{binary(), access_group()}]
              ,chats = [] :: [{binary(), access_group()}]
              ,rooms = [] :: [{binary(), access_group()}]
              ,is_male :: boolean()
              ,muted_chats = [] :: [binary()]}).

-record(session, {token :: binary()
                 ,owner_id :: non_neg_integer()
                 ,group :: access_group()
                 ,ws_pid :: pid()
                 ,access_level = 'infinity' :: non_neg_integer() | 'infinity'
                 ,expiration_time :: non_neg_integer()}).

-record(message, {msg_id :: non_neg_integer()
                 ,msg_body :: binary()
                 ,timestamp :: non_neg_integer()
                 ,status :: 'pending' | 'delivered' | 'readed'}).

-record(chat_info, {chat_id :: binary()
                   ,name :: binary()
                   ,users :: [non_neg_integer()]
                   ,chat_owner :: non_neg_integer()}).

-record(index, {name :: binary()
               ,value :: non_neg_integer()}).

-endif.
