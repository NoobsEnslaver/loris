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
              ,muted_chats = [] :: [binary()]
              ,last_visit_timestamp :: non_neg_integer()}).

-record(session, {token :: binary()
                 ,owner_id :: non_neg_integer()
                 ,group :: access_group()
                 ,ws_pid :: pid()
                 ,access_level = 'infinity' :: non_neg_integer() | 'infinity'
                 ,expiration_time :: non_neg_integer()}).

-record(message, {msg_id :: binary()
                 ,msg_body :: binary()
                 ,timestamp :: non_neg_integer()
                 ,status :: 'pending' | 'delivered' | 'readed'
                 ,from :: binary()}).

-record(chat_info, {chat_id :: binary()
                   ,name :: binary()
                   ,users :: [binary()]
                   ,chat_owner :: non_neg_integer()}).

-record(index, {name :: binary()
               ,value :: non_neg_integer()}).

-record(sms, {msisdn :: non_neg_integer()
             ,code :: non_neg_integer()
             ,timestamp :: non_neg_integer()}).

-record(device, {msisdn :: non_neg_integer()
                ,id :: binary()
                ,type :: 'android' | 'ios' | 'ios_voip'
                ,push_token :: binary()}).

-record(user_subscribe, {msisdn :: non_neg_integer()
                        ,subscriber :: non_neg_integer()}).

-record(pushes, {msisdn :: non_neg_integer()
                ,timestamp :: non_neg_integer()
                ,count :: non_neg_integer()
                ,last_msg :: binary()
                ,chat_name :: binary()}).

-endif.
