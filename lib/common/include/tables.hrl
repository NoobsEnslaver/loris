-ifndef(TABLES_HRL).
-define(TABLES_HRL, 'true').

-type access_group() :: 'guests'|'users'|'administrators'|'banned'.

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
                 ,access_level = 'infinity' :: non_neg_integer() | 'infinity'
                 ,expiration_time :: non_neg_integer()}).

-record(message, {msg_id :: non_neg_integer()
                 ,msg_body :: binary()
                 ,status :: 'pending' | 'delivered' | 'readed'
                 ,from :: non_neg_integer()}).

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

-record(pids, {msisdn :: non_neg_integer()
              ,pid :: pid()}).

-record(room, {id :: non_neg_integer()
              ,name :: binary()
              ,description :: binary()
              ,sub_rooms :: [#room{}]
              ,owner_id :: non_neg_integer()
              ,access :: map()                  %#{all => access_group(), MSISDN => access_group()} - access level for all, and exclusions
              ,chat_id :: binary()}).

-record(room_tag, {room_id,tag1 ,tag2 ,tag3 ,tag4 ,tag5 ,tag6 ,tag7 ,tag8 ,tag9 ,tag10
                          ,tag11,tag12,tag13,tag14,tag15,tag16,tag17,tag18,tag19,tag20
                          ,tag21,tag22,tag23,tag24,tag25,tag26,tag27,tag28,tag29,tag30
                          ,tag31,tag32,tag33,tag34,tag35,tag36,tag37,tag38,tag39,tag40
                          ,tag41,tag42,tag43,tag44,tag45,tag46,tag47,tag48,tag49,tag50
                          ,tag51,tag52,tag53,tag54,tag55,tag56,tag57,tag58,tag59,tag60
                          ,tag61,tag62,tag63,tag64}). %all boolean

-endif.
