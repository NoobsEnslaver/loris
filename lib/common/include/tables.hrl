-ifndef(TABLES_HRL).
-define(TABLES_HRL, 'true').

-type access_group() :: 'guest'|'sportsman'|'administrator'|'parent'|'trainer'.

-record(file, {id :: non_neg_integer()
              ,hash :: binary()
              ,content_type :: binary()
              ,name :: binary()
              ,data :: binary()
              ,owner_id :: non_neg_integer()
              ,size :: non_neg_integer()}).

-record(participant, {reward :: atom() %TODO: list rewards
                     ,points :: non_neg_integer()}).

-record(tournament, {id :: non_neg_integer()
                    ,city :: binary()
                    ,name :: binary()
                    ,judges :: [non_neg_integer()]
                    ,timestamp :: non_neg_integer()
                    ,participants :: #{MSISDN :: non_neg_integer() => #participant{}}}).

-record(sportsman_info, {height :: non_neg_integer()
                        ,weight :: non_neg_integer()
                        ,kyu :: non_neg_integer()
                        ,tournaments :: [non_neg_integer()]
                        ,affiliate_id :: non_neg_integer()
                        ,is_volunteer :: boolean()
                        ,is_on_team :: boolean()}).

-record(parent_info, {affiliate_id :: non_neg_integer()
                     ,parental_committee :: boolean()
                     ,is_volunteer :: boolean()}).

-record(trainer_info, {affiliate_id :: non_neg_integer()
                      ,trainer_committee :: boolean()
                      ,is_judge :: boolean()
                      ,is_department_head :: boolean()}).

-record(user, {msisdn :: non_neg_integer()
              ,group :: access_group()
              ,pwd_hash :: binary()
              ,fname :: binary()
              ,lname :: binary()
              ,age :: non_neg_integer()
              ,created :: non_neg_integer()
              ,access_level :: non_neg_integer() | 'infinity'
              ,chats_invatations = #{} :: #{binary() => 0..7}
              ,chats = #{} :: #{binary() => 0..7}
              ,rooms = #{} :: #{non_neg_integer() => 0..7}
              ,is_male :: boolean()
              ,muted_chats = [] :: [binary()]
              ,last_visit_timestamp :: non_neg_integer()
              ,city :: binary()
              ,special_info :: #sportsman_info{}|#trainer_info{}|#parent_info{}|'undefined'}).

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
                   ,users :: [non_neg_integer()]
                   ,chat_owner :: non_neg_integer()
                   ,on_room :: non_neg_integer() | 'undefined'}).

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
              ,subrooms :: [non_neg_integer()]
              ,owner_id :: non_neg_integer()
              ,room_access :: map()                  %#{default => 0..7, MSISDN => 0..7} - access level for all, and exclusions
              ,chat_access :: map()                  %#{default => 0..7, MSISDN => 0..7} - access level for all, and exclusions
              ,chat_id :: binary()
              ,additional_info :: any()}).

-record(room_tag, {room_id,tag1 ='false',tag2 ='false',tag3 ='false',tag4 ='false',tag5 ='false',tag6 ='false',tag7 ='false',tag8 ='false',tag9 ='false',tag10='false'
                          ,tag11='false',tag12='false',tag13='false',tag14='false',tag15='false',tag16='false',tag17='false',tag18='false',tag19='false',tag20='false'
                          ,tag21='false',tag22='false',tag23='false',tag24='false',tag25='false',tag26='false',tag27='false',tag28='false',tag29='false',tag30='false'
                          ,tag31='false',tag32='false', name :: binary()}).

-record(storage, {key :: any()
                 ,value :: any()}).

-record(resources, {name :: binary()
                   ,group :: binary()
                   ,value :: any()}).

-endif.
