-ifndef(DB_HRL).
-define(DB_HRL, 'true').

-include_lib("common/include/tables.hrl").

-define(APP_NAME, <<"db">>).

-define(GET_FIELDS(Name), ?GET_FIELDS(Name, [])).
-define(GET_FIELDS(Name, Opts), {Name, [{attributes, record_info(fields, Name)}| Opts]}).

-define(DEFAULT_SCHEMA, [?GET_FIELDS(file, [{index, [#file.owner_id]}, {disc_only_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(session, [{index, [#session.owner_id]}, {ram_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(user, [{disc_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(index, [{disc_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(chat_info, [{disc_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(sms, [{ram_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(device, [{disc_copies,[node() | nodes()]}, {type, bag}, {index, [#device.id]}])
                        ,?GET_FIELDS(call_offer, [{ram_copies,[node() | nodes()]}, {index, [#call_offer.msisdn_to]}])
                        ,?GET_FIELDS(user_subscribe, [{ram_copies,[node() | nodes()]}, {type, bag}, {index, [#user_subscribe.subscriber]}])
                        ]).

-endif.
