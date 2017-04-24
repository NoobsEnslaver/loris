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
                        ,?GET_FIELDS(message, [{disc_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(chat_info, [{disc_copies,[node() | nodes()]}])
                        ]).

-endif.
