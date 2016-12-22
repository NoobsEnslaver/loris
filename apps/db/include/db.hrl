-ifndef(DB_HRL).
-define(DB_HRL, 'true').

-include_lib("common/include/tables.hrl").

-define(APP_NAME, <<"db">>).

-define(GET_FIELDS(Name), ?GET_FIELDS(Name, [])).
-define(GET_FIELDS(Name, Opts), {Name, [{attributes, record_info(fields, Name)}| Opts]}).

-define(DEFAULT_SCHEMA, [?GET_FIELDS(file, [{disc_only_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(session, [{ram_copies,[node() | nodes()]}])
                        ,?GET_FIELDS(user, [{index, [#user.id]}])
                        ]).

-endif.
