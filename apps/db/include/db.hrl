-ifndef(DB_HRL).
-define(DB_HRL, 'true').

-define(APP_NAME, <<"db">>).

-define(GET_FIELDS(Name), ?GET_FIELDS(Name, [])).
-define(GET_FIELDS(Name, Opts), {Name, [{attributes, record_info(fields, Name)}| Opts]}).

-record(record_name1, {fname, lname}).
-record(record_name2, {fname, lname, age, sex}).

-define(DEFAULT_SCHEMA, [?GET_FIELDS(record_name1)
                        ,?GET_FIELDS(record_name2)]).

-endif.
