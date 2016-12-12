-ifndef(TABLES_HRL).
-define(TABLES_HRL, 'true').

-record(record_name1, {fname, lname}).
-record(record_name2, {fname, lname, age, sex}).

-record(files, {hash::binary()
               ,content_type::binary()
               ,name::binary()
               ,data::binary()
               ,owner_id::binary()}).

-endif.
