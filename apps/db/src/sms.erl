%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2017
%%%-------------------------------------------------------------------
-module(sms).
-include_lib("common/include/tables.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-ifdef(TEST).
-define(CODE, 6666).
-else.
-define(CODE, crypto:rand_uniform(1000, 9999)).
-endif.

-export([new/1
        ,delete/1
        ,get/1
        ,extract/2
        ,update_timestamp/1]).

new(MSISDN) ->
    Code = ?CODE,
    {MSec, Sec, _} = erlang:timestamp(),
    Now = MSec * 1000000 + Sec,
    Fun = fun()->
                  case mnesia:read(sms, MSISDN) of
                      [] ->
                          mnesia:write(#sms{msisdn = MSISDN, code = Code, timestamp = Now}),
                          Code;
                      [#sms{timestamp = T, code = C}] ->
                          {exists, T, C}
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

delete(MSISDN) ->
    Fun = fun()->
                  mnesia:delete({'sms', MSISDN})
          end,
    mnesia:transaction(Fun).

get(MSISDN) ->
    Fun = fun() ->
                  mnesia:read(sms, MSISDN)
          end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> 'false';
        {atomic, [Res]} -> Res;
        _Error -> 'false'
    end.

update_timestamp(MSISDN) ->
    {MSec, Sec, _} = erlang:timestamp(),
    Now = MSec * 1000000 + Sec,
    Fun = fun()->
                  case mnesia:read(sms, MSISDN) of
                      [] ->
                          'not_exists';
                      [SMS] ->
                          mnesia:write(SMS#sms{timestamp = Now})
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#sms{}, msisdn|timestamp|code) -> non_neg_integer().
extract(#sms{msisdn = MSISDN}, 'msisdn')-> MSISDN;
extract(#sms{code = Code}, 'code')-> Code;
extract(#sms{timestamp = T}, 'timestamp')-> T.
