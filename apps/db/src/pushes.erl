%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  09 Jul 2017
%%%-------------------------------------------------------------------
-module(pushes).
-include_lib("common/include/tables.hrl").
-compile({no_auto_import,[get/1, put/3]}).

-export([put/3
        ,delete/1
        ,pull_outdated/1
        ,get/1
        ,extract/2]).

put(MSISDN, Msg, ChatName) ->
    Fun = fun()->
                  case mnesia:read('pushes', MSISDN) of
                      [] ->
                          mnesia:write(#pushes{msisdn = MSISDN
                                              ,chat_name = ChatName
                                              ,last_msg = Msg
                                              ,count = 1
                                              ,timestamp = common:timestamp()});
                      [P] when is_record(P, 'pushes') ->
                          mnesia:write(P##pushes{chat_name = ChatName
                                                ,last_msg = Msg
                                                ,count = P#pushes.count + 1})
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

delete(MSISDN) ->
    Fun = fun()->
                  mnesia:delete({'pushes', MSISDN})
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

get(MSISDN) ->
    Fun = fun() ->
                  mnesia:read('pushes', MSISDN)
          end,
    case mnesia:transaction(Fun) of
        {atomic, Res} -> Res;
        _Error -> 'false'
    end.

pull_outdated(ExpirationTime) ->
    MatchHead = #pushes{msisdn='$1',chat_name='$2',last_msg='$3',count='$4',timestamp='$5'},
    Guard = {'>', '$5', ExpirationTime},
    Result = ['$1', '$2', '$3', '$4', '$5'],
    Fun = fun() ->
                  List = mnesia:select('pushes',[{MatchHead, [Guard], [Result]}]),
                  lists:map(fun([M,ChN,LaM,Count,T]) ->
                                    mnesia:delete({pushes, M}),
                                    #pushes{msisdn=M,chat_name=ChN,last_msg=LaM,count=LaM,timestamp=T}
                            end, List)
          end,
    case mnesia:transaction(Fun) of
        {atomic, [Res]} -> Res;
        _Error -> 'false'
    end.

%%%-------------------------------------------------------------------
%%% Data extractors
%%%-------------------------------------------------------------------
-spec extract(#pushes{}, msisdn|chat_name|last_msg|count|timestamp) -> non_neg_integer() | binary().
extract(#pushes{msisdn = MSISDN}, 'msisdn')-> MSISDN;
extract(#pushes{chat_name = C}, 'chat_name')-> C;
extract(#pushes{last_msg = L}, 'last_msg')-> L;
extract(#pushes{count = C}, 'count')-> C;
extract(#pushes{timestamp = T}, 'timestamp')-> T.
