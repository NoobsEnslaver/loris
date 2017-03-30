%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2017

-module(common_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

bin2hex_test()->
    RawHash = crypto:hash(md5, <<"Hello world!">>),
    Hash = common:bin2hex(RawHash),
    ?assertEqual(<<"86FB269D190D2C85F6E0468CECA42A20">>, Hash).
