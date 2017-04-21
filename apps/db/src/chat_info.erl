%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created :  18 Apr 2017
%%%-------------------------------------------------------------------
-module(chat_info).
-include("db.hrl").
-compile({no_auto_import,[get/1]}).
-export([new/3
        ,get/1
        ,delete/1
        ,rename/2
        ,add_user/2
        ,remove_user/2
        ]).

new(_ChatId, _Name, _Users)->
    ok.

get(_ChatId) ->
    ok.

delete(_ChatId) ->
    ok.

rename(_ChatId, _Name) ->
    ok.

add_user(_ChatId, _MSISDN) ->
    ok.

remove_user(_ChatId, _MSISDN) ->
    ok.
