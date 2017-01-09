%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2017
%%%-------------------------------------------------------------------
-module(db_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-define(TEST_INTENSITY, 10).
%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds, ?TEST_INTENSITY*5}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:ensure_all_started('db'),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop('db'),
    mnesia:delete_schema([node() | nodes()]).

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{'files', [{repeat_until_any_fail, ?TEST_INTENSITY}, parallel],
      ['test_files_write_read_data'
      ,'test_files_read_not_exists_data'
      ,'test_files_delete_data'
      ,'test_files_get_list'
      ,'test_files_get_list_by_owner_id']}
    ,{'users', [{repeat_until_any_fail, ?TEST_INTENSITY}, parallel],
      ['test_users_new'
      ,'test_users_new_exists'
      ,'test_users_new_get_by_id'
      ,'test_users_get_not_exists'
      ,'test_users_get_by_id_not_exists'
      ,'test_users_delete'
      ,'test_users_authorize_success'
      ,'test_users_authorize_not_exists'
      ,'test_users_authorize_failed']}
    ,{'sessions', [{repeat_until_any_fail, ?TEST_INTENSITY}, parallel],
      ['test_sessions_new'
      ,'test_sessions_new_replace'
      ,'test_sessions_delete'
      ,'test_sessions_delete_not_exists'
      ,'test_sessions_get_tokens'
      ,'test_sessions_get_by_owner_id'
      ,'test_sessions_get_by_owner_id_not_exists']}].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() ->
    [{'group', 'files'}
    ,{'group', 'users'}
    ,{'group', 'sessions'}].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%    Files
%%--------------------------------------------------------------------
test_files_write_read_data(_Config) ->
    RandData = crypto:strong_rand_bytes(32),
    Id = files:save(<<"1">>, <<"2">>, RandData, 4, 5),
    RandData = files:extract(files:get(Id), 'data'),
    'ok'.

test_files_read_not_exists_data(_Config) ->
    RandId = base64:encode(crypto:strong_rand_bytes(16)),
    'false' = files:get(RandId),
    'ok'.

test_files_delete_data(_Config) ->
    RandData = crypto:strong_rand_bytes(32),
    Id = files:save(<<"1">>, <<"2">>, RandData, 4, 5),
    files:delete(Id),
    'false' = files:get(Id),
    'ok'.

test_files_get_list(_Config) ->
    Id1 = files:save(<<"Name1">>, <<"2">>, crypto:strong_rand_bytes(32), 4, 5),
    Id2 = files:save(<<"Name2">>, <<"2">>, crypto:strong_rand_bytes(32), 4, 5),
    Id3 = files:save(<<"Name3">>, <<"2">>, crypto:strong_rand_bytes(32), 4, 5),
    P = fun(X)->
                Id4 = files:extract(X, 'hash'),
                Id4 == Id1 orelse Id4 == Id2 orelse Id4 == Id3
        end,
    List = files:get_list(),
    3 = length(lists:filter(P, List)),
    files:delete(Id1),
    files:delete(Id2),
    files:delete(Id3),
    List1 = files:get_list(),
    0 = length(lists:filter(P, List1)),
    'ok'.

test_files_get_list_by_owner_id(_Config) ->
    OwnerId = crypto:rand_uniform(1, 1000),
    Id1 = files:save(<<"Name1">>, <<"2">>, crypto:strong_rand_bytes(32), OwnerId, 5),
    Id2 = files:save(<<"Name2">>, <<"2">>, crypto:strong_rand_bytes(32), OwnerId, 5),
    Id3 = files:save(<<"Name3">>, <<"2">>, crypto:strong_rand_bytes(32), OwnerId, 5),
    P = fun(X)->
                Id4 = files:extract(X, 'hash'),
                OwnerId1 = files:extract(X, 'owner_id'),
                (Id4 == Id1 orelse Id4 == Id2 orelse Id4 == Id3) and (OwnerId1 == OwnerId)
        end,
    List = files:get_list_by_owner_id(OwnerId),
    3 = length(lists:filter(P, List)),
    files:delete(Id1),
    files:delete(Id2),
    files:delete(Id3),
    List1 = files:get_list(),
    0 = length(lists:filter(P, List1)),
    'ok'.

%%--------------------------------------------------------------------
%%    Users
%%--------------------------------------------------------------------
test_users_new(_Config)->
    Login = crypto:strong_rand_bytes(32),
    Password = <<"Password1">>,
    {MSec, Sec, _} = erlang:timestamp(),
    Created = MSec * 1000000 + Sec,
    User1 = users:new(Login, Password, <<"name">>, 0),
    Login = users:extract(User1, 'login'),
    'true' = (users:extract(User1, 'created') - Created) < 5,
    User1 = users:get(Login),
    'ok'.

test_users_new_exists(_Config)->
    Login = crypto:strong_rand_bytes(32),
    Password = <<"Password1">>,
    users:new(Login, Password, <<"name">>, 0),
    'exists' = users:new(Login, Password, <<"name">>, 0),
    'ok'.

test_users_new_get_by_id(_Config)->
    User1 = create_new_user(),
    Id = users:extract(User1, 'id'),
    User1 = users:get_by_id(Id),
    'ok'.

test_users_get_not_exists(_Config)->
    create_new_user(),
    'false' = users:get(crypto:strong_rand_bytes(32)),
    'ok'.

test_users_get_by_id_not_exists(_Config)->
    Id = crypto:rand_uniform(10000, 91000),
    'false' = users:get_by_id(Id),
    'ok'.

test_users_delete(_Config)->
    Login = users:extract(create_new_user(), 'login'),
    'ok' = users:delete(Login),
    'false' = users:get(Login),
    'ok'.

test_users_authorize_success(_Config)->
    User = create_new_user(),
    Login = users:extract(User, 'login'),
    PwdHash = users:extract(User, 'pwd_hash'),
    User = users:authorize(Login, PwdHash),
    'ok'.

test_users_authorize_not_exists(_Config)->
    User = create_new_user(),
    Login = users:extract(User, 'login'),
    Password = users:extract(User, 'pwd_hash'),
    'false' = users:authorize(Login, common:bin2hex(crypto:hash('md5', Password))),
    'ok'.

test_users_authorize_failed(_Config)->
    create_new_user(),
    'false' = users:authorize(crypto:strong_rand_bytes(32), <<"qwe">>),
    'ok'.

%%--------------------------------------------------------------------
%%    Sessions
%%--------------------------------------------------------------------
test_sessions_new(_Config)->
    User = create_new_user(),
    Token = sessions:new(User, self(), 10000),
    Session = sessions:get(Token),
    UserId = users:extract(User, 'id'),
    UserId = sessions:extract(Session, 'owner_id'),
    'ok'.

test_sessions_new_replace(_Config)->
    User = create_new_user(),
    Token = sessions:new(User, self(), 10000),
    Session = sessions:get(Token),
    ExpTime = sessions:extract(Session, 'expiration_time'),
    timer:sleep(1100),
    Token2 = sessions:new(User, self(), 10000),
    Session2 = sessions:get(Token2),
    ExpTime2 = sessions:extract(Session2, 'expiration_time'),
    io:format("ExpTime:~p~nExpTime2:~p~n", [ExpTime, ExpTime2]),
    'true' = Token /= Token2,
    'true' = (ExpTime < ExpTime2),
    'ok'.

test_sessions_delete(_Config)->
    User = create_new_user(),
    UserId = users:extract(User, 'id'),
    Token = sessions:new(User, self(), 10000),
    Session = sessions:get(Token),
    UserId = sessions:extract(Session, 'owner_id'),
    'ok' = sessions:delete(Token),
    'false' = sessions:get(Token),
    'ok'.

test_sessions_delete_not_exists(_Config)->
    'ok' = sessions:delete(crypto:strong_rand_bytes(32)),
    'ok'.

test_sessions_get_tokens(_Config)->
    User1 = create_new_user(),
    Token1 = sessions:new(User1, self(), 10000),
    User2 = create_new_user(),
    Token2 = sessions:new(User2, self(), 10000),
    User3 = create_new_user(),
    Token3 = sessions:new(User3, self(), 10000),
    Tokens = sessions:get_tokens(),
    'true' = lists:member(Token1, Tokens),
    'true' = lists:member(Token2, Tokens),
    'true' = lists:member(Token3, Tokens),
    'ok'.

test_sessions_get_by_owner_id(_Config)->
    User1 = create_new_user(),
    UserId = users:extract(User1, 'id'),
    sessions:new(User1, self(), 10000),
    sessions:new(create_new_user(), self(), 10000),
    sessions:new(create_new_user(), self(), 10000),
    Session = sessions:get_by_owner_id(UserId),
    UserId = sessions:extract(Session, 'owner_id'),
    'ok'.

test_sessions_get_by_owner_id_not_exists(_Config)->
    sessions:new(create_new_user(), self(), 10000),
    sessions:new(create_new_user(), self(), 10000),
    sessions:new(create_new_user(), self(), 10000),
    'false' = sessions:get_by_owner_id(crypto:rand_uniform(10000, 20000)),
    'ok'.

%% ---------------------------------
%% Internal functions
%% ---------------------------------
create_new_user()->
    Login1 = crypto:strong_rand_bytes(32),
    Password1 = <<"Password1">>,
    users:new(Login1, Password1, <<"name1">>, 1).
