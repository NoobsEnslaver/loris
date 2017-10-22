%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created : 30 Jun 2017
%%%-------------------------------------------------------------------
-module(push_sup).
-include_lib("common/include/otp_definitions.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-ifdef(TEST).
-define(MOCK, begin
                  meck:new(fcm, [passthrough]),
                  meck:expect(fcm, start, fun(_,_) -> ok end),
                  meck:expect(fcm, sync_push, fun(_,_,_) -> [] end),
                  meck:new(apns, [passthrough]),
                  meck:expect(apns, connect, fun(_) -> {ok, self()} end),
                  meck:expect(apns, push_notification, fun(_,_,_) -> {200, ok, ok} end),
                  meck:expect(apns, push_notification, fun(_,_,_,_) -> {200, ok, ok} end),
                  meck:expect(apns, get_feedback, fun(_) -> ok end)
                  %% TODO: meck fcm
              end).
-else.
-define(MOCK, ok).
-endif.

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?MOCK,
    {ok, FcmApiKey} = application:get_env('push', 'fcm_api_key'),
    SupFlags = #{strategy => 'one_for_one',
                 intensity => 100,
                 period => 1},
    {ok, {SupFlags, [?SUPER('apns_sup')
                    ,?WORKER('push_apple_server', 'push_apple_server', 'start_link', [[{'timeout', 15000}]])
                    ,?WORKER('fcm', 'fcm', 'start_link', ['push_android_server', FcmApiKey])
                    ,?WORKER('push_sender')]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
