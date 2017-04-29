-module(tester).

-include_lib("common/include/transport_lib.hrl").

-define(SERVER_ADDRESS, "127.0.0.1").
-define(SERVER_PORT, 8080).

%% -define(DBG, true).
-undef(DBG).

-ifdef(DBG).
-define(CONN_OPTS, #{trace => true}).
-else.
-define(CONN_OPTS, #{trace => false}).
-endif.
%%====================================================================
%% API functions
%%====================================================================
-export([connect_to_ws/2
        ,send_packet/3
        ,receive_packet/2
        ,disconnect/1
        ,authorize/2
        ,flush_messages/0]).

-spec authorize(binary(), binary()) -> binary().
authorize(MSISDN, Pwd) ->
    {ok, ConnPid} = gun:open(?SERVER_ADDRESS, ?SERVER_PORT, ?CONN_OPTS),
    case gun:await_up(ConnPid) of
        {ok, _} ->
            PwdHash = common:bin2hex(crypto:hash('md5', Pwd)),
            Body = transport_lib:encode(#{<<"msisdn">> => MSISDN, <<"password">> => PwdHash}, ?JSON),
            StreamRef = gun:post(ConnPid, "/v1/auth", [{<<"content-type">>, <<"application/", ?JSON/binary>>}], Body),
            Result = gun:await_body(ConnPid, StreamRef),
            gun:close(ConnPid),
            Result;
        _Error ->
            gun:close(ConnPid),
            _Error
    end.

-spec connect_to_ws(string(), string()) -> {ok, pid()}.
connect_to_ws(Address, Transport) ->            %Address = Tail of the address, "/ws/v1/chat"
    {ok, ConnPid} = gun:open(?SERVER_ADDRESS, ?SERVER_PORT, ?CONN_OPTS),
    case gun:await_up(ConnPid) of
        {ok, _} ->
            gun:ws_upgrade(ConnPid, Address, [{<<"sec-websocket-protocol">>, Transport}]),
            receive
                {gun_ws_upgrade, ConnPid, ok, _Headers} ->
                    {ok, ConnPid};
                {gun_response, ConnPid, _, _, _Status, _Headers} ->
                    {ws_upgrade_failed, _Status, _Headers};
                {gun_error, ConnPid, _StreamRef, Reason} ->
                    {ws_upgrade_failed, Reason}
            after 1000 ->
                    {error, timeout}
            end;
        _Error ->
            gun:close(ConnPid),
            _Error
    end.

-spec disconnect(pid()) -> ok.
disconnect(ConnPid) ->
    gun:close(ConnPid),
    timer:sleep(50),
    ok.

-spec send_packet(pid(), map() | ping, binary()) -> ok.
send_packet(Connection, ping, _Transport) ->
    gun:ws_send(Connection, ping);
send_packet(ConnPid, Data, Transport) when is_map(Data) ->
    BData = transport_lib:encode(Data, Transport),
    gun:ws_send(ConnPid, {binary, BData});
send_packet(ConnPid, BData, _Transport) when is_binary(BData) ->
    gun:ws_send(ConnPid, {binary, BData}).

receive_packet(_ConnPid, Transport)->
    receive
        {gun_ws, _ConnPid, {close, _, _}} -> {close, _ConnPid};
        {gun_ws, _ConnPid, {binary, Frame}} -> transport_lib:decode(Frame, Transport)
    after 500 -> {error, timeout}
    end.

flush_messages() ->
    receive
        _ -> flush_messages()
    after 50 -> ok
    end.
%%====================================================================
%% Internal functions
%%====================================================================
