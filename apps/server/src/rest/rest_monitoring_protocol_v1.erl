%%%-------------------------------------------------------------------
%%% @author Vorontsov Nikita <noobsenslaver@mail.ru>
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created :  27 May 2017
%%%-------------------------------------------------------------------
-module(rest_monitoring_protocol_v1).
-behaviour(rest_protocol_behaviour).
-include("server.hrl").

-export([handle/4
        ,access_level/1
        ,allowed_groups/1]).

-spec handle(method(), cowboy_req:req(), #q_state{}, [binary()]) -> {cowboy_req:req(), #q_state{}, [binary()]}.
handle(<<"GET">>, Req, #q_state{headers = Hdrs, body = Body} = State, []) ->
    {'ok', BData} = file:read_file(code:priv_dir(binary_to_atom(?APP_NAME, 'utf8')) ++ "/monitoring1.html"),
    NewHeaders = Hdrs#{<<"content-type">> => <<"text/html">>},
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>, headers = NewHeaders}, []};
handle(<<"GET">>, Req, #q_state{body = Body} = State, [<<"list">> | _Other]) ->
    Metrics = folsom_metrics:get_metrics(),
    BMetrics = [common:stringify(X) || X <- Metrics],
    BData = transport_lib:encode(BMetrics, ?JSON),
    {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>}, _Other};
handle(<<"GET">>, Req, #q_state{body = Body} = State, [Name | _Other]) ->
    case folsom_metrics:metric_exists(Name) of
        'true' ->
           BData = case folsom_metrics:get_metric_value(Name) of
                       MetricValue when is_number(MetricValue) ->
                           transport_lib:encode(#{Name => MetricValue}, ?JSON);
                       [] ->
                           transport_lib:encode(#{Name => []}, ?JSON);
                       [{_, [{_, _} | _]} | _] = MetricValue -> %list of proplists
                           ResultMap = lists:foldl(fun({T, X}, Map)->
                                                           maps:put(T, maps:from_list(X), Map)
                                                   end, #{}, MetricValue),
                           transport_lib:encode(ResultMap, ?JSON);
                       [{_, _} | _] = MetricValue -> %Proplist
                           ListMap =  maps:from_list(proplists:delete(acceleration, MetricValue)),
                           transport_lib:encode(ListMap, ?JSON)
                   end,
            {Req, State#q_state{code = 200, body = <<Body/binary, BData/binary>>}, _Other};
        'false'->
            {Req, State#q_state{code = 404}, []}
    end;
%% handle(<<"GET">>, Req, #q_state{body = Body} = State, _Other) ->
handle(_Method, Req, State, _Other)->
    {Req, State#q_state{code = 405}, []}.       %method not allowed

access_level(_Method) ->
    'infinity'.

allowed_groups(_Method) ->
    ['users', 'administrators', 'guests', 'company'].      %TODO: administrators only

%%
%% folsom_metrics:get_metric_value(<<"rest_monitoring_protocol_v1_meter">>).
%%
