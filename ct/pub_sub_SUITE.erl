%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2018 11:58 AM
%%%-------------------------------------------------------------------
-module(pub_sub_SUITE).
-author("iguberman").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_pub_sub/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("xaptum_client/include/xtt_endpoint.hrl").
-include_lib("xaptum_client/include/dds.hrl").

all() -> [test_pub_sub].

init_per_suite(Config)->
  application:ensure_all_started(lager),
  application:ensure_all_started(xaptum_client),
  Config.

end_per_suite(_Config) ->
  ok.

test_pub_sub(Config) ->
  DataDir = ?config(data_dir, Config),
  Creds = #file_creds{cred_dir = DataDir},
  {ok, Queue} = application:get_env(xaptum_client, dds_queue),
  {ok, Sub} = dds_sub:start(Creds, Queue),
  {ok, _SubSessionToken} = wait_for_session_token(Sub, undefined, fun(Data)-> #dds_sub_data{session_token = SessionToken} = Data, SessionToken end, 20000),
  {ok, Pub} = dds_pub:start(Creds),
  {ok, _PubSessionToken} = wait_for_session_token(Pub, undefined, fun(Data)->#dds_pub_data{session_token = SessionToken} = Data, SessionToken end, 20000),
  xaptum_endpoint:send_message(Sub, "Hello dds pub!"),
  timer:sleep(1000),
  PubData = xaptum_endpoint:get_data(Pub),
  #dds_pub_data{endpoint_data = #endpoint_data{num_received = NumPubReceived}} = PubData,
  ?assert(NumPubReceived =:= 1),
  lager:info("PubData is ~p", [NumPubReceived]),
  ct:print("PubData is ~p ~n", [NumPubReceived]),
  xaptum_endpoint:send_message(Pub, "Hello dds sub!"),
  timer:sleep(1000),
  SubData = xaptum_endpoint:get_data(Sub),
  #dds_sub_data{endpoint_data = #endpoint_data{num_received = NumSubReceived}} = SubData,
  ?assert(NumSubReceived =:= 1),
  lager:info("SubData is ~p", [SubData]),
  ct:print("SubData is ~p~n", [SubData]).

wait_for_session_token(_EndpointPid, _SessionToken, _SFFun, Timeout) when Timeout =< 0->
  {error, timeout};
wait_for_session_token(EndpointPid, SessionToken, SFFun, Timeout) when SessionToken =:= undefined; SessionToken =:= awaiting->
  timer:sleep(100),
  Data = xaptum_endpoint:get_data(EndpointPid),
  wait_for_session_token(EndpointPid, SessionToken, SFFun(Data), Timeout - 100);
wait_for_session_token(_EndpointPid, SessionToken, _SFFUn, Timeout) when is_binary(SessionToken)->
  lager:info("Got session token ~p after ~p ms", [SessionToken, Timeout]),
  {ok, SessionToken}.





