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
-export([test_pub/1, test_sub/1, test_pub_sub/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("xtt_erlang/include/xtt.hrl").
-include_lib("xaptum_client/include/xtt_endpoint.hrl").
-include_lib("xaptum_client/include/dds.hrl").

-define(SESSION_TOKEN_WAIT_TIMEOUT, 20000).
-define(MESSAGE_LATENCY, 2000).

-define(GROUP_DIR, "GROUP").
-define(CERT_DIR, "CERT").
-define(PUB_CRED_DIR, "MEMBER1").
-define(SUB_CRED_DIR, "MEMBER2").

all() -> [test_pub, test_sub].

init_per_suite(Config)->
  CTPrivDir = ?config(priv_dir, Config),
  application:ensure_all_started(lager),
  application:ensure_all_started(xaptum_client),
  xtt_client_utils:generate_credentials(1,2, CTPrivDir),
  Config.

end_per_suite(_Config) ->
  ok.

test_pub(Config)->
  FileCreds = init_file_creds(Config, ?PUB_CRED_DIR),
  {ok, Pub} = dds_pub:start(FileCreds),
  {ok, _PubSessionToken} = wait_for_pub_session_token(Pub, ?SESSION_TOKEN_WAIT_TIMEOUT),

  test_pub_send_message(Pub, "Hello from pub!", 1),
  test_pub_send_message(Pub, "Message 1 from pub!", 2),
  test_pub_send_message(Pub, "Message 2 from pub!", 3).

test_sub(Config)->
  FileCreds = init_file_creds(Config, ?SUB_CRED_DIR),
  {ok, Queue} = application:get_env(xaptum_client, dds_queue),
  {ok, Sub} = dds_sub:start(FileCreds, Queue),
  {ok, _SubSessionToken} = wait_for_sub_session_token(Sub, ?SESSION_TOKEN_WAIT_TIMEOUT),

  test_sub_send_message(Sub, "Hello from sub!", 1),
  test_pub_send_message(Sub, "Message 1 from sub!", 2),
  test_pub_send_message(Sub, "Message 2 from sub!", 3).

test_pub_sub(Config) ->
  PubFileCreds = init_file_creds(Config, ?PUB_CRED_DIR),
  SubFileCreds = init_file_creds(Config, ?SUB_CRED_DIR),

  {ok, Queue} = application:get_env(xaptum_client, dds_queue),
  {ok, Sub} = dds_sub:start(SubFileCreds, Queue),
  {ok, _SubSessionToken} = wait_for_sub_session_token(Sub, ?SESSION_TOKEN_WAIT_TIMEOUT),

  {ok, Pub} = dds_pub:start(PubFileCreds),
  {ok, _PubSessionToken} = wait_for_pub_session_token(Pub, ?SESSION_TOKEN_WAIT_TIMEOUT),

  test_pub_send_message(Pub, "Hello from pub!", 1),
  test_sub_recv_message(Sub, 1),

  test_sub_send_message(Sub, "Signal from sub!", 1),
  test_pub_recv_message(Pub, 1).


%%%===================================================================
%%% Test utils
%%%===================================================================

wait_for_pub_session_token(Pub, Timeout)->
  wait_for_session_token(Pub, undefined,
    fun(Data)->#dds_pub_data{session_token = SessionToken} = Data, SessionToken end,
    Timeout).

wait_for_sub_session_token(Sub, Timeout)->
  wait_for_session_token(Sub, undefined,
    fun(Data)-> #dds_sub_data{session_token = SessionToken} = Data, SessionToken end,
    Timeout).

wait_for_session_token(_EndpointPid, _SessionToken, _STFun, Timeout) when Timeout =< 0->
  {error, timeout};
wait_for_session_token(EndpointPid, SessionToken, STFun, Timeout) when SessionToken =:= undefined; SessionToken =:= awaiting->
  timer:sleep(100),
  Data = xaptum_endpoint:get_data(EndpointPid),
  wait_for_session_token(EndpointPid, STFun(Data), STFun, Timeout - 100);
wait_for_session_token(_EndpointPid, SessionToken, _STFun, Timeout) when is_binary(SessionToken)->
  lager:info("Got session token ~p after ~p ms", [SessionToken, Timeout]),
  {ok, SessionToken}.

test_pub_send_message(PubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(PubPid, Message),
  PubData = xaptum_endpoint:get_data(PubPid),
  #dds_pub_data{endpoint_data = #endpoint_data{num_sent = SendSequence}} = PubData.

test_sub_send_message(SubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(SubPid, Message),
  SubData = xaptum_endpoint:get_data(SubPid),
  #dds_sub_data{endpoint_data = #endpoint_data{num_sent = SendSequence}} = SubData.

test_pub_recv_message(PubPid, RecvSequence)->
  timer:sleep(?MESSAGE_LATENCY),
  PubData = xaptum_endpoint:get_data(PubPid),
  #dds_pub_data{endpoint_data = #endpoint_data{num_received = RecvSequence}} = PubData.

test_sub_recv_message(SubPid, RecvSequence)->
  timer:sleep(?MESSAGE_LATENCY),
  SubData = xaptum_endpoint:get_data(SubPid),
  #dds_sub_data{endpoint_data = #endpoint_data{num_received = RecvSequence}} = SubData.

init_file_creds(Config, MemberDir)->
  DataDir = ?config(data_dir, Config),
  PrivDir = ?config(priv_dir, Config),

  NullRequestedClientIdFile = filename:join([DataDir, ?REQUESTED_CLIENT_ID_FILE]),

  xtt_endpoint:init_file_creds(
    NullRequestedClientIdFile,
    filename:join([PrivDir, ?GROUP_DIR]),
    filename:join([DataDir, ?CERT_DIR]),
    filename:join([PrivDir, MemberDir])).