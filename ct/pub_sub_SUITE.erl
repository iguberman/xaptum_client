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
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([test_pub/1, test_sub/1, test_pub_sub/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("xtt_erlang/include/xtt.hrl").
-include_lib("xaptum_client/include/xtt_endpoint.hrl").
-include_lib("xaptum_client/include/dds.hrl").

-define(READY_WAIT_TIMEOUT, 20000).
-define(MESSAGE_LATENCY, 2000).

-define(MB_PUBLIC_KEYS_DIR, "/opt/xaptum/public/group_public_keys").

-define(GROUP_DIR, "GROUP").
-define(CERT_DIR, "CERT").
-define(PUB_CRED_DIR, "MEMBER1").
-define(SUB_CRED_DIR, "MEMBER2").

-define(GID_FILE_CONFIG, gid_file).


all() -> [
  {group, simple}
].

groups() -> [
  {simple, [sequence], [test_pub, test_sub]}
].

%%all() -> [test_pub, test_sub].

init_per_suite(Config)->
  CTPrivDir = ?config(priv_dir, Config),
  application:ensure_all_started(lager),
  application:ensure_all_started(xaptum_client),
  xtt_client_utils:generate_credentials(1,2, CTPrivDir),
  Config.

end_per_suite(Config) ->
  ct:print("Config: ~p~n", [Config]),
  GidFile = ?config(?GID_FILE_CONFIG, Config),
  ct:print("Deleting temporary file ~p", [GidFile]),
  file:delete(GidFile),
  ok.

test_pub(Config)->
  {NewConfig, FileCreds} = init_file_creds(Config, ?PUB_CRED_DIR),
  {ok, Pub} = dds_endpoint:start(FileCreds),
  {ok, _PubSessionToken} = wait_for_endpoint_ready(Pub, ?READY_WAIT_TIMEOUT),

  test_pub_send_message(Pub, "Hello from pub!", 1),
  test_pub_send_message(Pub, "Message 1 from pub!", 2),
  test_pub_send_message(Pub, "Message 2 from pub!", 3),
  ct:print("New config: ~p~n", [NewConfig]),
  NewConfig.

test_sub(Config)->
  {ok, Queues} = application:get_env(xaptum_client, dds_queues),
  {ok, Sub} = dds_endpoint:start(?DEFAULT_SUBNET, Queues),
  {ok, _SubSessionToken} = wait_for_endpoint_ready(Sub, ?READY_WAIT_TIMEOUT),

  test_sub_send_message(Sub, "Hello from sub!", 1),
  test_pub_send_message(Sub, "Message 1 from sub!", 2),
  test_pub_send_message(Sub, "Message 2 from sub!", 3),
  Config.

test_pub_sub(Config) ->
  {NewConfig, PubFileCreds} = init_file_creds(Config, ?PUB_CRED_DIR),

  {ok, Queues} = application:get_env(xaptum_client, dds_queues),
  {ok, Sub} = dds_endpoint:start(?DEFAULT_SUBNET, Queues),
  {ok, true} = wait_for_endpoint_ready(Sub, ?READY_WAIT_TIMEOUT),

  {ok, Pub} = dds_endpoint:start(PubFileCreds),
  {ok, true} = wait_for_endpoint_ready(Pub, ?READY_WAIT_TIMEOUT),

  test_pub_send_message(Pub, "Hello from pub!", 1),
  test_sub_recv_message(Sub, 1),

  test_sub_send_message(Sub, "Signal from sub!", 1),
  test_pub_recv_message(Pub, 1),

  ct:print("New config: ~p~n", [NewConfig]),
  NewConfig.


%%%===================================================================
%%% Test utils
%%%===================================================================

wait_for_endpoint_ready(Pub, Timeout)->
  wait_for_endpoint_ready(Pub, false, Timeout).

wait_for_endpoint_ready(_EndpointPid, false, Timeout) when Timeout =< 0->
  {error, timeout};
wait_for_endpoint_ready(EndpointPid, false, Timeout) ->
  timer:sleep(100),
  #dds{ready = Ready} = xaptum_endpoint:get_data(EndpointPid),
  wait_for_endpoint_ready(EndpointPid, Ready, Timeout - 100);
wait_for_endpoint_ready(_EndpointPid, true, Timeout) ->
  lager:info("Ready ~p after ~p ms", [?READY_WAIT_TIMEOUT - Timeout]),
  {ok, true}.

test_pub_send_message(PubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(PubPid, Message),
  PubData = xaptum_endpoint:get_data(PubPid),
  #dds{endpoint_data = #endpoint{num_sent = SendSequence}} = PubData.

test_sub_send_message(SubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(SubPid, Message),
  SubData = xaptum_endpoint:get_data(SubPid),
  #dds{endpoint_data = #endpoint{num_sent = SendSequence}} = SubData.

test_pub_recv_message(PubPid, RecvSequence)->
  timer:sleep(?MESSAGE_LATENCY),
  PubData = xaptum_endpoint:get_data(PubPid),
  #dds{endpoint_data = #endpoint{num_received = RecvSequence}} = PubData.

test_sub_recv_message(SubPid, RecvSequence)->
  timer:sleep(?MESSAGE_LATENCY),
  SubData = xaptum_endpoint:get_data(SubPid),
  #dds{endpoint_data = #endpoint{num_received = RecvSequence}} = SubData.

init_file_creds(Config, MemberDir)->
  DataDir = ?config(data_dir, Config),
  PrivDir = ?config(priv_dir, Config),

  NullRequestedClientIdFile = filename:join([DataDir, ?REQUESTED_CLIENT_ID_FILE]),

  GroupDir = filename:join([PrivDir, ?GROUP_DIR]),
  GidCsvFile = register_gpk_with_mb(GroupDir),

  {[{?GID_FILE_CONFIG, GidCsvFile} | Config ], xtt_endpoint:init_file_creds(
    NullRequestedClientIdFile,
    GroupDir,
    filename:join([DataDir, ?CERT_DIR]),
    filename:join([PrivDir, MemberDir]))}.

register_gpk_with_mb(GroupDir)->
  case xtt_client_utils:generate_group_csv(GroupDir) of
    {ok, already_exists} -> ok;
    {ok, GidCsv} when is_list(GidCsv) ->
      %% TEMP workaround until IAM is ready
      Prompt = lists:flatten(io_lib:format(
        "Please copy ~p to all MBs' ~p directory.~nPress any key when finished:", [GidCsv, ?MB_PUBLIC_KEYS_DIR])),
      io:fread(Prompt, "~s")
  end.
