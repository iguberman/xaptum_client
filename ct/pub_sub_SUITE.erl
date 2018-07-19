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


-compile([{parse_transform, lager_transform}]).

%% API
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([test_pub/1, test_sub/1, test_pub_sub/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("xtt_erlang/include/xtt.hrl").
-include_lib("xaptum_client/include/xtt_endpoint.hrl").
-include_lib("xaptum_client/include/dds.hrl").


-define(MESSAGE_LATENCY, 2000).

-define(MB_PUBLIC_KEYS_DIR, "/opt/xaptum/public/group_public_keys").

-define(CRED_BASE_DIR, "/opt/xaptum/xaptum_client").

-define(GROUP_DIR, "GROUP").
-define(CERT_DIR, "CERT").
-define(XTT_CRED_DIR1, "MEMBER1").
-define(XTT_CRED_DIR2, "MEMBER2").


-define(REMOTE_IP1, <<0,0,0,0,0,0,0,0,0,0,255,255,1,1,1,1>>).
-define(REMOTE_IP1_INT, 281470698586369).
-define(REMOTE_IP2, <<0,0,0,0,0,0,0,0,0,0,255,255,2,2,2,2>>).
-define(REMOTE_IP2_INT, 281470715429378).
-define(REMOTE_PORT1, 42222).
-define(REMOTE_PORT2, 41111).


-define(GID_FILE_CONFIG, gid_file).


all() -> [
  {group, simple}
].

groups() -> [
  {simple, [sequence], [test_pub_sub]}
].

%%all() -> [test_pub, test_sub].

init_per_suite(Config)->
  application:ensure_all_started(lager),
  application:ensure_all_started(xaptum_client),
  xtt_client_utils:generate_credentials(1,2, ?CRED_BASE_DIR),
  Config.

end_per_suite(Config) ->
%%  ct:print("Config: ~p~n", [Config]),
%%  GidFile = ?config(?GID_FILE_CONFIG, Config),
%%  ct:print("Deleting temporary file ~p", [GidFile]),
%%  file:delete(GidFile),
  ok.

test_pub(Config)->
  {NewConfig, FileCreds} = init_file_creds(Config, ?XTT_CRED_DIR1),
  {ok, Pub} = dds_endpoint:start(FileCreds, {?REMOTE_IP1, ?REMOTE_PORT1}),
  ct:print("Pub endpoint started: ~p", [Pub] ),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Pub),

  test_pub_send_message(Pub, "Hello from pub!", 1),
  timer:sleep(100),
  test_pub_send_message(Pub, "Message 1 from pub!", 2),
  timer:sleep(100),
  test_pub_send_message(Pub, "Message 2 from pub!", 3),
  timer:sleep(100),
  ct:print("New config: ~p~n", [NewConfig]),

  timer:sleep(5000),

  NewConfig.

test_sub(Config)->
  Queues = application:get_env(xaptum_client, dds_queues, ["$rr:0"]),
  {ok, Sub} = dds_endpoint:start(?DEFAULT_SUBNET, Queues, {?REMOTE_IP2, ?REMOTE_PORT2}),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Sub),

  test_sub_send_message(Sub, "Hello from sub!", 1),
  timer:sleep(100),
  test_sub_send_message(Sub, "Message 1 from sub!", 2),
  timer:sleep(100),
  test_sub_send_message(Sub, "Message 2 from sub!", 3),
  timer:sleep(100),

  timer:sleep(5000),

  Config.

test_pub_sub(Config) ->
  {NewConfig, PubFileCreds} = init_file_creds(Config, ?XTT_CRED_DIR2),

  timer:sleep(10000),

  Queues = application:get_env(xaptum_client, dds_queues, ["$rr:0"]),
  {ok, Sub} = dds_endpoint:start(?DEFAULT_SUBNET, Queues, {?REMOTE_IP1, ?REMOTE_PORT1}),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Sub),

  timer:sleep(10000),
  {ok, Pub} = dds_endpoint:start(PubFileCreds, {?REMOTE_IP2, ?REMOTE_PORT2}),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Pub),

  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = SendSequence}} = xaptum_endpoint:get_data(Pub),

  RegMessage = "Hello from pub!",
  test_pub_send_message(Pub, RegMessage, 1),
  test_sub_recv_message(Sub, 1),

  Signal = "Signal from sub!",
  lager:info("Sending signal ~p to ~p", [Signal, Ipv6]),

  test_sub_send_message(Sub, Signal, Ipv6, 1),

  test_pub_recv_message(Pub, 1),

  ct:print("New config: ~p~n", [NewConfig]),
  NewConfig.


%%%===================================================================
%%% Test utils
%%%===================================================================

test_pub_send_message(PubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(PubPid, Message),
  PubData = xaptum_endpoint:get_data(PubPid),
  #dds{endpoint_data = #endpoint{num_sent = SendSequence}} = PubData.

test_sub_send_message(SubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(SubPid, Message),
  SubData = xaptum_endpoint:get_data(SubPid),
  #dds{endpoint_data = #endpoint{num_sent = SendSequence}} = SubData.

test_sub_send_message(SubPid, Message, Dest, SendSequence)->
  xaptum_endpoint:send_message(SubPid, Message, Dest),
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

  NullRequestedClientIdFile = filename:join([DataDir, ?REQUESTED_CLIENT_ID_FILE]),

  GroupDir = filename:join([?CRED_BASE_DIR, ?GROUP_DIR]),
  GidCsvFile = register_gpk_with_mb(GroupDir),

  {[{?GID_FILE_CONFIG, GidCsvFile} | Config ], xtt_endpoint:init_file_creds(
    NullRequestedClientIdFile,
    GroupDir,
    filename:join([DataDir, ?CERT_DIR]),
    filename:join([?CRED_BASE_DIR, MemberDir]))}.

register_gpk_with_mb(GroupDir)->
  case xtt_client_utils:generate_group_csv(GroupDir) of
    {ok, already_exists} -> ok;
    {ok, GidCsv} when is_list(GidCsv) ->
      %% TEMP workaround until IAM is ready
      Prompt = lists:flatten(io_lib:format(
        "Please copy ~p to all MBs' ~p directory.~nPress any key when finished:", [GidCsv, ?MB_PUBLIC_KEYS_DIR])),
      io:fread(Prompt, "~s")
  end.
