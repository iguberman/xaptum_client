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
-export([test_pub_sub/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("xtt_erlang/include/xtt.hrl").
-include_lib("xaptum_client/include/xtt_endpoint.hrl").
-include_lib("xaptum_client/include/dds.hrl").
-include_lib("xaptum_client/include/bacnet.hrl").


-define(MESSAGE_LATENCY, 2000).

-define(MB_PUBLIC_KEYS_DIR, "/opt/xaptum/public/group_public_keys").

-define(CRED_BASE_DIR, "/opt/xaptum/xaptum_client").

-define(GROUP_DIR, "GROUP").
-define(CERT_DIR, "CERT").
-define(XTT_CRED_DIR(Id), "MEMBER" ++ integer_to_list(Id)).
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
  xtt_client_utils:generate_credentials(1,1000, ?CRED_BASE_DIR),
  Config.

end_per_suite(Config) ->
%%  ct:print("Config: ~p~n", [Config]),
%%  GidFile = ?config(?GID_FILE_CONFIG, Config),
%%  ct:print("Deleting temporary file ~p", [GidFile]),
%%  file:delete(GidFile),
  ok.

test_pub_sub(Config) ->
  {NewConfig, PubFileCreds} = init_file_creds(Config, ?XTT_CRED_DIR(2)),

  timer:sleep(5000),

  Queues = application:get_env(xaptum_client, dds_queues, ["$rr:0"]),
  {ok, Sub} = dds_endpoint:start(?DEFAULT_SUBNET, Queues),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Sub),

  timer:sleep(5000),
  {ok, Pub} = dds_endpoint:start(PubFileCreds),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Pub),

  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = SendSequence}} = xaptum_endpoint:get_data(Pub),

  RegMessage1 = "Hello2 from pub!",
  test_pub_send_message(Pub, RegMessage1, 1),
  test_sub_recv_message(Sub, 1),

  RegMessage2 = "Hello2 from pub!",
  test_pub_send_message(Pub, RegMessage2, 2),
  test_sub_recv_message(Sub, 2),

  Signal1 = "Signal 1 from sub!",
  lager:info("Sending signal ~p to ~p", [Signal1, Ipv6]),

  test_sub_send_message(Sub, Signal1, Ipv6, 1),

  Signal2 = "Signal 2 from sub!",
  lager:info("Sending signal ~p to ~p", [Signal2, Ipv6]),

  test_sub_send_message(Sub, Signal2, Ipv6, 2),

  test_pub_recv_message(Pub, 2),

  ct:print("New config: ~p~n", [NewConfig]),
  NewConfig.


test_pub_sub_multi(Config)->

%%
%%test_bacnet(Config) ->
%%  {NewConfig, PubFileCreds} = init_file_creds(Config, ?XTT_CRED_DIR2),
%%
%%  timer:sleep(5000),
%%
%%  Queues = application:get_env(xaptum_client, dds_queues, ["$rr:0"]),
%%  {ok, Sub} = bacnet_control:start(?DEFAULT_SUBNET, Queues),
%%  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Sub),
%%
%%  timer:sleep(5000),
%%  {ok, BacnetProxy} = bacnet_proxy:start(PubFileCreds),
%%%% TODO wait for ready instead
%%  timer:sleep(1000),
%%
%%  #bacnet_pub{dds = #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = SendSequence}}} = xaptum_endpoint:get_data(BacnetProxy),
%%
%%  RegMessage1 = "Hello2 from pub!",
%%  test_pub_send_message(Pub, RegMessage1, 1),
%%  test_sub_recv_message(Sub, 1),
%%
%%  RegMessage2 = "Hello2 from pub!",
%%  test_pub_send_message(Pub, RegMessage2, 2),
%%  test_sub_recv_message(Sub, 2),
%%
%%  Signal1 = "Signal 1 from sub!",
%%  lager:info("Sending signal ~p to ~p", [Signal1, Ipv6]),
%%
%%  test_sub_send_message(Sub, Signal1, Ipv6, 1),
%%
%%  Signal2 = "Signal 2 from sub!",
%%  lager:info("Sending signal ~p to ~p", [Signal2, Ipv6]),
%%
%%  test_sub_send_message(Sub, Signal2, Ipv6, 2),
%%
%%  test_pub_recv_message(Pub, 2),
%%
%%  ct:print("New config: ~p~n", [NewConfig]),
%%  NewConfig.



%%%===================================================================
%%% Test utils
%%%===================================================================

test_pub_send_message(PubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(PubPid, Message),
  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = NumSent}} = PubData = xaptum_endpoint:get_data(PubPid),
  lager:info("Pub ~p expecting num_sent ~p, actual ~p", [Ipv6, SendSequence, NumSent]),
  #dds{endpoint_data = #endpoint{num_sent = SendSequence}} = PubData.

test_sub_send_message(SubPid, Message, SendSequence)->
  xaptum_endpoint:send_message(SubPid, Message),
  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = NumSent}} = SubData = xaptum_endpoint:get_data(SubPid),
  lager:info("Sub ~p expecting num_sent ~p, actual ~p", [Ipv6, SendSequence, NumSent]),
  SendSequence = NumSent.

test_sub_send_message(SubPid, Message, Dest, SendSequence)->
  xaptum_endpoint:send_message(SubPid, Message, Dest),
  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = NumSent}} = xaptum_endpoint:get_data(SubPid),
  lager:info("Sub ~p expecting num_sent ~p, actual ~p", [Ipv6, SendSequence, NumSent]),
  ok.

test_pub_recv_message(PubPid, RecvSequence)->
  timer:sleep(?MESSAGE_LATENCY),
  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_received = NumRecv}} = PubData = xaptum_endpoint:get_data(PubPid),
  lager:info("Pub ~p expecting num received ~p, actual ~p", [Ipv6, RecvSequence, NumRecv]),
  RecvSequence = NumRecv.

test_sub_recv_message(SubPid, RecvSequence)->
  timer:sleep(?MESSAGE_LATENCY),
  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_received = NumRecv}} = SubData = xaptum_endpoint:get_data(SubPid),
  lager:info("Sub ~p epecting num received ~p, actual ~p", [Ipv6, RecvSequence, NumRecv]),
  RecvSequence = NumRecv.

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
