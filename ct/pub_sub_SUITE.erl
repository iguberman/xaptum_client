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
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).
-export([test_pub_sub/1, test_devices/1, test_subs/1, send_reg_messages/3]).

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

-define(MESSAGE(Prefix, Id1, Id2), Prefix ++ integer_to_list(Id1) ++ "_" ++ integer_to_list(Id2)).

-define(GID_FILE_CONFIG, gid_file).

-define(NUM_DEVICES, 100).
-define(NUM_SUBS, 5).
-define(NUM_DEV_MESSAGES, 100).
-define(NUM_SUB_MESSAGES, 100).

all() -> [
%%  {group, simple},
  {group, large}
].

groups() -> [
  {simple, [sequence], [test_pub_sub]},
  {large, [parallel], [test_devices, test_subs]}
].

init_per_suite(Config)->
  application:ensure_all_started(lager),
  application:ensure_all_started(xaptum_client),
  xtt_client_utils:generate_credentials(1,1001, ?CRED_BASE_DIR),
  Config.

end_per_suite(Config) ->
%%  ct:print("Config: ~p~n", [Config]),
%%  GidFile = ?config(?GID_FILE_CONFIG, Config),
%%  ct:print("Deleting temporary file ~p", [GidFile]),
%%  file:delete(GidFile),
  ok.

init_per_group(large, Config)->
  DataDir = ?config(data_dir, Config),

  Subs = start_rr_subscribers(?NUM_SUBS),

  Devs = start_devices(DataDir, 1, ?NUM_DEVICES, ?NUM_DEV_MESSAGES),

  lager:info("*******************************************************************"),
  lager:info("********************** Started ~b subs and ~b devs *************", [length(Subs), length(Devs)]),
  lager:info("*******************************************************************"),

  Config ++ [{subs, Subs}, {devs, Devs}];

init_per_group(simple, Config)->
  Config.

end_per_group(_Any, _Config)->
  ok.

test_devices(Config)->
  lager:info("################ test_devices ######################"),

  Subs = ?config(subs, Config),
  Devs = ?config(devs, Config),

  %% NOTE inefficient comprehension is ok as long as we only have few of subs
  [send_signals(lists:nth(N, Subs), N, Devs, ?NUM_SUB_MESSAGES) || N <- lists:seq(1, length(Subs))],

  TotalSubMessages = ?NUM_SUBS * ?NUM_SUB_MESSAGES * ?NUM_DEVICES,

  verify_counts(TotalSubMessages, fun() -> count_sends(Subs) end),

  lager:info("################ test_devices:  verified SUB sends ######################"),

  verify_counts(TotalSubMessages, fun() -> count_receives(Devs) end),

  lager:info("################ test_devices:  verified DEV receives ######################"),

  Config.

test_subs(Config)->
  lager:info("################ test_subs ######################"),

  Subs = ?config(subs, Config),
  Devs = ?config(devs, Config),

  TotalDevMessages = ?NUM_DEVICES * ?NUM_DEV_MESSAGES,
  verify_counts(TotalDevMessages, fun() -> count_sends(Devs) end),
  verify_counts(TotalDevMessages, fun() -> count_receives(Subs) end),

  Config.

test_pub_sub(Config) ->
  DataDir = ?config(data_dir, Config),

  [Sub1, Sub2] = Subs = start_rr_subscribers(2),

  [Dev1] = Devs = start_devices(DataDir, 1,1),

  RegMessage1 = ?MESSAGE("DEV_", 1,1),
  RegMessage2 = ?MESSAGE("DEV_", 1,2),

  lager:info("Sending reg messages..."),

  xaptum_endpoint:send_message(Dev1, RegMessage1),
  xaptum_endpoint:send_message(Dev1, RegMessage2),

  lager:info("Verifying reg message sends"),

  verify_counts(2, fun() -> count_sends(Devs) end),

  lager:info("Verifying reg message receives"),

  verify_counts(2, fun() -> count_receives(Subs) end),

  Signal1 = "Signal 1 from sub1!",
  Signal2 = "Signal 2 from sub2!",

  #dds{endpoint_data = #endpoint{ipv6 = DestIpv6}} = xaptum_endpoint:get_data(Dev1),

  lager:info("Sending signal messages..."),
  xaptum_endpoint:send_message(Sub1, Signal1, DestIpv6),
  xaptum_endpoint:send_message(Sub2, Signal2, DestIpv6),

  lager:info("Verifying signal sends"),

  verify_counts(2, fun() -> count_sends(Subs) end),

  lager:info("Verifying signal receives"),

  verify_counts(2, fun() -> count_receives(Devs) end),

  xaptum_endpoint:disconnect(Sub1),
  xaptum_endpoint:disconnect(Sub2),
  xaptum_endpoint:disconnect(Dev1),

  Config.


%%%===================================================================
%%% Test utils
%%%===================================================================

start_devices(DataDir, StartDevices, EndDevices)->
  start_devices(DataDir, StartDevices, EndDevices, 0).

start_devices(DataDir, StartDevices, EndDevices, NumMessages) ->
  [begin
     timer:sleep(10),
     start_device(DataDir, N, NumMessages)
   end
    || N <- lists:seq(StartDevices, EndDevices)].

start_device(DataDir, N, NumMessages)->
  PubFileCreds = init_file_creds(DataDir, ?XTT_CRED_DIR(N)),
  {ok, Device} = dds_endpoint:start(PubFileCreds),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Device),
  lager:info("############## STARTING TO SEND ~b messages from device #~b", [NumMessages, N]),
  spawn(?MODULE, send_reg_messages, [Device, N, NumMessages]),
  Device.

send_signals(EndpointPid, EndpointSequence, DestinationPids, NumMessages) ->
  lager:info("###### START sending ~b signals from endpoint ~b  to ~b devices ########",
    [NumMessages, EndpointSequence, length(DestinationPids)]),
  Destinations =
    [begin
       #dds{endpoint_data = #endpoint{ipv6 = Ipv6}} = xaptum_endpoint:get_data(DestinationPid),
       Ipv6
     end || DestinationPid <- DestinationPids],
  [begin
     xaptum_endpoint:send_message(EndpointPid, ?MESSAGE("SIGNAL_", EndpointSequence, MN), DestinationIpv6),
     timer:sleep(10)
   end
    || DestinationIpv6 <- Destinations, MN <- lists:seq(1, NumMessages)],
  lager:info("###### DONE sending ~b signals from endpoint ~b to ~b destinations ########",
    [NumMessages, EndpointSequence, length(Destinations)]).

send_reg_messages(EndpointPid, EndpointSequence, NumMessages) ->
  [begin
     xaptum_endpoint:send_message(EndpointPid, ?MESSAGE("DEV_REG_", EndpointSequence, MN))
     %%,timer:sleep(10)
   end
    || MN <- lists:seq(1, NumMessages)].

start_rr_subscribers(NumSubs) ->
  Queues = application:get_env(xaptum_client, dds_queues, ["$rr:0"]),
  lists:map(fun(_N)-> start_rr_subscriber(Queues) end, lists:seq(1, NumSubs)).

start_rr_subscriber(Queues)->
  {ok, Sub} = dds_endpoint:start(?DEFAULT_SUBNET, Queues),
  {ok, true} = dds_endpoint:wait_for_endpoint_ready(Sub),
  Sub.

count_receives(Endpoints)->
  lager:info("Counting receives on ~p", [Endpoints]),
  lists:foldl(
    fun(EndpointPid, Acc) ->
      #dds{endpoint_data = #endpoint{num_received = NumRecv}} = xaptum_endpoint:get_data(EndpointPid),
      lager:info("~p received ~b", [EndpointPid, NumRecv]),
      Acc + NumRecv
    end,
    0,
    Endpoints).

count_sends(Endpoints)->
  lager:info("Counting sends on ~p", [Endpoints]),
  lists:foldl(
    fun(EndpointPid, Acc) ->
      #dds{endpoint_data = #endpoint{num_sent = NumSent}} = xaptum_endpoint:get_data(EndpointPid),
      lager:info("~p sent ~b", [EndpointPid, NumSent]),
      Acc + NumSent
    end,
    0,
    Endpoints).

verify_counts(ExpectedCount, CountFun)->
  wait_for_counts(-1, CountFun(), ExpectedCount, CountFun).

%% MATCH of actual and expected counts -> SUCCESS!
wait_for_counts(_PrivActualCount, ExpectedCount, ExpectedCount, _CountFun) ->
  lager:info("SUCCESS: got expected count ~b", [ExpectedCount]),
  ok;
%% actual count already more than expected -> FAIL
wait_for_counts(_PrivActualCount, ActualCount, ExpectedCount, _CountFun) when ActualCount > ExpectedCount ->
  true = {overflow, actual, ActualCount, expected, ExpectedCount};
%% made no progress since last time -> FAIL
wait_for_counts(PrivActualCount, ActualCount, ExpectedCount, _CountFun) when PrivActualCount >= ActualCount ->
  true = {waiting_wont_help_us, actual, ActualCount, expected, ExpectedCount};
%% actual count is increasing, keep trying
wait_for_counts(PrivActualCount, ActualCount, ExpectedCount, CountFun) when PrivActualCount < ActualCount ->
  timer:sleep(2000),
  NewActualCount = CountFun(),
  lager:info("............ wait_for_counts(~b, ~b, ~b)..............", [ActualCount, NewActualCount, ExpectedCount]),
  wait_for_counts(ActualCount, NewActualCount, ExpectedCount, CountFun).


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

%%
%%test_pub_send_message(PubPid, Message, SendSequence) ->
%%  xaptum_endpoint:send_message(PubPid, Message),
%%  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = NumSent}} = PubData = xaptum_endpoint:get_data(PubPid),
%%  lager:info("Pub ~p expecting num_sent ~p, actual ~p", [Ipv6, SendSequence, NumSent]),
%%  #dds{endpoint_data = #endpoint{num_sent = SendSequence}} = PubData.
%%
%%test_sub_send_message(SubPid, Message, SendSequence)->
%%  xaptum_endpoint:send_message(SubPid, Message),
%%  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = NumSent}} = SubData = xaptum_endpoint:get_data(SubPid),
%%  lager:info("Sub ~p expecting num_sent ~p, actual ~p", [Ipv6, SendSequence, NumSent]),
%%  SendSequence = NumSent.
%%
%%test_sub_send_message(SubPid, Message, Dest, SendSequence)->
%%  xaptum_endpoint:send_message(SubPid, Message, Dest),
%%  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_sent = NumSent}} = xaptum_endpoint:get_data(SubPid),
%%  lager:info("Sub ~p expecting num_sent ~p, actual ~p", [Ipv6, SendSequence, NumSent]),
%%  ok.
%%
%%test_pub_recv_message(PubPid, RecvSequence)->
%%  timer:sleep(?MESSAGE_LATENCY),
%%  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_received = NumRecv}} = PubData = xaptum_endpoint:get_data(PubPid),
%%  lager:info("Pub ~p expecting num received ~p, actual ~p", [Ipv6, RecvSequence, NumRecv]),
%%  RecvSequence = NumRecv.
%%
%%test_sub_recv_message(SubPid, RecvSequence)->
%%  timer:sleep(?MESSAGE_LATENCY),
%%  #dds{endpoint_data = #endpoint{ipv6 = Ipv6, num_received = NumRecv}} = SubData = xaptum_endpoint:get_data(SubPid),
%%  lager:info("Sub ~p epecting num received ~p, actual ~p", [Ipv6, RecvSequence, NumRecv]),
%%  RecvSequence = NumRecv.

init_file_creds(DataDir, MemberDir)->
  NullRequestedClientIdFile = filename:join([DataDir, ?REQUESTED_CLIENT_ID_FILE]),

  GroupDir = filename:join([?CRED_BASE_DIR, ?GROUP_DIR]),
  register_gpk_with_mb(GroupDir),

  xtt_endpoint:init_file_creds(
    NullRequestedClientIdFile,
    GroupDir,
    filename:join([DataDir, ?CERT_DIR]),
    filename:join([?CRED_BASE_DIR, MemberDir])).

register_gpk_with_mb(GroupDir)->
  case xtt_client_utils:generate_group_csv(GroupDir) of
    {ok, already_exists} -> ok;
    {ok, GidCsv} when is_list(GidCsv) ->
      %% TEMP workaround until IAM is ready
      Prompt = lists:flatten(io_lib:format(
        "Please copy ~p to all MBs' ~p directory.~nPress any key when finished:", [GidCsv, ?MB_PUBLIC_KEYS_DIR])),
      io:fread(Prompt, "~s")
  end.
