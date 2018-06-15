-module(bacnet_proxy).

-behaviour(xaptum_endpoint).

-include("bacnet.hrl").
-include("xtt_endpoint.hrl").
-include("dds.hrl").

%% xaptum_endpoint callbacks
-export([
  auth/3,
  on_receive/2,
  do_receive/1,
  on_send/2,
  on_send/3,
  on_connect/1,
  on_reconnect/1,
  on_disconnect/1
]).

%% export API
-export([
  start/1,
  heartbeat_loop/2
]).

start(Creds)->
  xaptum_endpoint_sup:create_endpoint(?MODULE, #bacnet_pub{}, Creds).

%%====================================
%% xaptum_endpoint callbacks
%%====================================

auth(#hosts_config{} = HostsConfig, Creds, #bacnet_pub{ dds = DdsData0 } = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:auth(HostsConfig, Creds, DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.

on_connect(#bacnet_pub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:on_connect(DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.

on_reconnect(#bacnet_pub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:on_reconnect(DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.

on_disconnect(#bacnet_pub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:on_disconnect(DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.


%% READY RESPONSE RECEIVE
on_receive(Msg, #bacnet_pub{
  udp_socket = undefined,
  dds = #dds{ready = false} = DdsCallbackData0 } = CallbackData0) ->
  case dds_endpoint:on_receive(Msg, DdsCallbackData0) of
    {ok, #dds{ready = true} = DdsCallbackData1} ->
      CallbackData1 = CallbackData0#bacnet_pub{dds = DdsCallbackData1},
      {ok, Pid} = spawn_link(?MODULE, heartbeat_loop, [self(), CallbackData1]),
      {ok, UdpSocket} = gen_udp:open(8780, [binary, {active, false}]),
      {ok, CallbackData1#bacnet_pub{udp_socket = UdpSocket, heartbeat_pid = Pid}};
    {error, Error} -> {error, Error}
  end;
%% CONTROL MESSAGE RECEIVE
on_receive(Msg,
    #bacnet_pub{udp_socket = Socket, udp_recv = UdpRecv, udp_sent = UdpSent,
                dds = #dds{ready = true,
                           endpoint_data = #endpoint{msg = Bin} } = DdsCallbackData0 } = CallbackData0)
      when is_port(Socket) ->
  {ok, #dds{endpoint_data = #endpoint{msg = BacnetRequest}} = DdsCallbackData1} =
    dds_endpoint:on_receive(Msg, DdsCallbackData0),
  %% Send socket to 47808
  ok = gen_udp:send(Socket, {127,0,0,1}, 47808, BacnetRequest),
  %% Read the response from bacserv
  {ok, {_Address, _Port, BacnetAck}} = gen_udp:recv(Socket, 0, 5000),
  %% Now Send the ACK to control
  xaptum_endpoint:send_message(self(), BacnetAck),
  {ok, CallbackData0#bacnet_pub{dds = DdsCallbackData1, udp_recv = UdpRecv + 1, udp_sent = UdpSent + 1 }}.

do_receive(TlsSocket)->
  dds_endpoint:do_receive(TlsSocket).

on_send(Msg0, Dest, #bacnet_pub{dds = DdsCallbackData0} = CallbackData) ->
  {ok, Msg1, DdsCallbackData1} = dds_endpoint:on_send(Msg0, Dest, DdsCallbackData0),
  {ok, Msg1, CallbackData#bacnet_pub{dds = DdsCallbackData1}}.

on_send(Msg0, #bacnet_pub{dds = DdsCallbackData0} = CallbackData) ->
  {ok, Msg1, DdsCallbackData1} = dds_endpoint:on_send(Msg0, DdsCallbackData0),
  {ok, Msg1, CallbackData#bacnet_pub{dds = DdsCallbackData1}}.


%%====================================
%% Private functions
%%====================================

heartbeat_loop(EndpointPid, #bacnet_pub{dds = #dds{endpoint_data = #endpoint{ipv6 = Ipv6}}} = CallbackData) ->
  Msg = <<?IAM, Ipv6/binary>>,
  xaptum_endpoint:send_message(EndpointPid, Msg),
  timer:sleep(10000),
  heartbeat_loop(EndpointPid, CallbackData).