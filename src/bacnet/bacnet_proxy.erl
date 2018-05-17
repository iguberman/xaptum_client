-module(bacnet_proxy).

-behaviour(xaptum_endpoint).

-include("bacnet.hrl").
-include("xtt_endpoint.hrl").
-include("dds.hrl").

%% xaptum_endpoint callbacks
-export([
  auth/4,
  on_receive/3,
  do_receive/1,
  on_send/2,
  on_send/3,
  on_connect/2,
  on_reconnect/2,
  on_disconnect/2
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

auth(XttServerHost, XttServerPort, Creds, #bacnet_pub{ dds = DdsData0 } = CallbackData) ->
  {ok, DdsData1} = dds_pub:auth(XttServerHost, XttServerPort, Creds, DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.

on_connect(EndpointPid, #bacnet_pub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_pub:on_connect(EndpointPid, DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.

on_reconnect(EndpointPid, #bacnet_pub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_pub:on_reconnect(EndpointPid, DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.

on_disconnect(EndpointPid, #bacnet_pub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_pub:on_disconnect(EndpointPid, DdsData0),
  {ok, CallbackData#bacnet_pub{dds = DdsData1}}.


%% AUTH RESP RECEIVE
%% TODO when the session token concept goes away this initial receive should probably be on_(re)connect instead
on_receive(Msg, EndpointPid, #bacnet_pub{
  udp_socket = undefined,
  dds = #dds{session_token = SessionToken} = DdsCallbackData0 } = CallbackData0)
  when is_atom(SessionToken) ->
  case dds_pub:on_receive(Msg, EndpointPid, DdsCallbackData0) of
    {ok, #dds{session_token = SessionToken} = DdsCallbackData1}
      when is_binary(SessionToken), size(SessionToken) =:= ?SESSION_TOKEN_SIZE ->
      CallbackData1 = CallbackData0#bacnet_pub{dds = DdsCallbackData1},
      {ok, Pid} = spawn_link(?MODULE, heartbeat_loop, [CallbackData1]),
      {ok, UdpSocket} = gen_udp:open(8780, [binary, {active, false}]),
      {ok, CallbackData1#bacnet_pub{udp_socket = UdpSocket, heartbeat_pid = Pid}};
    {error, Error} -> {error, Error}
  end;
%% CONTROL MESSAGE RECEIVE
on_receive(Msg, EndpointPid,
    #bacnet_pub{udp_socket = Socket, udp_recv = UdpRecv, udp_sent = UdpSent,
                dds = #dds{session_token = SessionToken,
                           endpoint_data = #endpoint{msg = Bin} } = DdsCallbackData0 } = CallbackData0)
      when is_port(Socket), is_binary(SessionToken), size(SessionToken) =:= ?SESSION_TOKEN_SIZE ->
  {ok, #dds{endpoint_data = #endpoint{msg = BacnetRequest}} = DdsCallbackData1} =
    dds_pub:on_receive(Msg, EndpointPid, DdsCallbackData0),
  %% Send socket to 47808
  ok = gen_udp:send(Socket, {127,0,0,1}, 47808, BacnetRequest),
  %% Read the response from bacserv
  {ok, {_Address, _Port, BacnetAck}} = gen_udp:recv(Socket, 0, 5000),
  %% Now Send the ACK to control
  xaptum_endpoint:send_message(EndpointPid, BacnetAck),
  {ok, CallbackData0#bacnet_pub{dds = DdsCallbackData1, udp_recv = UdpRecv + 1, udp_sent = UdpSent + 1 }}.

do_receive(TlsSocket)->
  dds_pub:do_receive(TlsSocket).

on_send(Msg0, Dest, #bacnet_pub{dds = DdsCallbackData0} = CallbackData) ->
  {ok, Msg1, DdsCallbackData1} = dds_pub:on_send(Msg0, Dest, DdsCallbackData0),
  {ok, Msg1, CallbackData#bacnet_pub{dds = DdsCallbackData1}}.

on_send(Msg0, #bacnet_pub{dds = DdsCallbackData0} = CallbackData) ->
  {ok, Msg1, DdsCallbackData1} = dds_pub:on_send(Msg0, DdsCallbackData0),
  {ok, Msg1, CallbackData#bacnet_pub{dds = DdsCallbackData1}}.


%%====================================
%% Private functions
%%====================================

heartbeat_loop(EndpointPid, #bacnet_pub{dds = #dds{endpoint_data = #endpoint{ipv6 = Ipv6}}} = CallbackData) ->
  Msg = <<?IAM, Ipv6/binary>>,
  xaptum_endpoint:send_message(EndpointPid, Msg),
  timer:sleep(10000),
  heartbeat_loop(EndpointPid, CallbackData).