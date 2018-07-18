-module(bacnet_control).

-behaviour(xaptum_endpoint).

-include("bacnet.hrl").

%% xaptum_endpoint callbacks
-export([
  auth/3,
  on_receive/2,
  on_send/2,
  on_send/3,
  on_connect/2,
  on_reconnect/2,
  on_disconnect/2
]).

%% export API
-export([
  start/1,
  poll_loop/2
]).

start(Creds)->
  xaptum_endpoint_sup:create_endpoint(?MODULE, #bacnet_sub{}, Creds).

%%====================================
%% xaptum_endpoint callbacks
%%====================================

auth(#hosts_config{} = HostsConfig, Creds, #bacnet_sub{ dds = DdsData0 } = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:auth(HostsConfig, Creds, DdsData0),
  {ok, CallbackData#bacnet_sub{dds = DdsData1}}.

on_connect(TlsSocket, #bacnet_sub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:on_connect(TlsSocket, DdsData0),
  {ok, CallbackData#bacnet_sub{dds = DdsData1}}.

on_reconnect(TlsSocket, #bacnet_sub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:on_reconnect(TlsSocket, DdsData0),
  {ok, CallbackData#bacnet_sub{dds = DdsData1}}.

on_disconnect(TlsSocket, #bacnet_sub{ dds = DdsData0} = CallbackData) ->
  {ok, DdsData1} = dds_endpoint:on_disconnect(TlsSocket, DdsData0),
  {ok, CallbackData#bacnet_sub{dds = DdsData1}}.


%% READY RESPONSE RECEIVE
%% TODO when the session token concept goes away this initial receive should probably be on_(re)connect instead

%% MSG RECEIVE
on_receive(Msg, #bacnet_sub{dict = Dict, poll_reqs = PollReqs, dds = #dds{ready = true} = DdsCallbackData0}) ->
  %% Extract payload out of DDS message into Mdxp
  {ok, #dds{endpoint_data = #endpoint{msg = Mdxp}} = DdsCallbackData1} =
    dds_endpoint:on_receive(Msg, DdsCallbackData0),
  %% Decode the original msg
  OriginalMsg = base64:decode(ddslib:extract_mdxp_payload(Mdxp)),
  case OriginalMsg of
    <<?IAM, Ip/binary>> ->
      DestIp = xaptum_client:ipv6_binary_to_text(Ip),
      NewDict = dict:store(DestIp, 1, Dict),
      {ok, #bacnet_sub{dds = DdsCallbackData1, dict = NewDict}};
    BacnetAck ->
      process_bacnet_ack(BacnetAck),
      {ok, #bacnet_sub{dds = DdsCallbackData1, poll_reqs = PollReqs + 1}}
  end;
on_receive(Msg, #bacnet_sub{dds = #dds{ready = _Whatever} = DdsCallbackData0 } = CallbackData0) ->
  case dds_endpoint:on_receive(Msg, DdsCallbackData0) of
    {ok, #dds{ready = true} = DdsCallbackData1} ->
      CallbackData1 = CallbackData0#bacnet_sub{dds = DdsCallbackData1},
      {ok, Pid} = spawn_link(?MODULE, poll_loop, [write_poll, self()]),
      {ok, CallbackData1#bacnet_sub{poll_pid = Pid}};
    {error, Error} -> {error, Error}
  end.

%% CONTROL MESSAGE
on_send(Msg0, Dest, #bacnet_pub{dds = DdsCallbackData0} = CallbackData) ->
  {ok, Msg1, DdsCallbackData1} = dds_endpoint:on_send(Msg0, Dest, DdsCallbackData0),
  {ok, Msg1, CallbackData#bacnet_pub{dds = DdsCallbackData1}}.

%% REGULAR MESSAGE
on_send(Msg0, #bacnet_pub{dds = DdsCallbackData0} = CallbackData) ->
  {ok, Msg1, DdsCallbackData1} = dds_endpoint:on_send(Msg0, DdsCallbackData0),
  {ok, Msg1, CallbackData#bacnet_pub{dds = DdsCallbackData1}}.

%%====================================
%% Private functions
%%====================================
process_bacnet_ack(BacnetAck)->
  case bacnet_utils:get_apdu_from_message(BacnetAck) of
    {ok, Apdu} ->
      case bacnet_utils:get_pdu_type(Apdu) of
        pdu_type_simple_ack ->
          lager:info("Received bacnet Simple ACK");
        pdu_type_complex_ack ->
          case bacnet_utils:get_value_from_complex_ack(Apdu) of
            {ok, Id, Tag} ->
              lager:info("Received bacnet Complex ACK with Id: ~p, Tag: ~p", [Id, Tag]);
            CV ->
              lager:info("Got ~p while processing Complex Ack. Ignore", [CV])
          end
      end;
    ApduError ->
      lager:info("Got ~p while processing BacknetAck. Ignore", [ApduError])
  end.

poll_loop(write_poll, EndpointPid) ->
  timer:sleep(2500),
  #bacnet_sub{dict = Dict} = xaptum_endpoint:get_data(EndpointPid),
  Ips = dict:fetch_keys(Dict),
  lists:foreach( fun(Ip) ->
    IpBytes = xaptum_client:ipv6_to_binary(Ip),
    <<Id:64, Tag:64>> = IpBytes,
    {ok, Control} = bacnet_utils:build_write_property_request(Id, Tag),
    xaptum_endpoint:send_message(EndpointPid, Control, Ip),
    lager:info("Sending write property request with Id: ~p, Tag: ~p", [Id, Tag])
                 end, Ips),
  poll_loop(read_poll, EndpointPid);
poll_loop(read_poll, EndpointPid) ->
  timer:sleep(2500),
  #bacnet_sub{dict = Dict} = xaptum_endpoint:get_data(EndpointPid),
  Ips = dict:fetch_keys(Dict),
  lists:foreach( fun(Ip) ->
    {ok, Control} = bacnet_utils:build_read_property_request(),
    xaptum_endpoint:send_message(EndpointPid, Control, Ip),
    lager:info("Sending read property request")
   end, Ips),
  poll_loop(write_poll, EndpointPid).

