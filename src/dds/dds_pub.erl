%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2018 4:14 PM
%%%-------------------------------------------------------------------
-module(dds_pub).
-author("iguberman").

-include("dds.hrl").

-behavior(xaptum_endpoint).

%% API
-export([start/1]).

%% xaptum_endpoint callbacks
-export([
  auth/3,
  on_send/2,
  on_send/3,
  on_receive/2,
  do_receive/1,
  on_connect/1,
  on_reconnect/1,
  on_disconnect/1
  ]).


start(Creds)->
  xaptum_endpoint_sup:create_endpoint(?MODULE, #dds{}, Creds).

%%%===================================================================
%%% xaptum_endpoint callbacks
%%%===================================================================

%% NOTE: this is TLS auth (not dds AUTH which no longer exists)
auth(#hosts_config{xaptum_host = _XttServerHost, xtt_port = _XttServerPort} = HostsConfig,
    Inputs, #dds{endpoint_data = EndpointData0} = CallbackData) ->
  {ok, TlsCreds, EndpointData1} =
    xtt_endpoint:auth(HostsConfig, Inputs, EndpointData0),
  {ok, TlsCreds, CallbackData#dds{endpoint_data = EndpointData1}}.

on_connect(#dds{endpoint_data = #endpoint{ipv6 = Ipv6, remote_ip = RemoteIp, remote_port = RemotePort}} = CallbackData) ->
  send_connect_event(Ipv6, RemoteIp, RemotePort, self()),
  {ok, CallbackData#dds{}}.

on_reconnect(#dds{
  endpoint_data = EndpointData0 = #endpoint{ipv6 = Ipv6}} = CallbackData) ->
  send_pub_auth_request(Ipv6, self()),
  {ok, EndpointData1} = xtt_endpoint:on_reconnect(EndpointData0),
  {ok, CallbackData#dds{endpoint_data = EndpointData1}}.

on_disconnect(CallbackData) -> {ok, CallbackData#dds{ready = false}}.

on_receive(<<?DDS_MARKER, ?READY, ?IPV6_SIZE:16, Ipv6:?IPV6_SIZE/bytes>>,
    #dds{endpoint_data = #endpoint{ipv6 = Ipv6}} = CallbackData)->
  lager:info("Device ~p Ready response received", [Ipv6]),
  {ok, CallbackData#dds{ready = true}};
on_receive(<<?DDS_MARKER, ReqType, _Size:16, _Payload/binary>>,
    #dds{ready = false})  when ReqType =:= ?SUB_REQ; ReqType =:= ?REG_MSG; ReqType =:= ?CONTROL_MSG ->
  lager:error("Got message type ~p request, while not ready to subscribe to queues or receive messages", [ReqType]),
  {error, not_ready};
on_receive(<<?DDS_MARKER, ?REG_MSG, Size:16, Payload/binary>>,
    #dds{ready = true, endpoint_data = EndpointData0} = CallbackData) ->
  Size = size(Payload), %% sanity check
  {ok, EndpointData1} = xtt_endpoint:on_receive(Payload, EndpointData0),
  lager:info("Message ~p received by subscriber ~p", [Payload, self()]),
  {ok, CallbackData#dds{endpoint_data = EndpointData1}};
on_receive(<<?DDS_MARKER, ?CONTROL_MSG, Size:16, Payload/binary>> = Msg,
       #dds{ready = true, endpoint_data = EndpointData0} = CallbackData) ->
  Size = size(Payload), %% sanity check
  {ok, EndpointData1} = xtt_endpoint:on_receive(Payload, EndpointData0),
  lager:info("Control message ~p received by ~p", [Msg, self()]),
  {ok, CallbackData#dds{endpoint_data = EndpointData1}};
on_receive(<<?DDS_MARKER, ?SUB_REQ, Size:16, Queue/bytes>>)->
  Size = size(Queue), %% sanity check



do_receive(TlsSocket)->
  ddslib:recv(TlsSocket).

%% CONTROL MESSAGE: normally a sub functionality
on_send(Msg, Dest, #dds{} = CallbackData)->
  dds_sub:on_send(Msg, Dest, CallbackData).

%% REG MSG
on_send(Msg0, #dds{endpoint_data = EndpointData0, ready = true} = CallbackData) ->
  {ok, Msg1, EndpointData1} = xtt_endpoint:on_send(Msg0, EndpointData0),
  Msg2 = ddslib:reg_msg_request(Msg1),
  {ok, Msg2, CallbackData#dds{endpoint_data = EndpointData1} };
on_send(_Msg, #dds{ready = false}) ->
  lager:error("Not ready to send messages ~p! Try again later", []),
  {error, retry_later}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_connect_event(Ipv6, RemoteIp, RemotePort, EndpointPid)->
  PubConnectEvent = ddslib:connect_event(Ipv6, RemoteIp, RemotePort),
  xaptum_endpoint:send_request(EndpointPid, PubConnectEvent).