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

auth(#hosts_config{xaptum_host = XttServerHost, xtt_port = XttServerPort},
    Inputs, #dds{endpoint_data = EndpointData0} = CallbackData) ->
  {ok, TlsCreds, EndpointData1} =
    xtt_endpoint:auth(XttServerHost, XttServerPort, Inputs, EndpointData0),
  {ok, TlsCreds, CallbackData#dds{endpoint_data = EndpointData1}}.

on_connect(#dds{endpoint_data = #endpoint{ipv6 = Ipv6}} = CallbackData) ->
  send_pub_auth_request(Ipv6, self()),
  {ok, CallbackData#dds{session_token = awaiting}}.

on_reconnect(#dds{
  endpoint_data = EndpointData0 = #endpoint{ipv6 = Ipv6}} = CallbackData) ->
  send_pub_auth_request(Ipv6, self()),
  {ok, EndpointData1} = xtt_endpoint:on_reconnect(EndpointData0),
  {ok, CallbackData#dds{session_token = awaiting, endpoint_data = EndpointData1}}.

on_disconnect(CallbackData) -> {ok, CallbackData}.

on_receive(<<?DDS_MARKER, ?SIGNAL_MSG, Size:16, SessionToken:?SESSION_TOKEN_SIZE/bytes, Payload/binary>> = Msg,
       #dds{session_token = SessionToken,
      endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  Size = ?SESSION_TOKEN_SIZE + size(Payload), %% sanity check
  {ok, EndpointData1} = xtt_endpoint:on_receive(Payload, EndpointData0),
  lager:info("Control message ~p received by ~p", [Msg, self()]),
  {ok, CallbackData#dds{endpoint_data = EndpointData1}};

on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, SessionToken:?SESSION_TOKEN_SIZE/bytes>>,
          #dds{session_token = awaiting, endpoint_data = #endpoint{ipv6 = Ipv6}} = CallbackData)->
  lager:info("Device ~p Auth response received", [Ipv6]),
  {ok, CallbackData#dds{session_token = SessionToken}};

on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, _SessionToken:?SESSION_TOKEN_SIZE/bytes>>,
          #dds{session_token = _NotAwaiting, endpoint_data = #endpoint{ipv6 = Ipv6}})->
  lager:error("Device ~p Auth response received out of sync", [Ipv6]),
  {error, auth_out_of_sync};

on_receive(<<?DDS_MARKER, ?SIGNAL_MSG, Size:16, _DdsPayload:Size/bytes, _ActualMsg/binary>>,
          #dds{session_token = SessionToken, endpoint_data = #endpoint{ipv6 = Ipv6}})
  when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Device ~p not ready to receive control messages, session token is still ~p", [Ipv6, SessionToken]),
  {error, not_ready}.

do_receive(TlsSocket)->
  ddslib:recv(TlsSocket).

%% CONTROL MESSAGE: normally a sub functionality
on_send(Msg, Dest, #dds{} = CallbackData)->
  dds_sub:on_send(Msg, Dest, CallbackData).

%% REG MSG
on_send(Msg0, #dds{session_token = SessionToken, endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {ok, Msg1, EndpointData1} = xtt_endpoint:on_send(Msg0, EndpointData0),
  Msg2 = ddslib:build_reg_message(SessionToken, Msg1),
  {ok, Msg2, CallbackData#dds{endpoint_data = EndpointData1} };
on_send(_Msg, #dds{session_token = SessionToken}) when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Can't send reg message when session token is still ~p! Try again later", [SessionToken]),
  {error, retry_later}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_pub_auth_request(Ipv6, EndpointPid)->
  ddslib:curl_identity_to_xcr(Ipv6, "D"),
  DevInitRequest = ddslib:build_init_pub_req(Ipv6),
  xaptum_endpoint:send_request(EndpointPid, DevInitRequest).