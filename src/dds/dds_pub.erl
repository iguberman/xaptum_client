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

-define(SUPERCLASS, xtt_endpoint).
%% API
-export([start/1]).

%% xaptum_endpoint callbacks
-export([
  auth/4,
  on_send/2,
  on_send/3,
  on_receive/3,
  receive_loop/3,
  on_connect/2,
  on_reconnect/2,
  on_disconnect/2
  ]).


start(Creds)->
  xaptum_endpoint_sup:create_endpoint(?MODULE, #dds_pub_data{}, Creds).

%%%===================================================================
%%% xaptum_endpoint callbacks
%%%===================================================================

auth(XttServerHost, XttServerPort, Creds, #dds_pub_data{endpoint_data = EndpointData0} = CallbackData)->
  {ok, XttCreds, EndpointData1} =
    ?SUPERCLASS:auth(XttServerHost, XttServerPort, Creds, EndpointData0),
  {ok, XttCreds, CallbackData#dds_pub_data{endpoint_data = EndpointData1}}.

on_connect(EndpointPid, #dds_pub_data{endpoint_data = #endpoint_data{ipv6 = Ipv6}} = CallbackData) ->
  send_pub_auth_request(Ipv6, EndpointPid),
  {ok, CallbackData#dds_pub_data{session_token = awaiting}}.

on_reconnect(EndpointPid, #dds_pub_data{
  endpoint_data = EndpointData0 = #endpoint_data{ipv6 = Ipv6}} = CallbackData) ->
  send_pub_auth_request(Ipv6, EndpointPid),
  {ok, EndpointData1} = ?SUPERCLASS:on_reconnect(EndpointPid, EndpointData0),
  {ok, CallbackData#dds_pub_data{session_token = awaiting, endpoint_data = EndpointData1}}.

on_disconnect(_EndpointPid, CallbackData) -> {ok, CallbackData}.

on_receive(<<?DDS_MARKER, ?SIGNAL_MSG, Size:16, _DdsPayload:Size/bytes, _Rest/binary>> = Msg,
    EndpointPid, #dds_pub_data{session_token = SessionToken,
      endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {ok, EndpointData1} = ?SUPERCLASS:on_receive(Msg, EndpointPid, EndpointData0),
  lager:info("Control message ~p received by ~p", [Msg, EndpointPid]),
  {ok, CallbackData#dds_pub_data{endpoint_data = EndpointData1}};
on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, SessionToken:?SESSION_TOKEN_SIZE/bytes>>,
    _EndpointPid, #dds_pub_data{session_token = awaiting, endpoint_data = #endpoint_data{ipv6 = Ipv6}} = CallbackData)->
  lager:info("Device ~p Auth response received", [Ipv6]),
  {ok, CallbackData#dds_pub_data{session_token = SessionToken}};
on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, _SessionToken:?SESSION_TOKEN_SIZE/bytes>>,
    _EndpointPid, #dds_pub_data{session_token = awaiting, endpoint_data = #endpoint_data{ipv6 = Ipv6}})->
  lager:error("Device ~p Auth response received out of sync", [Ipv6]),
  {error, recv_out_of_sync};
on_receive(<<?DDS_MARKER, ?SIGNAL_MSG, Size:16, _DdsPayload:Size/bytes, _Rest/binary>>,
    _EndpointPid, #dds_pub_data{session_token = SessionToken, endpoint_data = #endpoint_data{ipv6 = Ipv6}})
  when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Device ~p not ready to receive control messages, session token is still ~p", [Ipv6, SessionToken]),
  {error, not_ready}.

receive_loop(TlsSocket, EndpointPid, CallbackData0) ->
  case erltls:recv(TlsSocket, 0) of
    {ok, Msg} ->
      {ok, CallbackData1} = on_receive(Msg, EndpointPid, CallbackData0),
      xaptum_endpoint:set_data(EndpointPid, CallbackData1), %% real time updates
      receive_loop(TlsSocket, EndpointPid, CallbackData1);
    {error, Error} ->
      xaptum_endpoint:ssl_error(EndpointPid, TlsSocket, Error, CallbackData0)
  end.

on_send(Msg0, Dest, #dds_pub_data{
  session_token = SessionToken,
  endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {Msg1, EndpointData1} = ?SUPERCLASS:on_send(Msg0, Dest, EndpointData0), %% Oh-bject Oh-riented programming
  Msg2 = ddslib:build_reg_message(SessionToken, Msg1),
  {ok, Msg2, CallbackData#dds_pub_data{endpoint_data = EndpointData1} };
on_send(_Msg, _Dest, #dds_pub_data{session_token = SessionToken})
  when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Can't send reg message when session token is still ~p! Try again later", [SessionToken]),
  {error, retry_later}.

on_send(Msg0, #dds_pub_data{session_token = SessionToken, endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {Msg1, EndpointData1} = ?SUPERCLASS:on_send(Msg0, EndpointData0),
  Msg2 = ddslib:build_reg_message(SessionToken, Msg1),
  {ok, Msg2, CallbackData#dds_pub_data{endpoint_data = EndpointData1} };
on_send(_Msg, #dds_pub_data{session_token = SessionToken}) when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Can't send reg message when session token is still ~p! Try again later", [SessionToken]),
  {error, retry_later}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_pub_auth_request(Ipv6, EndpointPid)->
  ddslib:curl_identity_to_xcr(Ipv6, "D"),
  DevInitRequest = ddslib:build_init_pub_req(Ipv6),
  xaptum_endpoint:send_request(EndpointPid, DevInitRequest).