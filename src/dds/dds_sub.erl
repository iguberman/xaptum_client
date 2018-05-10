%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2018 4:15 PM
%%%-------------------------------------------------------------------
-module(dds_sub).
-author("iguberman").


-include("dds.hrl").

-behavior(xaptum_endpoint).

-define(SUPERCLASS, xtt_endpoint).

%% API
-export([start/2]).

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


start(Creds, Queue)->
  xaptum_endpoint_sup:create_endpoint(?MODULE, #dds_sub_data{queue = Queue}, Creds).

%%%===================================================================
%%% xaptum_endpoint callbacks
%%%===================================================================

auth(XttServerHost, XttServerPort, Creds, #dds_sub_data{endpoint_data = EndpointData0} = CallbackData)->
  {ok, XttCreds, EndpointData1} =
    ?SUPERCLASS:auth(XttServerHost, XttServerPort, Creds, EndpointData0),
  {ok, XttCreds, CallbackData#dds_sub_data{endpoint_data = EndpointData1}}.

on_connect(EndpointPid, #dds_sub_data{
    queue = Queue,
    endpoint_data = #endpoint_data{ipv6 = Ipv6}} = CallbackData) ->
  send_sub_auth_request(Ipv6, Queue, EndpointPid),
  {ok, CallbackData#dds_sub_data{session_token = awaiting}}.

on_reconnect(EndpointPid, #dds_sub_data{
    queue = Queue,
    endpoint_data = EndpointData0 = #endpoint_data{ipv6 = Ipv6}} = CallbackData) ->
  send_sub_auth_request(Ipv6, Queue, EndpointPid),
  {ok, EndpointData1} = ?SUPERCLASS:on_reconnect(EndpointPid, EndpointData0),
  {ok, CallbackData#dds_sub_data{session_token = awaiting, endpoint_data = EndpointData1}}.

on_disconnect(_EndpointPid, CallbackData) -> {ok, CallbackData}.

on_receive(<<?DDS_MARKER, ?REG_MSG, Size:16, _DdsPayload:Size/bytes, _Rest/binary>> = Msg,
    EndpointPid, #dds_sub_data{session_token = SessionToken,
      endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {ok, EndpointData1} = ?SUPERCLASS:on_receive(Msg, EndpointPid, EndpointData0),
  lager:info("Reg msg ~p received by ~p", [Msg, EndpointPid]),
  {ok, CallbackData#dds_sub_data{endpoint_data = EndpointData1}};
on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, SessionToken:?SESSION_TOKEN_SIZE/bytes>>,
    _EndpointPid, #dds_sub_data{session_token = awaiting} = CallbackData)->
  lager:info("Subscriber auth response received"),
  {ok, CallbackData#dds_sub_data{session_token = SessionToken}};
on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, _RespSessionToken:?SESSION_TOKEN_SIZE/bytes>>,
    _EndpointPid, #dds_sub_data{session_token = SessionToken})
    when is_binary(SessionToken); SessionToken =:= undefined ->
  lager:error("Subscriber auth response received out of sync: session token ~p, should be 'awaiting'", [SessionToken]),
  {error, recv_out_of_sync};
on_receive(<<?DDS_MARKER, ?REG_MSG, Size:16, _DdsPayload:Size/bytes, _Rest/binary>>,
    _EndpointPid, #dds_sub_data{session_token = SessionToken})
    when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Subscriber not ready to receive messages: session token should be assigned, but is ~p", [SessionToken]),
  {error, not_ready}.

receive_loop(TlsSocket, EndpointPid, CallbackData0) ->
  case erltls:recv(TlsSocket, 0) of
    {ok, Msg} ->
      {ok, CallbackData1} = on_receive(Msg, EndpointPid, CallbackData0),
      xaptum_endpoint:set_data(EndpointPid, CallbackData1), %% real time updates, could be batched if needed
      receive_loop(TlsSocket, EndpointPid, CallbackData1);
    {error, Error} ->
      xaptum_endpoint:ssl_error(EndpointPid, TlsSocket, Error, CallbackData0)
  end.

on_send(Msg0, Dest, #dds_sub_data{
  session_token = SessionToken,
  endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {Msg1, EndpointData1} = ?SUPERCLASS:on_send(Msg0, Dest, EndpointData0),
  Msg2 = ddslib:build_control_message(SessionToken, Msg1),
  {ok, Msg2, CallbackData#dds_sub_data{endpoint_data = EndpointData1} };
on_send(_Msg, _Dest, #dds_sub_data{session_token = SessionToken}) when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Can't send control message from subscriber that's not yet been activated! Try again later"),
  {error, retry_later}.


on_send(Msg0, #dds_sub_data{session_token = SessionToken, endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {Msg1, EndpointData1} = ?SUPERCLASS:on_send(Msg0, EndpointData0),
  Msg2 = ddslib:build_reg_message(SessionToken, Msg1),
  {ok, Msg2, CallbackData#dds_sub_data{endpoint_data = EndpointData1} };
on_send(_Msg, #dds_sub_data{session_token = SessionToken}) when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Can't send message from device that's not yet been activated! Try again later"),
  {error, retry_later}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_sub_auth_request(Ipv6, Queue, EndpointPid) when is_list(Queue)->
  send_sub_auth_request(Ipv6, list_to_binary(Queue), EndpointPid);
send_sub_auth_request(Ipv6, Queue, EndpointPid) when is_binary(Queue)->
  SubInitRequest = ddslib:build_init_sub_req(Ipv6, Queue),
  xaptum_endpoint:send_request(EndpointPid, SubInitRequest).
