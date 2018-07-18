%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2018 12:28 AM
%%%-------------------------------------------------------------------
-module(dds_endpoint).
-author("iguberman").

-include("dds.hrl").

-behavior(xaptum_endpoint).

-define(XCR_USERNAME_ENV, "XCR_USERNAME").
-define(XCR_TOKEN_ENV, "XCR_TOKEN").

-define(SERVER_HELLO_TIMEOUT, 10000).
-define(READY_WAIT_TIMEOUT, 30000).

%% API
-export([
  start/2,
  start/3,
  wait_for_endpoint_ready/1,
  wait_for_endpoint_ready/2]).

%% xaptum_endpoint callbacks
-export([
  auth/3,
  on_send/2,
  on_send/3,
  on_receive/2,
  on_connect/2,
  on_reconnect/2,
  on_disconnect/2
]).

start(Subnet, Queues, {RemoteIp, RemotePort})->
  xaptum_endpoint_sup:create_endpoint(?MODULE,
    #dds{sub_queues = Queues, endpoint_data = #endpoint{remote_ip = RemoteIp, remote_port = RemotePort}}, Subnet).

start(Creds, {RemoteIp, RemotePort})->
  xaptum_endpoint_sup:create_endpoint(?MODULE,
    #dds{endpoint_data = #endpoint{remote_ip = RemoteIp, remote_port = RemotePort}}, Creds).

wait_for_endpoint_ready(Pub) ->
  wait_for_endpoint_ready(Pub, ?READY_WAIT_TIMEOUT).

wait_for_endpoint_ready(Pub, Timeout)->
  wait_for_endpoint_ready(Pub, false, Timeout).

wait_for_endpoint_ready(_EndpointPid, false, Timeout) when Timeout =< 0->
  {error, timeout};
wait_for_endpoint_ready(EndpointPid, false, Timeout) ->
  timer:sleep(100),
  #dds{ready = Ready} = xaptum_endpoint:get_data(EndpointPid),
  wait_for_endpoint_ready(EndpointPid, Ready, Timeout - 100);
wait_for_endpoint_ready(_EndpointPid, true, _Timeout) ->
  {ok, true}.

%%%===================================================================
%%% xaptum_endpoint callbacks
%%%===================================================================

%% This is a non-subscribing endpoint so assume xtt auth
%% NOTE: this is TLS auth (not dds AUTH which no longer exists)
auth(#hosts_config{xaptum_host = _XttServerHost, xtt_port = _XttServerPort} = HostsConfig,
    Inputs, #dds{sub_queues = [], endpoint_data = EndpointData0} = CallbackData) ->
  {ok, TlsCreds, EndpointData1} =
    xtt_endpoint:auth(HostsConfig, Inputs, EndpointData0),
  {ok, TlsCreds, CallbackData#dds{endpoint_data = EndpointData1}};

%% This is a subscribing endpoint so assume normal enacl auth
auth(#hosts_config{xcr_host = XcrHost, xcr_port = XcrPort}, Subnet,
    #dds{sub_queues = Queues, endpoint_data = EndpointData} = CallbackData) when length(Queues) > 0, is_binary(Subnet)->

  #{public := Pk, secret := Sk} = enacl:crypto_sign_ed25519_keypair(),

  PkBase64Enc = binary_to_list(base64:encode(Pk)),

  SubnetStr = xaptum_client:ipv6_binary_to_text(Subnet),

  XcrUsername = os:getenv(?XCR_USERNAME_ENV),
  XcrToken = os:getenv(?XCR_TOKEN_ENV),

  GetTokenCmd = "curl -s -X POST -H \"Content-Type: application/json\" -d '{ \"username\": \"" ++
    XcrUsername ++ "\", \"token\": \"" ++ XcrToken ++ "\" }' http://"
    ++ XcrHost ++ ":" ++ integer_to_list(XcrPort) ++ "/api/xcr/v2/xauth",

  lager:info("Executing get token cmd: ~n~p", [GetTokenCmd]),

  Token = os:cmd(GetTokenCmd),

  lager:info("Got Token from XCR: ~p", [Token]),

  CurlCmd = "curl -s -X POST -H \"Content-Type: application/json Authorization: Bearer \"" ++ Token
    ++ "\" -d '{ \"subnet\" : \"" ++ SubnetStr ++ "/64\", \"pub_key\" : \""
    ++ PkBase64Enc ++ "\" }' http://" ++ XcrHost ++ ":" ++ integer_to_list(XcrPort) ++ "/api/xcr/v2/ephook",

  lager:info("Running ~p", [CurlCmd]),

%%  Expecting JSON in this format:
%% {"data":[{"subnet":"2607:8f80:8000::/64",
%%           "ipv6":"26078F80800000004F12CEEB95F13575",
%%           "pub_key":"VkYD0g//Dje5dXV413I7jsdegT7ZHmbQmBCoe6s3Tak="}],
%% "page":{"curr":-1,"next":-1,"prev":-1}}

  JsonResp = os:cmd(CurlCmd),
  jsx:maps_support(),
  DecodedResp = jsx:decode(list_to_binary(JsonResp), [return_maps]),
  lager:info("Decoded curl resp: ~p", [DecodedResp]),

  #{<<"data">> := [#{<<"ipv6">> := Ipv6}]} = DecodedResp,

  Identity = xtt_client_utils:hex_to_bin(Ipv6),

  lager:info("Got ipv6 ~p from response converted to Identity: ~b", [Ipv6, Identity]),

  {ok, CertAsn1} = xtt_erlang:xtt_x509_from_keypair(Pk, Sk, Identity),

  {ok, PrivKeyAsn1} = xtt_erlang:xtt_asn1_from_private_key(Sk),

  TlsCreds = #tls_creds{key = PrivKeyAsn1, cert = CertAsn1, identity = Identity},

  lager:info("Resulting tls_creds: ~p", [TlsCreds]),

  {ok, TlsCreds, CallbackData#dds{endpoint_data = EndpointData#endpoint{ipv6 = Identity}}}.

on_connect(TlsSocket, #dds{endpoint_data = #endpoint{ipv6 = Ipv6, remote_ip = RemoteIp, remote_port = RemotePort} = EndpointData0} = CallbackData) ->
  case send_client_hello(TlsSocket, Ipv6) of
    ok ->
      {ok, EndpointData1} = xtt_endpoint:on_connect(TlsSocket, EndpointData0),
      {ok, CallbackData#dds{endpoint_data = EndpointData1}};
    {error, _Error} -> {error, client_hello_error}
  end.


on_reconnect(TlsSocket, #dds{
  endpoint_data = EndpointData0 = #endpoint{ipv6 = Ipv6}} = CallbackData0) ->
  {ok, CallbackData1} = dds_endpoint:on_connect(TlsSocket, CallbackData0),
  {ok, EndpointData1} = xtt_endpoint:on_reconnect(TlsSocket, EndpointData0),
  {ok, CallbackData1#dds{endpoint_data = EndpointData1}}.


on_disconnect(_TlsSocket, CallbackData) ->
  {ok, CallbackData}.

on_receive(<<?DDS_MARKER, ?DDS_SERVER_HELLO, ?IPV6_SIZE:16, Ipv6:?IPV6_SIZE/bytes>>,
    #dds{sub_queues = Queues, endpoint_data = #endpoint{ipv6 = Ipv6}} = CallbackData)->
  lager:info("SERVER HELLO received by ~p", [Ipv6]),
  [send_subscribe_request(Queue) || Queue <- Queues],
  {ok, CallbackData#dds{ready = true}};
on_receive(<<?DDS_MARKER, ReqType, _Size:16, _Payload/binary>>,
    #dds{ready = false})  when ReqType =:= ?SUB_REQ; ReqType =:= ?REG_MSG; ReqType =:= ?CONTROL_MSG ->
  lager:error("Got message type ~p request, while not in ready state!", [ReqType]),
  {error, not_ready};
on_receive(<<?DDS_MARKER, ?CONTROL_MSG, Size:16, Payload/binary>> = Msg,
    #dds{ready = true, endpoint_data = EndpointData0} = CallbackData) ->
  Size = size(Payload), %% sanity check
  {ok, EndpointData1} = xtt_endpoint:on_receive(Payload, EndpointData0),
  lager:info("Control message ~p received by ~p", [Msg, self()]),
  {ok, CallbackData#dds{endpoint_data = EndpointData1}};
on_receive(Unexpected, CallbackData)->
  lager:error("DDS_ENDPOINT ~p received unexpected message ~p", [Unexpected, CallbackData] ).

on_send(_Msg, #dds{ready = false}) ->
  lager:error("Not ready to send messages or requests ~p! Try again later", []),
  {error, retry_later};
%% REG MSG
on_send(Msg0, #dds{endpoint_data = EndpointData0, ready = true} = CallbackData) ->
  {ok, Msg1, EndpointData1} = xtt_endpoint:on_send(Msg0, EndpointData0),
  Msg2 = ddslib:reg_msg_request(Msg1),
  lager:debug("Generated REG_MSG request ~p", [Msg2]),
  {ok, Msg2, CallbackData#dds{endpoint_data = EndpointData1} }.

on_send(Msg0, Dest, #dds{ready = true, endpoint_data = EndpointData0} = CallbackData) ->
  {ok, Msg1, EndpointData1} = xtt_endpoint:on_send(Msg0, Dest, EndpointData0),
  Msg2 = ddslib:control_request(Dest, Msg1),
  lager:debug("Generated CONTROL request ~p", [Msg2]),
  {ok, Msg2, CallbackData#dds{endpoint_data = EndpointData1} };
on_send(_Msg, _Dest, #dds{ready = false}) ->
  lager:error("Not ready to send messages ~p! Try again later", []),
  {error, retry_later}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

send_subscribe_request(Queue) ->
  SubReq = ddslib:subscribe_request(Queue),
  xaptum_endpoint:send_request(self(), SubReq).

send_client_hello(TlsSocket, Ipv6) ->
  lager:info("Sending client hello to TlsSocket ~p", [TlsSocket]),
  PubClientHello = ddslib:client_hello(Ipv6),
  case erltls:send(TlsSocket, PubClientHello) of
    ok ->
      lager:info("Sent client hello: ~p", [PubClientHello]),
      ok;
    {error, Error} ->
      lager:error("Failed to send client hello ~p due to error ~p", [PubClientHello, Error]),
      {error, {tls_error, Error}}
  end.
