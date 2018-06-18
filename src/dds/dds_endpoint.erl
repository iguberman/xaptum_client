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


%% API
-export([start/2, start/3]).

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

start(Subnet, Queues, {RemoteIp, RemotePort})->
  xaptum_endpoint_sup:create_endpoint(?MODULE,
    #dds{sub_queues = Queues, endpoint_data = #endpoint{remote_ip = RemoteIp, remote_port = RemotePort}}, Subnet).

start(Creds, {RemoteIp, RemotePort})->
  xaptum_endpoint_sup:create_endpoint(?MODULE,
    #dds{endpoint_data = #endpoint{remote_ip = RemoteIp, remote_port = RemotePort}}, Creds).

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

on_connect(#dds{endpoint_data = #endpoint{ipv6 = Ipv6, remote_ip = RemoteIp, remote_port = RemotePort} = EndpointData0} = CallbackData) ->
  send_connect_event(Ipv6, RemoteIp, RemotePort),
  {ok, EndpointData1} = xtt_endpoint:on_connect(EndpointData0),
  {ok, CallbackData#dds{ready = false, endpoint_data = EndpointData1}}.

on_reconnect(#dds{
  endpoint_data = EndpointData0 = #endpoint{ipv6 = Ipv6}} = CallbackData0) ->
  {ok, CallbackData1} = dds_endpoint:on_connect(CallbackData0),
  {ok, EndpointData1} = xtt_endpoint:on_reconnect(EndpointData0),
  {ok, CallbackData1#dds{endpoint_data = EndpointData1}}.


on_disconnect(CallbackData) -> {ok, CallbackData}.

on_receive(<<?DDS_MARKER, ?READY, ?IPV6_SIZE:16, Ipv6:?IPV6_SIZE/bytes>>,
    #dds{sub_queues = Queues, endpoint_data = #endpoint{ipv6 = Ipv6, remote_ip = RemoteIp, remote_port = RemotePort}} = CallbackData)->
  lager:info("READY response received by ~p", [Ipv6]),
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
  {ok, CallbackData#dds{endpoint_data = EndpointData1}}.


do_receive(TlsSocket)->
  ddslib:recv(TlsSocket).

on_send(_Msg, #dds{ready = false}) ->
  lager:error("Not ready to send messages or requests ~p! Try again later", []),
  {error, retry_later};
%% REG MSG
on_send(Msg0, #dds{endpoint_data = EndpointData0, ready = true} = CallbackData) ->
  {ok, Msg1, EndpointData1} = xtt_endpoint:on_send(Msg0, EndpointData0),
  Msg2 = ddslib:reg_msg_request(Msg1),
  {ok, Msg2, CallbackData#dds{endpoint_data = EndpointData1} }.

on_send(Msg0, Dest, #dds{ready = true, endpoint_data = EndpointData0} = CallbackData) ->
  {ok, Msg1, EndpointData1} = xtt_endpoint:on_send(Msg0, Dest, EndpointData0),
  Msg2 = ddslib:control_request(Dest, Msg1),
  {ok, Msg2, CallbackData#dds{endpoint_data = EndpointData1} };
on_send(_Msg, _Dest, #dds{ready = false}) ->
  lager:error("Not ready to send messages ~p! Try again later", []),
  {error, retry_later}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

send_subscribe_request(Queue)->
  SubReq = ddslib:subscribe_request(Queue),
  xaptum_endpoint:send_request(self(), SubReq).

send_connect_event(Ipv6, undefined, undefined) ->
  lager:error("FAILED to connect ~p! RemoteIp and Port are undefined!", [Ipv6]);
send_connect_event(Ipv6, RemoteIp, RemotePort)->
  PubConnectEvent = ddslib:connect_event(Ipv6, RemoteIp, RemotePort),
  xaptum_endpoint:send_request(self(), PubConnectEvent).