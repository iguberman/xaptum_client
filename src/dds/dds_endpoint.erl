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
  start/1,
  start/2,
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

start(Subnet, Queues)->
  xaptum_endpoint_sup:create_endpoint(?MODULE,
    #dds{sub_queues = Queues, endpoint_data = #endpoint{}}, Subnet).

start(Creds)->
  xaptum_endpoint_sup:create_endpoint(?MODULE,
    #dds{endpoint_data = #endpoint{}}, Creds).

wait_for_endpoint_ready(Pub) ->
  wait_for_endpoint_ready(Pub, ?READY_WAIT_TIMEOUT).

wait_for_endpoint_ready(Pub, Timeout)->
  wait_for_endpoint_ready(Pub, false, Timeout).

wait_for_endpoint_ready(_EndpointPid, false, Timeout) when Timeout =< 0->
  {error, timeout};
wait_for_endpoint_ready(EndpointPid, false, Timeout) ->
  timer:sleep(100),
  #dds{ready = Ready} = DdsEndpoint = xaptum_endpoint:get_data(EndpointPid),
  lager:debug("Waiting for DdsEndpoint ready ~p... ", [DdsEndpoint]),
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
  {ok, TlsCreds, #endpoint{ipv6 = Ipv6} =  EndpointData1} =
    xtt_endpoint:auth(HostsConfig, Inputs, EndpointData0),

  lager:info("************* XTT AUTH complete ~p (~p)***************", [Ipv6, convert_to_Ipv6Str(Ipv6)]),

  timer:sleep(2000),

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

  jsx:maps_support(),
  TokenJson = os:cmd(GetTokenCmd),
  TokenJsonDecoded = jsx:decode(list_to_binary(TokenJson), [return_maps]),
  #{<<"data">> := [#{<<"token">> := Token}]} = TokenJsonDecoded,

  lager:info("Got Token from XCR: ~p", [Token]),

  CurlCmd = "curl -s -X POST -H \"Content-Type: application/json\" -H \"Authorization: Bearer " ++ binary_to_list(Token)
    ++ "\" -d '{ \"subnet\" : \"" ++ SubnetStr ++ "/64\", \"pub_key\" : \""
    ++ PkBase64Enc ++ "\" }' http://" ++ XcrHost ++ ":" ++ integer_to_list(XcrPort) ++ "/api/xcr/v2/ephook",

  lager:info("Running ~p", [CurlCmd]),

%%  Expecting JSON in this format:
%% {"data":[{"subnet":"2607:8f80:8000::/64",
%%           "ipv6":"26078F80800000004F12CEEB95F13575",
%%           "pub_key":"VkYD0g//Dje5dXV413I7jsdegT7ZHmbQmBCoe6s3Tak="}],
%% "page":{"curr":-1,"next":-1,"prev":-1}}

  JsonResp = os:cmd(CurlCmd),

  DecodedResp = jsx:decode(list_to_binary(JsonResp), [return_maps]),
  lager:info("Decoded curl resp: ~p", [DecodedResp]),

  #{<<"data">> := [#{<<"ipv6">> := Ipv6}]} = DecodedResp,

  Identity = xtt_client_utils:hex_to_bin(Ipv6),

  lager:info("Got ipv6 ~p from response converted to Identity: ~p", [Ipv6, Identity]),

  {ok, CertAsn1} = xtt_erlang:xtt_x509_from_keypair(Pk, Sk, Identity),

  {ok, PrivKeyAsn1} = xtt_erlang:xtt_asn1_from_private_key(Sk),

  TlsCreds = #tls_creds{key = PrivKeyAsn1, cert = CertAsn1, identity = Identity},

  lager:info("Resulting tls_creds: ~p", [TlsCreds]),

  lager:info("************* SUB AUTH complete ~p (~p)***************", [Identity, convert_to_Ipv6Str(Identity)]),

  timer:sleep(2000),

  {ok, TlsCreds, CallbackData#dds{endpoint_data = EndpointData#endpoint{ipv6 = Identity}}}.

on_connect(TlsSocket, #dds{endpoint_data = #endpoint{ipv6 = Ipv6} = EndpointData0} = CallbackData) ->
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

on_receive(<<?DDS_MARKER, ReqType, Size:16, Rest/binary>> = Packet, #dds{prev_bytes = <<>>} = DdsEndpoint)->
  case size(Rest) of
    Size -> process_dds_packet(ReqType, Rest, DdsEndpoint);
    SmallerSize when SmallerSize < Size ->
      {ok, DdsEndpoint#dds{prev_bytes = Packet}};
    LargerSize when LargerSize > Size ->
      <<Payload:Size/binary, ExtraBytes/binary>> = Rest,
      case process_dds_packet(ReqType, Payload, DdsEndpoint) of
        {ok, DdsEndpoint1} ->
          on_receive(ExtraBytes, DdsEndpoint1);
        {error, Reason} -> {error, Reason}
      end
  end;
on_receive(Packet, #dds{prev_bytes = PrevBytes} = DdsEndpoint) when size(PrevBytes) > 0 ->
  Size = size(PrevBytes),
  on_receive(<<PrevBytes:Size/binary, Packet/binary>>, DdsEndpoint#dds{prev_bytes = <<>>});
on_receive(UnexpectedTcpPacket, #dds{prev_bytes = PrevBytes} = DdsEndpoint)->
  lager:warning("TCP: Unexpected tcp packet ~p, PrevBytes ~p", [UnexpectedTcpPacket, PrevBytes]),
  {error, invalid_packet}.

process_dds_packet(?DDS_SERVER_HELLO, Ipv6,
    #dds{sub_queues = Queues, endpoint_data = #endpoint{ipv6 = Ipv6}} = CallbackData)->
  lager:info("SERVER HELLO received by ~p", [Ipv6]),
  lager:info("Generating sub requests for queues ~p", [Queues]),
  [send_subscribe_request(Queue) || Queue <- Queues],
  {ok, CallbackData#dds{ready = true}};
process_dds_packet(ReqType, _Payload,
    #dds{ready = false})  when ReqType =:= ?SUB_REQ; ReqType =:= ?REG_MSG; ReqType =:= ?CONTROL_MSG ->
  lager:error("Got message type ~p request, while not in ready state!", [ReqType]),
  {error, not_ready};
process_dds_packet(?CONTROL_MSG, Payload,
    #dds{ready = true, endpoint_data = EndpointData0} = CallbackData) ->
  {ok, EndpointData1} = xtt_endpoint:on_receive(Payload, EndpointData0),
  lager:info("Control message ~p received by ~p", [Payload, self()]),
  {ok, CallbackData#dds{endpoint_data = EndpointData1}};
process_dds_packet(UnexpectedRequest, Payload, CallbackData)->
  lager:error("DDS_ENDPOINT ~p received unexpected request ~p with payload ~p", [CallbackData, UnexpectedRequest, Payload] ),
  {error, unexpected_dds_request}.

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
  Msg2 = ddslib:control_request(Msg1, Dest),
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

convert_to_Ipv6Str(Ipv6Bytes)->
  <<IP1:16,IP2:16,IP3:16,IP4:16,IP5:16,IP6:16, IP7:16,IP8:16>> = Ipv6Bytes,
  inet:ntoa({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}).
