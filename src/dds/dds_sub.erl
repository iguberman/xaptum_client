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
-include("xtt_endpoint.hrl").

-behavior(xaptum_endpoint).

%% API
-export([start/2]).

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


start(Subnet, Queue)->
  xaptum_endpoint_sup:create_endpoint(?MODULE, #dds{queue = Queue}, Subnet).

%%%===================================================================
%%% xaptum_endpoint callbacks
%%%===================================================================

auth(#hosts_config{xcr_host = XcrHost, xcr_port = XcrPort}, Subnet,
    #dds{endpoint_data = EndpointData} = CallbackData) when is_binary(Subnet)->

  #{public := Pk, secret := Sk} = enacl:crypto_sign_ed25519_keypair(),

  PkBase64Enc = binary_to_list(base64:encode(Pk)),

  SubnetStr = xaptum_client:ipv6_binary_to_text(Subnet),

  CurlCmd = "curl -s -X POST -H \"Content-Type: application/json\" -d '{ \"subnet\" : \"" ++ SubnetStr ++ "/64\", \"pub_key\" : \""
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

  lager:info("Got ipv6 ~p from response", [Ipv6]),

  <<V1:16, V2:16, V3:16, V4:16, V5:16, V6:16, V7:16, V8:16, V9:16, V10:16, V11:16, V12:16, V13:16, V14:16, V15:16, V16:16>> = Ipv6,
  Identity = <<V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16>>,

  {ok, CertAsn1} = xtt_erlang:xtt_x509_from_keypair(Pk, Sk, Identity),

  {ok, PrivKeyAsn1} = xtt_erlang:xtt_asn1_from_private_key(Sk),

  TlsCreds = #tls_creds{key = PrivKeyAsn1, cert = CertAsn1, identity = Identity},

  lager:info("Resulting tls_creds: ~p", [TlsCreds]),

  {ok, TlsCreds, CallbackData#dds{endpoint_data = EndpointData#endpoint{ipv6 = Identity}}}.

on_connect(#dds{
    queue = Queue,
    endpoint_data = #endpoint{ipv6 = Ipv6}} = CallbackData) ->
  send_sub_auth_request(Ipv6, Queue, self()),
  {ok, CallbackData#dds{session_token = awaiting}}.

on_reconnect(#dds{
    queue = Queue,
    endpoint_data = EndpointData0 = #endpoint{ipv6 = Ipv6}} = CallbackData) ->
  send_sub_auth_request(Ipv6, Queue, self()),
  {ok, EndpointData1} = xtt_endpoint:on_reconnect(EndpointData0),
  {ok, CallbackData#dds{session_token = awaiting, endpoint_data = EndpointData1}}.

on_disconnect(CallbackData) -> {ok, CallbackData}.

on_receive(<<?DDS_MARKER, ?REG_MSG, Size:16, SessionToken:?SESSION_TOKEN_SIZE/bytes, Payload/binary>> = Msg,
      #dds{session_token = SessionToken, endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  Size = ?SESSION_TOKEN_SIZE + size(Payload), %% sanity check
  {ok, EndpointData1} = xtt_endpoint:on_receive(Payload, self(), EndpointData0),
  lager:info("Reg msg ~p received by ~p", [Msg, self()]),
  {ok, CallbackData#dds{endpoint_data = EndpointData1}};

on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, SessionToken:?SESSION_TOKEN_SIZE/bytes>>,
         #dds{session_token = awaiting} = CallbackData)->
  lager:info("Subscriber auth response received"),
  {ok, CallbackData#dds{session_token = SessionToken}};

on_receive(<<?DDS_MARKER, ?AUTH_RES, ?SESSION_TOKEN_SIZE:16, _RespSessionToken:?SESSION_TOKEN_SIZE/bytes>>,
           #dds{session_token = SessionToken})
    when is_binary(SessionToken); SessionToken =:= undefined ->
  lager:error("Subscriber auth response received out of sync: session token ~p, should be 'awaiting'", [SessionToken]),
  {error, recv_out_of_sync};

on_receive(<<?DDS_MARKER, ?REG_MSG, _Size:16, _SessionToken:?SESSION_TOKEN_SIZE/bytes, _Rest/binary>>,
    #dds{session_token = SessionToken})
    when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Subscriber not ready to receive messages: session token should be assigned, but is ~p", [SessionToken]),
  {error, not_ready}.

do_receive(TlsSocket)->
  ddslib:recv(TlsSocket).

%% CONTROL MSG
on_send(Msg0, Dest, #dds{
  session_token = SessionToken,
  endpoint_data = EndpointData0} = CallbackData) when is_binary(SessionToken)->
  {ok, Msg1, EndpointData1} = xtt_endpoint:on_send(Msg0, Dest, EndpointData0), %% Oh-bject Oh-riented programming
  Msg2 = ddslib:build_control_message(SessionToken, <<Dest/binary, Msg1/binary>>),
  {ok, Msg2, CallbackData#dds{endpoint_data = EndpointData1} };
on_send(_Msg, _Dest, #dds{session_token = SessionToken})
  when SessionToken =:= undefined; SessionToken =:= awaiting ->
  lager:error("Can't send control message when session token is still ~p! Try again later", [SessionToken]),
  {error, retry_later}.

%% REG MSG: normally a PUB functionality
on_send(Msg, #dds{} = CallbackData)->
  dds_pub:on_send(Msg, CallbackData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_sub_auth_request(Ipv6, Queue, EndpointPid) when is_list(Queue)->
  send_sub_auth_request(Ipv6, list_to_binary(Queue), EndpointPid);
send_sub_auth_request(Ipv6, Queue, EndpointPid) when is_binary(Queue)->
  ddslib:curl_identity_to_xcr(Ipv6, "S"),
  SubInitRequest = ddslib:build_init_sub_req(Ipv6, Queue),
  xaptum_endpoint:send_request(EndpointPid, SubInitRequest).
