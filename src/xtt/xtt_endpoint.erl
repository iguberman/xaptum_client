%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 06. May 2018 12:35 AM
%%%-------------------------------------------------------------------
-module(xtt_endpoint).
-author("iguberman").

-include("xtt_endpoint.hrl").
-include_lib("xtt_erlang/include/xtt.hrl").

-define(XTT_VERSION, ?XTT_VERSION_ONE).
-define(XTT_SUITE, ?XTT_X25519_LRSW_ED25519_AES256GCM_SHA512).

-behavior(xaptum_endpoint).

-define(READY_WAIT_TIMEOUT, 30000).

%% API
-export([start/1, init_file_creds/4, wait_for_endpoint_ready/1]).

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

start(Creds)->
  xaptum_endpoint_sup:create_endpoint(?MODULE, #endpoint{}, Creds).

wait_for_endpoint_ready(EndpointPid) ->
  wait_for_endpoint_ready(EndpointPid, ?READY_WAIT_TIMEOUT).

wait_for_endpoint_ready(EndpointPid, Timeout)->
  wait_for_endpoint_ready(EndpointPid, xaptum_endpoint:get_data(EndpointPid), Timeout).

wait_for_endpoint_ready(_EndpointPid, #endpoint{ipv6 = undefined}, Timeout) when Timeout =< 0->
  {error, timeout};
wait_for_endpoint_ready(EndpointPid, #endpoint{ipv6 = undefined}, Timeout) ->
  timer:sleep(100),
  lager:debug("Waiting for XttEndpoint getting ipv6 address ~p... ", [EndpointPid]),
  wait_for_endpoint_ready(EndpointPid, xaptum_endpoint:get_data(EndpointPid), Timeout - 100);
wait_for_endpoint_ready(_EndpointPid, #endpoint{ipv6 = Ipv6}, _Timeout) when is_binary(Ipv6) ->
  {ok, Ipv6}.

%%%===================================================================
%%% xaptum_endpoint callbacks
%%%===================================================================

auth(#hosts_config{xaptum_host = XttServerHost, xtt_port = XttServerPort},
    #tpm_creds{basename = BasenameFile, tpm_host = TpmHost, tpm_port = TpmPort, tpm_password = TpmPassword,
      client_id = ClientIdFile, server_id = ServerIdFile},
    CallbackData)->
  {ok, GroupContextInputs} = xtt_utils:group_context_inputs_tpm(BasenameFile, TpmHost, TpmPort, TpmPassword),
  {ok, #tls_creds{identity = Identity} = TlsCreds} = do_handshake(GroupContextInputs, ClientIdFile, ServerIdFile, XttServerHost, XttServerPort),
  {ok, TlsCreds, CallbackData#endpoint{ipv6 = Identity}};
auth(#hosts_config{xaptum_host = XttServerHost, xtt_port = XttServerPort},
    #file_creds{basename = BasenameFile, gpk = GpkFile, cred = CredFile, sk = SecretKeyFile,
      root_id = RootIdFile, root_pk = RootPubkeyFile, client_id = ClientIdFile, server_id = ServerIdFile},
    CallbackData)->
  {ok, GroupContextInputs} = xtt_utils:group_context_inputs(BasenameFile,
    GpkFile, CredFile,  SecretKeyFile, RootIdFile, RootPubkeyFile),
  {ok, #tls_creds{identity = Identity} = XttCreds} = do_handshake(
    GroupContextInputs, ClientIdFile, ServerIdFile,
    XttServerHost, XttServerPort),
  {ok, XttCreds, CallbackData#endpoint{ipv6 = Identity}}.

on_receive(Msg, #endpoint{num_received = NumReceived} = CallbackData)->
  lager:debug("Calling ~p:on_receive", [?MODULE]),
  {ok, CallbackData#endpoint{num_received = NumReceived + 1, msg = Msg}}.

on_send(Msg, _Dest, #endpoint{num_sent = NumSent} = CallbackData) ->
  {ok, Msg, CallbackData#endpoint{num_sent = NumSent + 1}}.

on_send(Msg, #endpoint{num_sent = NumSent} = CallbackData) ->
  {ok, Msg, CallbackData#endpoint{num_sent = NumSent + 1}}.

on_connect(_TlsSocket, CallbackData) ->
  {ok, CallbackData}.

on_reconnect(_TlsSocket, #endpoint{num_reconnects = Reconnects} = CallbackData) ->
  {ok, CallbackData#endpoint{num_reconnects = Reconnects + 1}}.

on_disconnect(_TlsSocket, CallbackData) ->
  {ok, CallbackData}.


%%%===================================================================
%%% UTILS
%%%===================================================================

init_tpm_creds(BaseDir, GroupDir, CertDir, CredDir, TpmHost, TpmPort, TpmPassword)->
  #tpm_creds{
    basename = filename:join([BaseDir, GroupDir, ?BASENAME_FILE]),
    tpm_host = TpmHost,
    tpm_port = TpmPort,
    tpm_password = TpmPassword,
    client_id = filename:join([BaseDir, CredDir, ?REQUESTED_CLIENT_ID_FILE]),
    server_id = filename:join([BaseDir, CertDir, ?SERVER_ID_FILE])}.


init_file_creds(RequestedClientIdFile, GroupDir, CertDir, CredDir)->
  lager:info("Initializing file creds from GroupDir ~p:~n~p, ~nCertDir ~p:~n~p, and ~nCredDir ~p:~n~p",
    [GroupDir, os:cmd("ls " ++ GroupDir), CertDir, os:cmd("ls " ++ CertDir), CredDir, os:cmd("ls " ++ CredDir)]),

  #file_creds{
    basename = filename:join([GroupDir, ?BASENAME_FILE]),
    gpk = filename:join([GroupDir, ?DAA_GPK_FILE]),
    cred = filename:join([CredDir, ?DAA_CRED_FILE]),
    sk = filename:join([CredDir, ?DAA_SECRETKEY_FILE]),
    root_id = filename:join([CertDir, ?ROOT_ID_FILE]),
    root_pk = filename:join([CertDir, ?ROOT_PUBKEY_FILE]),
    client_id = RequestedClientIdFile,
    server_id = filename:join([CertDir, ?SERVER_ID_FILE])}.

%%%===================================================================
%%% internal functions
%%%===================================================================

do_handshake(GroupContextInputs, RequestedClientIdFile, IntendedServerIdFile, XttServerHost, XttServerPort)->
  {RequestedClientId, IntendedServerId} =
    xtt_utils:initialize_ids(RequestedClientIdFile, IntendedServerIdFile),
  {ok, Pid} = xtt_handshake:start_handshake(
    XttServerHost, XttServerPort,
    RequestedClientId, IntendedServerId,
    ?XTT_VERSION, ?XTT_SUITE,
    GroupContextInputs),
  {ok, HandshakeContext} = xtt_utils:get_handshake_result(Pid),
  get_creds_from_xtt_context(HandshakeContext).


get_creds_from_xtt_context (HandshakeContext)->

  {ok, LongTermKey} = xtt_erlang:xtt_get_my_longterm_key(HandshakeContext),

  {ok, LongTermPrivKey} = xtt_erlang:xtt_get_my_longterm_private_key(HandshakeContext),

  {ok, Identity} = xtt_erlang:xtt_get_my_id(HandshakeContext),

  {ok, Pseudonym} = xtt_erlang:xtt_get_my_pseudonym(HandshakeContext),

  {ok, CertAsn1} = xtt_erlang:xtt_x509_from_keypair(LongTermKey, LongTermPrivKey, Identity),

  lager:info("IDENTITY: ~p", [Identity]),
  lager:info("CertAsn1: ~p, LongTermKey ~p, LongTermPrivKey ~p", [CertAsn1, LongTermKey, LongTermPrivKey]),

  {ok, PrivKeyAsn1} = xtt_erlang:xtt_asn1_from_private_key(LongTermPrivKey),

  lager:info("LongTermPrivKeyAsn1 ~p", [PrivKeyAsn1]),

  {ok, #tls_creds{identity = Identity, pseudonym = Pseudonym, cert = CertAsn1, key = PrivKeyAsn1}}.

