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

-behavior(xaptum_endpoint).

%% API

%% xaptum_endpoint callbacks
-export([
  auth/4,
  on_receive/3,
  receive_loop/3,
  on_send/3,
  on_send/4,
  on_disconnect/2,
  on_connect/2,
  on_reconnect/2,
  on_disconnect/2
]).

%%%===================================================================
%%% xaptum_endpoint callbacks
%%%===================================================================

auth(XttServerHost, XttServerPort,
    #tpm_creds{cred_dir = CredDir, tpm_host = TpmHost, tpm_port = TpmPort, tpm_password = TpmPassword}, _CallbackData)->
  {ok, GroupContextInputs} = xtt_utils:group_context_inputs_tpm(CredDir, ?BASENAME_FILE, TpmHost, TpmPort, TpmPassword),
  do_handshake(GroupContextInputs, CredDir, XttServerHost, XttServerPort, _CallbackData);
auth(XttServerHost, XttServerPort,
    #file_creds{cred_dir = CredDir}, _CallbackData)->
  {ok, GroupContextInputs} = xtt_utils:group_context_inputs(CredDir,
    ?BASENAME_FILE,
    ?DAA_GPK_FILE,
    ?DAA_CRED_FILE,
    ?DAA_SECRETKEY_FILE,
    ?ROOT_ID_FILE, ?ROOT_PUBKEY_FILE),
  do_handshake(GroupContextInputs, CredDir, XttServerHost, XttServerPort, _CallbackData).


on_receive(_Msg, _Parent, #endpoint_data{num_received = NumReceived} = CallbackData)->
  {ok, CallbackData#endpoint_data{num_received = NumReceived + 1}}.

receive_loop(TlsSocket, Parent, CallbackData0) ->
  case erltls:recv(TlsSocket, 0) of
    {ok, Msg} ->
      {ok, CallbackData1} = on_receive(Msg, Parent, CallbackData0),
      xaptum_endpoint:set_data(Parent, CallbackData1),
      receive_loop(TlsSocket, Parent, CallbackData1);
    {error, Error} ->
      xaptum_endpoint:ssl_error(Parent, TlsSocket, Error, CallbackData0)
  end.

on_send(Msg, _Dest, _Parent, #endpoint_data{num_received = NumSent} = CallbackData) ->
  {Msg, CallbackData#endpoint_data{num_sent = NumSent + 1}}.

on_send(Msg, _Parent, #endpoint_data{num_received = NumSent} = CallbackData) ->
  {Msg, CallbackData#endpoint_data{num_sent = NumSent + 1}}.

on_connect(_Parent, CallbackData) -> {ok, CallbackData}.

on_reconnect(_Parent, #endpoint_data{num_reconnects = Reconnects} = CallbackData) ->
  {ok, CallbackData#endpoint_data{num_reconnects = Reconnects + 1}}.

on_disconnect(_Parent, CallbackData) ->
  {ok, CallbackData}.


%%%===================================================================
%%% internal functions
%%%===================================================================

do_handshake(GroupContextInputs, CredDir, XttServerHost, XttServerPort, _CallbackData)->
  {RequestedClientId, IntendedServerId} =
    xtt_utils:initialize_ids(CredDir, ?REQUESTED_CLIENT_ID_FILE, ?SERVER_ID_FILE),
  {ok, Pid} = xtt_handshake:start_link(
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

  {ok, PrivKeyAsn1} = xtt_erlang:xtt_asn1_from_private_key(LongTermPrivKey),

  {ok, #xtt_creds{identity = Identity, pseudonym = Pseudonym, cert = CertAsn1, key = PrivKeyAsn1}}.
