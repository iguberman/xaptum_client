%%%-------------------------------------------------------------------
%%% @author zanebeckwith
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% xdaa implements the Xaptum TLS-DAA handshake protocol
%%%-------------------------------------------------------------------
-module(xdaa).

%% API
-export([start/4, get_group_keys/1]).

-define(TIMEOUT, 5000).
-define(XDAA_VERSION, 0).

%% Sizes are in octets
-define(XDAA_GID_LENGTH, 16).
-define(XDAA_NONCE_LENGTH, 32).
-define(XDAA_ECDHE_PUB_KEY_LENGTH, 32).
-define(XDAA_SERVER_KEY_EXCHANGE_HEADER_LENGTH, 9).

%%%===================================================================
%%% API
%%%===================================================================

start(TCPSocket, GID, MyDSAPrivKey, ServerDSAPubKey) ->
        xdaa_send_client_hello(TCPSocket, GID, MyDSAPrivKey, ServerDSAPubKey).

get_group_keys(GroupKeysFileName) ->
        {ok, GroupKeysFile} = file:open(GroupKeysFileName, [read]),

        %% Ignore first line of file.
        {ok, _} = file:read_line(GroupKeysFile),

        {ok, Line} = file:read_line(GroupKeysFile),
        [GIDRaw,OthersPublicKeyRaw,MyPrivateKeyRaw] = string:tokens(Line, ",\n\r"),

        file:close(GroupKeysFile),

        {ok,
         list_to_binary(GIDRaw),
         list_to_integer(MyPrivateKeyRaw, 16),
         list_to_integer(OthersPublicKeyRaw, 16)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

xdaa_send_client_hello(TCPSocket, GID, MyDSAPrivKey, ServerDSAPubKey) ->
        ClientNonce = enacl:randombytes(?XDAA_NONCE_LENGTH),
        Packet = <<?XDAA_VERSION:8,
                   ?XDAA_GID_LENGTH:16/big,
                   ?XDAA_NONCE_LENGTH:16/big,
                   GID/binary,
                   ClientNonce/binary>>,
        lager:debug("Sending XDAA ClientHello (~p)...", [Packet]),
        case gen_tcp:send(TCPSocket, Packet) of
                ok ->
                        lager:debug("Sent XDAA ClientHello (~p) of length ~p octets", [Packet, bit_size(Packet) div 8]),
                        xdaa_wait_on_server_key_exchange_header(TCPSocket, GID, ClientNonce, MyDSAPrivKey, ServerDSAPubKey);
                {error, Reason} ->
                        lager:warning("XDAA: Error sending ClientHello: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_wait_on_server_key_exchange_header(TCPSocket, GID, ClientNonce, MyDSAPrivKey, ServerDSAPubKey) ->
        case gen_tcp:recv(TCPSocket, ?XDAA_SERVER_KEY_EXCHANGE_HEADER_LENGTH, ?TIMEOUT) of
                {ok, <<?XDAA_VERSION:8,
                       ?XDAA_GID_LENGTH:16/big,
                       ?XDAA_NONCE_LENGTH:16/big,
                       ?XDAA_ECDHE_PUB_KEY_LENGTH:16/big,
                       ServerSigLength:16/big>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchangeHeader with SigLength:~p", [ServerSigLength]),
                        xdaa_wait_on_server_key_exchange(TCPSocket, GID, ServerSigLength, ClientNonce, MyDSAPrivKey, ServerDSAPubKey);
                {ok, InvalidKeyExchangeHeaderPacket} ->
                        xdaa_handle_invalid_key_exchange_header(InvalidKeyExchangeHeaderPacket);
                {error, Reason} ->
                        lager:warning("XDAA: Error receiving ServerKeyExchange header: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_handle_invalid_key_exchange_header(<<?XDAA_VERSION:8,
                                        ServerGIDLength:16/big,
                                        ServerNonceLength:16/big,
                                        ServerECDHEPubKeyLength:16/big,
                                        ServerSigLength:16/big>>) ->
        lager:warning("XDAA: Received ServerKeyExchange header with incorrect lengths - " ++
                         "GIDLength:~p(not ~p), NonceLength:~p(not ~p), PubKeyLength:~p(not ~p), and SigLength:~p",
                      [ServerGIDLength, ?XDAA_GID_LENGTH, ServerNonceLength, ?XDAA_NONCE_LENGTH, ServerECDHEPubKeyLength, ?XDAA_ECDHE_PUB_KEY_LENGTH, ServerSigLength]),
        {error, "XDAA: Incorrect lengths in ServerKeyExchange header"};
xdaa_handle_invalid_key_exchange_header(<<Version:8, _/binary>>) ->
        lager:warning("XDAA: Received ServerKeyExchange header with unsupported version: ~p(not ~p)", [Version, ?XDAA_VERSION]),
        {error, "XDAA: Unsupported version in ServerKeyExchange header"};
xdaa_handle_invalid_key_exchange_header(_) ->
        lager:warning("XDAA: Received mal-formed ServerKeyExchange header", []),
        {error, "XDAA: Mal-formed ServerKeyExchange header"}.

xdaa_wait_on_server_key_exchange(TCPSocket, GID, ServerSigLength, ClientNonce, MyDSAPrivKey, ServerDSAPubKey) ->
        case gen_tcp:recv(TCPSocket, ?XDAA_GID_LENGTH+?XDAA_NONCE_LENGTH+?XDAA_ECDHE_PUB_KEY_LENGTH+ServerSigLength, ?TIMEOUT) of
                {ok, <<ServerGID:?XDAA_GID_LENGTH/binary-unit:8,
                       ServerNonce:?XDAA_NONCE_LENGTH/binary-unit:8,
                       ServerECDHEPubKeySwapped:?XDAA_ECDHE_PUB_KEY_LENGTH/binary-unit:8,
                       ServerSig:ServerSigLength/binary-unit:8>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchange with GID:~p, Nonce:~p, SwappedPubKey:~p, and Sig:~p",
                                    [ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig]),
                        xdaa_validate_server_gid(TCPSocket, GID, ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPrivKey, ServerDSAPubKey);
                {ok, _} ->
                        lager:warning("XDAA: Received mal-formed ServerKeyExchange", []),
                        {error, "XDAA: Mal-formed ServerKeyExchange message"};
                {error, Reason} ->
                        {error, Reason}
        end.

xdaa_validate_server_gid(TCPSocket, GID, GID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPrivKey, ServerDSAPubKey) ->
        lager:debug("Server GID (~p) accepted", [GID]),
        xdaa_validate_server_signature(TCPSocket, ServerDSAPubKey, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPrivKey);
xdaa_validate_server_gid(_, _, NonMatchingServerGID, _, _, _, _, _, _) ->
        lager:info("XDAA: Server GID (~p) not recognized", [NonMatchingServerGID]),
        {error, "XDAA: Server GID not recognized"}.

xdaa_validate_server_signature(TCPSocket, ServerDSAPubKey, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPrivKey) ->
        SigStruct = <<ServerECDHEPubKeySwapped/binary, ClientNonce/binary>>,
        case crypto:verify(ecdsa,
                           sha256,
                           SigStruct,
                           ServerSig,
                           [ServerDSAPubKey, secp256r1] ) of
                true ->
                        lager:debug("XDAA: Validation of server signature successful", []),
                        ServerECDHEPubKey = reverse_bytes(ServerECDHEPubKeySwapped),
                        xdaa_generate_ecdhe_keys(TCPSocket, ServerNonce, ServerECDHEPubKey, MyDSAPrivKey);
                false ->
                        lager:warning("XDAA: Validation of server signature failed", []),
                        {error, "XDAA: Validation of server signature failed"}
        end.

xdaa_generate_ecdhe_keys(TCPSocket, ServerNonce, ServerECDHEPubKey, MyDSAPrivKey) ->
        #{public := ClientECDHEPubKey, secret := ClientECDHEPrivKey} = enacl:kx_keypair(),
        case bit_size(ClientECDHEPubKey) div 8 of
                ?XDAA_ECDHE_PUB_KEY_LENGTH ->
                        xdaa_generate_signature(TCPSocket, ServerNonce, ServerECDHEPubKey, ClientECDHEPubKey, ClientECDHEPrivKey, MyDSAPrivKey);
                PubKeyLength ->
                        lager:warning("XDAA: Generated ECDHE key with unexpected length: ~p (not ~p)", [PubKeyLength, ?XDAA_ECDHE_PUB_KEY_LENGTH]),
                        {error, "XDAA: Generated ECDHE key with unexpected length"}
        end.

xdaa_generate_signature(TCPSocket, ServerNonce, ServerECDHEPubKey, ClientECDHEPubKey, ClientECDHEPrivKey, MyDSAPrivKey) ->
        ClientECDHEPubKeySwapped = reverse_bytes(ClientECDHEPubKey),
        SigStruct = <<ClientECDHEPubKeySwapped/binary, ServerNonce/binary>>,
        Signature = crypto:sign(ecdsa,
                                sha256,
                                SigStruct,
                                [MyDSAPrivKey, secp256r1]),
        SignatureLength = bit_size(Signature) div 8,
        xdaa_send_client_key_exchange(TCPSocket, ServerECDHEPubKey, Signature, SignatureLength, ClientECDHEPubKeySwapped, ClientECDHEPrivKey).

xdaa_send_client_key_exchange(TCPSocket, ServerECDHEPubKey, Signature, SignatureLength, ClientECDHEPubKeySwapped, ClientECDHEPrivKey) ->
        Packet = <<?XDAA_VERSION:8,
                   ?XDAA_ECDHE_PUB_KEY_LENGTH:16/big,
                   SignatureLength:16/big,
                   ClientECDHEPubKeySwapped/binary,
                   Signature/binary>>,
        lager:debug("XDAA: Sending XDAA ClientKeyExchange (~p)...", [Packet]),
        case gen_tcp:send(TCPSocket, Packet) of
                ok ->
                        lager:debug("XDAA: Sent XDAA ClientKeyExchange (~p) of length ~p octets", [Packet, bit_size(Packet) div 8]),
                        xdaa_run_diffie_hellman(TCPSocket, ServerECDHEPubKey, ClientECDHEPrivKey);
                {error, Reason} ->
                        lager:warning("XDAA: Error sending ClientKeyExchange: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_run_diffie_hellman(TCPSocket, ServerECDHEPubKey, ClientECDHEPrivKey) ->
        DHSharedSecret = enacl:curve25519_scalarmult(ClientECDHEPrivKey, ServerECDHEPubKey),
        lager:debug("Got shared-secret:~p, starting TLS handshake...", [DHSharedSecret]),
        DHSharedSecretSwapped = reverse_bytes(DHSharedSecret),
        tls_connect(TCPSocket, DHSharedSecretSwapped).

tls_connect(TCPSocket, DHSharedSecretSwapped) ->
        SSLOptions = [{ciphers, [{psk, aes_256_gcm, null, sha384}]},
                      {psk_identity, "id"},
                      {user_lookup_fun, {fun(psk, _, UserState) -> {ok, UserState} end, <<DHSharedSecretSwapped/binary>>}}],
        case ssl:connect(TCPSocket, SSLOptions, ?TIMEOUT) of
                {ok, SSLSocket} ->
                        lager:debug("XDAA: Completed TLS handshake"),
                        finish(SSLSocket);
                {error, Reason} ->
                        lager:warning("XDAA: Error performing TLS handshake: ~p", [Reason]),
                        {error, Reason}
        end.

finish(SSLSocket) ->
        {ok, SSLSocket}.

reverse_bytes(Input) ->
        %% Interpret input as little-endian, then re-interpret as big-endian.
        Size = size(Input),
        <<AsLittle:Size/little-unit:8>> = Input,
        <<AsLittle:Size/big-unit:8>>.
