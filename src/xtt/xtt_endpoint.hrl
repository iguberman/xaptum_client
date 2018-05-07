
-include_lib("xtt_erlang/include/xtt.hrl").

-define(XTT_VERSION, ?XTT_VERSION_ONE).
-define(XTT_SUITE, ?XTT_X25519_LRSW_ED25519_AES256GCM_SHA512).

-record(tpm_creds, {cred_dir, tpm_host, tpm_port, tpm_password}).

-record(file_creds, {cred_dir}).

-record(xtt_creds, {identity, pseudonym, cert, key}).

-record(endpoint_data, {num_sent = 0, num_received = 0, num_reconnects}).
