-ifndef('__xtt_endpoint_hrl__').
-define('__xtt_endpoint_hrl__',true).

-record(tpm_creds, {basename, tpm_host, tpm_port, tpm_password, client_id, server_id}).

-record(file_creds, {basename, gpk, cred, sk, root_id, root_pk, client_id, server_id}).

-record(cert, {client_id, server_id}).

-record(xtt_creds, {identity, pseudonym, cert, key}).

-record(endpoint, {ipv6, num_sent = 0, num_received = 0, num_reconnects = 0, msg = <<>>}).

-endif.