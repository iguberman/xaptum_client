-ifndef('__xtt_endpoint_hrl__').
-define('__xtt_endpoint_hrl__',true).

-define(DEFAULT_SUBNET, <<38,7,143,128,128,0,0,0>>).

-record(tpm_creds, {basename, tpm_host, tpm_port, tpm_password, client_id, server_id}).

-record(file_creds, {basename, gpk, cred, sk, root_id, root_pk, client_id, server_id}).

-record(cert, {client_id, server_id}).

-record(tls_creds, {identity, pseudonym, cert, key}).

-record(hosts_config, {xaptum_host, xtt_port, tls_port, xcr_host, xcr_port}).

-record(endpoint, {ipv6, num_sent = 0, num_received = 0, num_reconnects = 0, msg = <<>>}).

-endif.