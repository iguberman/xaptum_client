-ifndef('__dds_hrl__').
-define('__dds_hrl__',true).

-include("xtt_endpoint.hrl").

-define(DEVICE, device).
-define(SUBSCRIBER, subscriber).

-record(dds, {endpoint_data = #endpoint{}, session_token, queue}).

-define(DDS_MARKER, 120).

-define(NOOP, 0).

-define(AUTH_EMP_REQ, 64).
-define(AUTH_REG_REQ, 80).
-define(AUTH_OBB_REQ, 112).
-define(AUTH_SUB_REQ, 88).
-define(AUTH_RES, 208).

-define(REG_MSG, 16).
-define(SIGNAL_MSG, 148).
-define(OBB_MSG, 48).

-define(SESSION_TOKEN_SIZE, 36).
-define(GUID_SIZE, 16).
-define(AUTH_INFO_SIZE, ?GUID_SIZE).


-endif.


