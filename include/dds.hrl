-ifndef('__dds_hrl__').
-define('__dds_hrl__',true).

-include("xtt_endpoint.hrl").

-define(DEVICE, device).
-define(SUBSCRIBER, subscriber).

-record(dds, {endpoint_data = #endpoint{}, num_connects = 0, ready = false, sub_queues = [], prev_bytes = <<>>}).

-define(DDS_MARKER, 120).

-define(NOOP, 0).
-define(DDS_CLIENT_HELLO, 80).
-define(SUB_REQ, 88).
-define(DDS_SERVER_HELLO, 208).

-define(REG_MSG, 16).
-define(CONTROL_MSG, 148).
-define(OBB_MSG, 48).

-define(IPV6_SIZE, 16).

-endif.


