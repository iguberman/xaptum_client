-ifndef('__dds_hrl__').
-define('__dds_hrl__',true).

-include("xtt_endpoint.hrl").

-define(DEVICE, device).
-define(SUBSCRIBER, subscriber).

-record(dds, {endpoint_data = #endpoint{}, num_connects = 0, ready = false, sub_queues = []}).

-define(DDS_MARKER, 120).

-define(NOOP, 0).

-define(SUB_REQ, 88).
-define(READY, 208).

-define(REG_MSG, 16).
-define(CONTROL_MSG, 148).
-define(OBB_MSG, 48).

-define(IPV6_SIZE, 16).

-define(CONNECT, 0).
-define(DISCONNECT, 1).

-endif.


