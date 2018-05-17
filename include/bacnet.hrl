-ifndef('__bacnet_hrl__').
-define('__bacnet_hrl__',true).

-include("dds.hrl").

-define(BACNET_PROXY, bacnet_proxy).
-define(BACNET_CONTROL, bacnet_control).
-define(IAM, "IAM").

-record(bacnet_pub, {dds = #dds{}, udp_sent = 0, udp_recv = 0, udp_data, udp_socket, heartbeat_pid}).

-record(bacnet_sub, {dds = #dds{}, dict = dict:new(), write_reqs = 0, read_reqs = 0, poll_reqs = 0, poll_pid}).

-endif.
