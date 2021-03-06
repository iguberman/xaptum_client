# xaptum_client

`xaptum_client` is an Erlang application for connecting to and transfering
messages through the Xaptum Edge Network Fabric. It can be run standalone or
as part of message handling application.

Implement (or specify via the config) the `message_handler` callback on the
provided `gen_xaptum` behavior to handle incoming messages.  If not set, a
default `dummy_message_handler` that simply prints the message to console is
used.

xaptum_client can be run as a 
1. single device or subscriber (`single` mode)
1. gateway with multiple devices and/or subscribers (`multi` mode)

## Requirements
Requires the libsodium library, and at least in version 1.0.12.
Note: If installing on systems which cuts packages into subpackages,
make sure you also get the "-dev" package containing the header files necessary in order to compile software linking to libsodium.

## Single (or device/subscriber) Mode Usage

The client can either be a device that sends messages to queues or a
subscriber that reads messages from these queues and sends control messages
back to the devices. Run the following `device` and `subscriber` examples
concurrently.

### Device

To test sending messages to preconfigured queues:
  
1. Set the `group_keys_file` variable in `sys.config` to the location of the `group_keys.csv` file
provided by Xaptum.

1. Set the environment variables `XAPTUM_DEV_GUID`, `XAPTUM_DEV_USER`, and
`XAPTUM_DEV_TOKEN` with device credentials issued by Xaptum.
  
1. Run the standalone device client.

        make device-console
  
1. Send a test message (either string or binary).

        xaptum_device:send_message(Message).
  
If no subscriber is online, messages will be buffered in this device's queue(s) (up to 1000 messages)

The subscriber will receive a JSON string with the message as a base64-encoded
payload. For example,

    xaptum_device:send_message("Hello online sub 2!").

results in the subscriber receiving the following message

    <<"{\"messageId\":1494541243945767,\"originalPayload\":\"SGVsbG8gb25saW5lIHN1YiAyIQ==\",\"vbId\":83,\"accountId\":23,\"domainId\":23}">>
 

### Subscriber

To test receiving messages from device queues and sending messages to a device:
 
1. Set the `group_keys_file` variable in `sys.config` to the location of the `group_keys.csv` file
provided by Xaptum.

1. Set the environment variables `XAPTUM_SUB_GUID`, `XAPTUM_SUB_USER`, and
 `XAPTUM_SUB_TOKEN` with subscriber credentials issued by Xaptum. Set
 `XAPTUM_SUB_QUEUE` to indicate the queue on which this subscriber should
 listen.
 
1. Run the standalone subscriber client.

        make subscriber-console

1. Send a control message (string or binary) to a device identified by its
 IPv6 GUID (for the purpose of this test, equivalent to XAPTUM_DEV_GUID from previous section).
 
        xaptum_subscriber:send_message(ControlMessage, DeviceIpv6).
 
Unlike device messages, control messages are received as-is, not as a
base64-encoded payload in a JSON string.


## Multi (or gateway) Mode Usage

1. Run gateway client.
   
   `make gateway-console`
   
1. Start device(s)
   
    `xaptum_device:start(DeviceIpv6Str, DeviceUser, DeviceToken)` or
    
    `xaptum_device:start(DeviceIpv6Str, DeviceUser, DeviceToken, RegNameAtom)`
    
1. Start subscriber(s)
   
    `xaptum_subscriber:start(SubscriberIpv6Str, SubUser, SubToken, Queue)`  or
    
    `xaptum_subscriber:start(SubscriberIpv6Str, SubUser, SubToken, Queue, RegNameAtom)`

1. Device sends regular message 
    
    `xaptum_device:send_message(SrcDeviceIpv6Str, Message)` or
    
    `xaptum_device:send_message(RegNameAtom, Message)`
    
1. Subscriber sends control message to device
    
    `xaptum_subscriber:send_message(SrcSubscriberIpv6Str, Message, DestDeviceIpv6Str)` or
    
    `xaptum_subscriber:send_message(RegNameAtom, Message, DestDeviceIpv6Str)`
    

### Code

1. `gen_xaptum.erl` 
a gen_server implementing xaptum communication protocol and contains all the 
functionality common to both device and subscriber 
It handles device/subscriber authentication and subsequent sending and receiving of messages

1. `xaptum_dummy_message_handler.erl` default `gen_xaptum` behavior implementation. 
Prints messages to console. 

1. `xaptum_device.erl` 
 implements device-specific functionality and is used by gen_xaptum
 
1. `xaptum_subscriber.erl` 
 implements subscriber-specific functionality and is used by gen_xaptum
