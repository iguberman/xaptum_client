%%-------------------------------------------------------------------------------------------
%% 
%% XAPTUM CONFIDENTIAL
%% __________________
%% 
%%  2017(C) Xaptum, Inc.
%%  All Rights Reserved.Patents Pending.
%% 
%% NOTICE:  All information contained herein is, and remains
%% the property of Xaptum, Inc.  The intellectual and technical concepts contained
%% herein are proprietary to Xaptum, Inc and may be covered by U.S. and Foreign Patents,
%% patents in process, and are protected by trade secret or copyright law.
%% Dissemination of this information or reproduction of this material
%% is strictly forbidden unless prior written permission is obtained
%% from Xaptum, Inc.
%%
%% @author Venkatakumar Srinivasan
%%
%%-------------------------------------------------------------------------------------------
-module(ddslib).

-export([
  client_hello/1,
  server_hello/1,
  subscribe_request/1,
  control_request/2,
  reg_msg_request/1,
  recv/1,
  extract_mdxp_payload/1,
  curl_identity_to_xcr/2
]).

-include("dds.hrl").


-define(TYPE, <<0>>).
-define(TOTAL, 10).
-define(DELAY, 200).

%%-define(MB_HOST, "broker.xaptum.net").
-define(MB_HOST, {192,168,1,10}).

-ifndef(CONNECT_DELAY).
-define(CONNECT_DELAY, 5).
-endif.

-ifndef(TOTAL_MSG).
-define(TOTAL_MSG, 10000).
-endif.

-ifndef(MSG_BYTE_SIZE).
-define(MSG_BYTE_SIZE, 8).
-endif.

-ifndef(MSG_DELAY).
-define(MSG_DELAY, 4).
-endif.

-define(LOG_PROC, test_log_proc).
-define(LOG_SQL, <<"insert into test_log(dev_id, msg_id, message, type, ts) values (?,?,?,?,?)">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% DDS Protocol Implementation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

client_hello(Ipv6) ->
  <<?DDS_MARKER, ?DDS_CLIENT_HELLO, ?IPV6_SIZE:16, Ipv6:?IPV6_SIZE/bytes>>.

server_hello(Ipv6) ->
  <<?DDS_MARKER, ?DDS_SERVER_HELLO, ?IPV6_SIZE:16, Ipv6:?IPV6_SIZE/bytes>>.

control_request(Message, DestIpv6) when is_list(Message)->
  control_request(list_to_binary(Message), DestIpv6);
control_request(Message, DestIpv6) when is_binary(Message)->
  Payload = <<DestIpv6/binary, Message/binary>>,
  dds_payload(Payload, ?CONTROL_MSG).

reg_msg_request(Message) when is_list(Message) ->
  reg_msg_request(list_to_binary(Message));
reg_msg_request(Message) when is_binary(Message) ->
  dds_payload(Message, ?REG_MSG).

subscribe_request(Queue)->
  lager:info("Creating sub request from queue ~p", [Queue]),
  Payload = list_to_binary(Queue),
  dds_payload(Payload, ?SUB_REQ).

dds_payload(Payload, Type) when is_integer(Type), is_binary(Payload) ->
  Size = size(Payload),
  <<?DDS_MARKER, Type, Size:16, Payload/binary>>.

recv(Client) ->
  case erltls:recv(Client, 4, 5000) of
    {ok, FixedHeader} ->
      <<?DDS_MARKER:8, _Type:8, Size:16>> = FixedHeader,
       case erltls:recv(Client, Size, 2000) of
         {ok, Rest} -> {ok, <<FixedHeader/binary, Rest/binary>>};
         {error, Error} -> lager:error("Error receiving expected ~b bytes", [Size]),
           {error, Error}
       end;
    {error, Error} -> lager:error("Error ~p receiving dds header", [Error]),
      {error, Error}
  end.

extract_mdxp_payload(Mdxp) ->
    {match, [Msg]} = re:run(Mdxp, ".*originalPayload\"\s*:\s*\"(.*)\".*$", [{capture, [1], list}, ungreedy]),
    list_to_binary(Msg).

curl_identity_to_xcr(Identity, Type) when Type =:= "D"; Type =:= "S" ->
  {ok, XcrHost} = application:get_env(xaptum_client, xcr_host),
  {ok, XcrPort} = application:get_env(xaptum_client, xcr_port),
  AssignedIp = xtt_client_utils:bin_to_hex(Identity),
  Cmd = "curl -X POST -H \"Content-Type: application/json\" http://" ++ XcrHost ++ ":" ++
    integer_to_list(XcrPort) ++ "/api/xcr/v2/ephook/" ++ AssignedIp ++ "/" ++ Type,
  lager:info("CMD: ~p", [Cmd]),
  Res = os:cmd(Cmd),
  lager:info("Result: ~p", [Res]).
