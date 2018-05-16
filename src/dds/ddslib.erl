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

	 build_reg_message/2,
	 build_control_message/2,
	 build_init_pub_req/1,
	 build_init_sub_req/2,
	 recv/1,
	 extract_mdxp_payload/1,
   convert_to_xcr_ipv6/1,
   curl_identity_to_xcr/2
]).

-include("dds.hrl").

-define(TOKEN, <<"abcdefghijklmnopqrstuvwzyz1234567890ABCD">>).
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
build_init_pub_req(Guid) ->
    build_req(Guid, ?AUTH_EMP_REQ, <<>>). 

build_init_sub_req(Guid, Queue) ->
    build_req(Guid, ?AUTH_SUB_REQ, Queue).

build_reg_message(SessionToken, Message) when is_list(Message) ->
  build_reg_message(SessionToken, list_to_binary(Message));
build_reg_message(SessionToken, Message) when is_binary(Message) ->
    build_message(SessionToken, ?REG_MSG, Message).


build_control_message(SessionToken, Message) when is_list(Message) ->
  build_control_message(SessionToken, list_to_binary(Message));
build_control_message(SessionToken, Message) when is_binary(Message) ->
    build_message(SessionToken, ?SIGNAL_MSG, Message).

recv(Client) ->
    {ok, FixedHeader} = erltls:recv(Client, 4, 2000),
    <<?DDS_MARKER:8, _Type:8, Size:16>> = FixedHeader,
    {ok, Rest} = erltls:recv(Client, Size, 2000),
    <<FixedHeader/binary, Rest/binary>>.
    
%%=============================================================
%% Private functions
%%=============================================================
build_req(Guid, Type, ReqPayload) ->
    Size = 16 + byte_size(ReqPayload),
    FixedHeader = <<?DDS_MARKER:8, Type:8, Size:16>>,
    VariableHeader = Guid,
    Payload = ReqPayload,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    Packet.

build_message(SessionToken, MsgType, Message) ->
    Size = 36 + byte_size(Message),
    FixedHeader = <<?DDS_MARKER:8, MsgType:8, Size:16>>,
    VariableHeader = SessionToken,
    Payload = Message,
    Packet = <<FixedHeader/binary, VariableHeader/binary, Payload/binary>>,
    Packet.

extract_mdxp_payload(Mdxp) ->
    {match, [Msg]} = re:run(Mdxp, ".*originalPayload\"\s*:\s*\"(.*)\".*$", [{capture, [1], list}, ungreedy]),
    list_to_binary(Msg).

curl_identity_to_xcr(Identity, Type) when Type =:= "D"; Type =:= "S" ->
  {ok, XcrHost} = application:get_env(xaptum_client, xcr_host),
  {ok, XcrPort} = application:get_env(xaptum_client, xcr_port),
  AssignedIp = xtt_client_utils:binary_to_hex(Identity),
  Cmd = "curl -X POST -H \"Content-Type: application/json\" http://" ++ XcrHost ++ ":" ++
    integer_to_list(XcrPort) ++ "/api/xcr/v2/ephook/" ++ AssignedIp ++ "/" ++ Type,
  lager:info("CMD: ~p", [Cmd]),
  Res = os:cmd(Cmd),
  lager:info("Result: ~p", [Res]).