-module(bacnet_control).

-behaviour(gen_server).

%% export API
-export([
	 start_control/0,
	 start_subscriber/2,

	 stop/1,
	 send_message/3,
	 send_message/2,

	 get_message_count/1
	]).

%% export gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("bacnet.hrl").

-record(state, {ip, type, session_token, queue, sent = 0, received = 0, fsm = init, dict, data = <<>>, poll_req = 0, poll_resp = 0}).

%%====================================
%% API
%%====================================
start_control() ->
    {ok, App} = xaptum_client:get_application(),
    {ok, IpFile} = xaptum_client:get_env(App, ipv6_file),
    {ok, Queue} = xaptum_client:get_env(App, dds_queue),

    SubIp = xaptum_client:read_ipv6_file(IpFile),
    start_subscriber(SubIp, Queue).

start_subscriber(SubIp, Queue) when is_list(SubIp), is_list(Queue) ->
    Q = list_to_binary(Queue),
    SIP = xaptum_client:ipv6_to_binary(SubIp),
    gen_xaptum:start_link({local, ?BACNET_CONTROL}, ?MODULE, [{?BACNET_CONTROL, SIP, Q}], []).

send_message(Server, Msg) when is_list(Msg) ->
    send_message(Server, list_to_binary(Msg));
send_message(Server, Msg) when is_binary(Msg) ->
    gen_server:cast(Server, {send, Msg}).

send_message(Server, Dest, Msg) when is_list(Dest), is_list(Msg) ->
    send_message(Server, Dest, list_to_binary(Msg));
send_message(Server, Dest, Msg) when is_list(Dest), is_binary(Msg) ->
    D = xaptum_client:ipv6_to_binary(Dest),
    gen_server:cast(Server, {send, D, Msg}).

stop(Server) ->
    gen_server:cast(Server, stop).

get_message_count(Server) ->
    gen_server:call(Server, get_message_count).
    
%%====================================
%% callbacks
%%====================================
init([{?BACNET_CONTROL, SIP, Q}]) ->
    self() ! init_session,
    {ok, #state{ip = SIP, type = ?BACNET_CONTROL, queue = Q, dict = dict:new()}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(init_session, #state{ip = SIP, type = ?BACNET_CONTROL, queue = Q} = State) ->
    ok = create_dds_subscriber(SIP, Q),
    NewState = State#state{fsm = init},
    send_init_timeout(),
    {noreply, NewState};

handle_info(init_timeout, #state{fsm = init} = State) ->
    {stop, init_timeout, State};

handle_info(init_timeout, #state{fsm = op} = State) ->
    {noreply, State};

handle_info({recv, RawData}, #state{fsm = init, type = ?BACNET_CONTROL, data = Bin} = State) ->
    Data = erlang:list_to_binary([Bin, RawData]),
    <<120, _PacketType:8, _Size:16, SessionToken:36/binary, Rest/binary>> = Data,
    lager:info("Received Authentication Response"),
    poll_loop(write_poll),
    {noreply, State#state{session_token = SessionToken, fsm = op, data = Rest}};
    
handle_info({recv, RawData}, #state{fsm = op, type = ?BACNET_CONTROL, data = Bin} = State) ->
    %% Log dds message packets

    MatchFun = fun Fn(Data, #state{received = R,poll_resp = PRESP, poll_req = PREQ, dict = Dict} = FnState)->
		       case Data of 
			   %% This is a control message
			   <<120, _PacketType:8, Size:16, DdsPayload:Size/bytes, Rest/binary>> ->
			       %% Get bacnet request
			       <<_SessionToken:36/binary, Mdxp/binary>> = DdsPayload,

			       %% Decode the original msg
			       OriginalMsg = base64:decode(ddslib:extract_mdxp_payload(Mdxp)),

			       %% If the Original msg matches heartbeat
			       FnNewState = case OriginalMsg of
						<<?IAM, Ip/binary>> ->
						    DestIp = xaptum_client:ipv6_binary_to_text(Ip),
						    NewDict = dict:store(DestIp, 1, Dict),
						    FnState#state{received = R+1, dict = NewDict, data = Rest};

						BacnetAck ->
						    case bacnet_utils:get_apdu_from_message(BacnetAck) of
							{ok, Apdu} ->
							    lager:info("Sent ~p Poll Requests, Received ~p Poll Responses", [PREQ, PRESP+1]),
							    case bacnet_utils:get_pdu_type(Apdu) of
								pdu_type_simple_ack ->
								    lager:info("Received bacnet Simple ACK"),
								    ignore;
							
								pdu_type_complex_ack ->
								    case bacnet_utils:get_value_from_complex_ack(Apdu) of
									{ok, Id, Tag} ->
									    lager:info("Received bacnet Complex ACK with Id: ~p, Tag: ~p", [Id, Tag]);
									CV ->
									    lager:info("Got ~p while processing Complex Ack. Ignore", [CV])
								    end
							    end;
							ApduError ->
							    lager:info("Got ~p while processing BacknetAck. Ignore", [ApduError])
						    end,
						    FnState#state{received = R+1, poll_resp = PRESP+1, data = Rest}
					    end,
			       Fn(Rest, FnNewState);
			   
			   Rest ->
			       {Rest, FnState}
		       end
	       end,
    {_, NewState} = MatchFun(erlang:list_to_binary([Bin, RawData]), State),
    {noreply, NewState};
       
handle_info({poll_loop, write_poll}, #state{type = ?BACNET_CONTROL, dict = Dict, poll_req = PREQ} = State) ->
    Ips = dict:fetch_keys(Dict),
    lists:foreach( fun(Ip) ->
			   IpBytes = xaptum_client:ipv6_to_binary(Ip),
			   <<Id:64, Tag:64>> = IpBytes,
			   {ok, Control} = bacnet_utils:build_write_property_request(Id, Tag),
			   ?MODULE:send_message(self(), Ip, Control),
			   lager:info("Sending write property request with Id: ~p, Tag: ~p", [Id, Tag])
		   end, Ips),
    poll_loop(read_poll),
    {noreply, State#state{poll_req = PREQ+length(Ips)}};

handle_info({poll_loop, read_poll}, #state{type = ?BACNET_CONTROL, dict = Dict, poll_req = PREQ} = State) ->
    Ips = dict:fetch_keys(Dict),
    lists:foreach( fun(Ip) ->
			   {ok, Control} = bacnet_utils:build_read_property_request(),
			   ?MODULE:send_message(self(), Ip, Control),
			   lager:info("Sending read property request")
		   end, Ips),
    poll_loop(write_poll),
    {noreply, State#state{poll_req = PREQ+length(Ips)}};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({send, Msg}, #state{session_token = ST, sent = S} = State) ->
    %% send a regular message
    Packet = ddslib:build_reg_message(ST, Msg),
    ok = gen_xaptum:send(Packet),
    {noreply, State#state{sent = S+1}};

handle_cast({send, Dest, Msg}, #state{session_token = ST, sent = S} = State) ->
    %% send a control message
    Control = <<Dest/binary,Msg/binary>>,
    Packet = ddslib:build_control_message(ST, Control),
    ok = gen_xaptum:send(Packet),
    {noreply, State#state{sent = S+1}};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_message_count, _From, #state{type = ?BACNET_CONTROL, sent = S, received = R} = State) ->
    Reply = {?BACNET_CONTROL, S, R},
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%====================================
%% Private functions
%%====================================
create_dds_subscriber(Ip, Q) ->
    %% build Authentication Request
    SubReq = ddslib:build_init_sub_req(Ip, Q),
    ok = gen_xaptum:send(SubReq),
    lager:info("Sent subscriber authentication Request"),
    ok.

poll_loop(Type) ->
    erlang:send_after(2500, self(), {poll_loop, Type}).

send_init_timeout() ->
    erlang:send_after(2000, self(), init_timeout).
