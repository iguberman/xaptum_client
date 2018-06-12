-module(xaptum_client).

-behaviour(gen_server).

%% export API
-export([
	 start_device/0,
	 start_subscriber/0,

	 start_device/1,
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

-export([
	 get_application/0,
	 get_env/2,
	 priv_dir/0,
	 read_ipv6_file/1,
	 ipv6_to_binary/1,
	 ipv6_binary_to_text/1
]).

-include("dds.hrl").

-record(state, {ip, type, session_token, queue, sent = 0, received = 0, fsm = init, data = <<>>}).

%%====================================
%% API
%%====================================
start_device() ->
    {ok, App} = xaptum_client:get_application(),
    {ok, IpFile} = xaptum_client:get_env(App, ipv6_file),

    DeviceIp = read_ipv6_file(IpFile),
    start_device(DeviceIp).

start_subscriber() ->
    {ok, App} = xaptum_client:get_application(),
    {ok, IpFile} = xaptum_client:get_env(App, ipv6_file),
    {ok, Queues} = xaptum_client:get_env(App, dds_queues),

    SubIp = read_ipv6_file(IpFile),
    start_subscriber(SubIp, Queue).

start_device(DeviceIp) when is_list(DeviceIp) ->
    DIP = ipv6_to_binary(DeviceIp),
    gen_xaptum:start_link({local, ?DEVICE}, ?MODULE, [{?DEVICE, DIP}], []).

start_subscriber(SubIp, Queue) when is_list(SubIp), is_list(Queue) ->
    Q = list_to_binary(Queue),
    SIP = ipv6_to_binary(SubIp),
    gen_xaptum:start_link({local, ?SUBSCRIBER}, ?MODULE, [{?SUBSCRIBER, SIP, Q}], []).

send_message(Server, Msg) when is_list(Msg) ->
    send_message(Server, list_to_binary(Msg));
send_message(Server, Msg) when is_binary(Msg) ->
    gen_server:cast(Server, {send, Msg}).

send_message(Server, Dest, Msg) when is_list(Dest), is_list(Msg) ->
    send_message(Server, Dest, list_to_binary(Msg));
send_message(Server, Dest, Msg) when is_list(Dest), is_binary(Msg) ->
    D = ipv6_to_binary(Dest),
    gen_server:cast(Server, {send, D, Msg}).

stop(Server) ->
    gen_server:cast(Server, stop).

get_message_count(Server) ->
    gen_server:call(Server, get_message_count).
    
%%====================================
%% Helper API's
%%====================================
get_application() ->
    xaptum_client_app:get_application().

get_env(App, EnvVar) ->
    xaptum_client_app:get_env(App, EnvVar).

priv_dir() ->
    xaptum_client_app:priv_dir().

read_ipv6_file(IpFile) ->
    {ok, File} = file:read_file(IpFile),
    <<Ip:32/binary, _Rest/binary>> = File,
    <<A:4/binary, B:4/binary, C:4/binary, D:4/binary, E:4/binary, F:4/binary, G:4/binary, H:4/binary>> = Ip,
    StrictIp = <<A/binary, ":", B/binary, ":", C/binary, ":", D/binary, ":", E/binary, ":", F/binary, ":", G/binary, ":", H/binary>>,
    binary_to_list(StrictIp).
    
ipv6_to_binary(IpText) ->
    {ok, Ip} = inet:parse_ipv6_address(IpText),
    List = [ <<I:16>> || I <- tuple_to_list(Ip) ],
    iolist_to_binary(List).

ipv6_binary_to_text(IpBin) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>> = IpBin,
    inet:ntoa({A,B,C,D,E,F,G,H}).
%%====================================
%% callbacks
%%====================================
init([{?DEVICE, DIP}]) ->
    self() ! init_session,
    {ok, #state{ip = DIP, type = ?DEVICE}};

init([{?SUBSCRIBER, SIP, Q}]) ->
    self() ! init_session,
    {ok, #state{ip = SIP, type = ?SUBSCRIBER, queue = Q}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(init_session, #state{ip = DIP, type = ?DEVICE} = State) ->
    ok = create_dds_device(DIP),
    NewState = State#state{fsm = init},
    send_init_timeout(),
    {noreply, NewState};

handle_info(init_session, #state{ip = SIP, type = ?SUBSCRIBER, queue = Q} = State) ->
    ok = create_dds_subscriber(SIP, Q),
    NewState = State#state{fsm = init},
    send_init_timeout(),
    {noreply, NewState};

handle_info(init_timeout, #state{fsm = init} = State) ->
    {stop, init_timeout, State};

handle_info(init_timeout, #state{fsm = op} = State) ->
    {noreply, State};

handle_info(send_loop, #state{ip = DIP, type = ?DEVICE} = State) ->
    Msg = DIP, %%<<"buzzwigs">>,
    ?MODULE:send_message(self(), Msg),
    send_loop(),
    {noreply, State};

handle_info({recv, RawData}, #state{fsm = init, type = Type, data = Bin} = State) ->
    Data = erlang:list_to_binary([Bin, RawData]),
    <<120, _PacketType:8, _Size:16, SessionToken:36/binary, Rest/binary>> = Data,
    lager:info("Received Authentication Response"),
    case Type of
	?SUBSCRIBER ->
	    noop;
	?DEVICE ->
	    send_loop()
    end,
    {noreply, State#state{session_token = SessionToken, fsm = op, data = Rest}};

    
handle_info({recv, RawData}, #state{fsm = op, type = T, received = R, sent = S, data = Bin} = State) ->
    %% Log dds message packets
    lager:info("Sent ~p Packets, Received ~p Packets", [S, R+1]),
    MatchFun = fun Fn(Data)->
		       case Data of 
			   <<120, _PacketType:8, Size:16, DdsPayload:Size/bytes, Rest/binary>> ->
			       case T of
				   ?SUBSCRIBER ->
				       %% Send control message
				       <<_SessionToken:36/binary, Mdxp/binary>> = DdsPayload,
				       Control = <<"zigzaggy">>,
				       OriginalMsg = base64:decode(ddslib:extract_mdxp_payload(Mdxp)),
				       DestIp = ipv6_binary_to_text(OriginalMsg),	    
				       ?MODULE:send_message(self(), DestIp, Control);
			   
				   ?DEVICE ->
				       ignore
			       end,
			       Fn(Rest);
			   Rest ->
			       Rest
		       end
	       end,
    Rest = MatchFun(erlang:list_to_binary([Bin, RawData])),
    {noreply, State#state{received = R+1, data = Rest}};

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

handle_call(get_message_count, _From, #state{type = T, sent = S, received = R} = State) ->
    Reply = {T, S, R},
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%====================================
%% Private functions
%%====================================
create_dds_device(Ip) ->
    %% Build authentication
    PubReq = ddslib:build_init_pub_req(Ip),
    ok = gen_xaptum:send(PubReq),
    lager:info("Sent device authentication Request"),
    ok.

create_dds_subscriber(Ip, Q) ->
    %% build Authentication Request
    SubReq = ddslib:build_init_sub_req(Ip, Q),
    ok = gen_xaptum:send(SubReq),
    lager:info("Sent subscriber authentication Request"),
    ok.

send_loop() ->
    erlang:send_after(1000, self(), send_loop).

send_init_timeout() ->
    erlang:send_after(2000, self(), init_timeout).
