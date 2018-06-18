%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2018 10:05 PM
%%%-------------------------------------------------------------------
-module(xaptum_endpoint).
-author("iguberman").

-behaviour(gen_server).

-include("xtt_endpoint.hrl").
-include_lib("erltls/include/erltls.hrl").

%% API
-export([start_link/4,
  send_message/2,
  send_message/3,
  send_request/2,
  start_receiving/1,
  received/2,
  receive_loop/3,
  get_data/1,
  ssl_error/2,
  connect/1,
  reconnect/1,
  disconnect/1
  ]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  hosts_config,
  xaptum_host, xtt_port, tls_port,
  ipv6, pseudonym, cert, key, tls_socket,
  callback_data, callback_module,
  receiver_pid}).


-callback auth(HostsConfig :: tuple(), Inputs :: term(), CallbackData :: any())->
  {Identity :: binary() , Cert :: binary(), Key :: binary()}.
-callback on_receive(Msg :: binary(), CallbackData :: any()) -> {ok, CallbackData :: any()}.
-callback do_receive(TlsSocket :: tuple()) -> {ok, Msg :: any} | {error, Error :: any}.
-callback on_send(Msg :: binary(), Dest :: any(), CallbackData :: any()) ->
  {ok, Msg :: binary(), CallbackData :: any()}.
-callback on_send(Msg :: binary(), CallbackData :: any()) ->
  {ok, Msg :: binary(), CallbackData :: any()}.
-callback on_disconnect(CallbackData :: any()) -> {ok, CallbackData :: any}.
-callback on_connect(CallbackData :: any()) -> {ok, CallbackData :: any()}.
-callback on_reconnect(CallbackData :: any()) -> {ok, Callbackdata :: any()}.

%% TODO for when xtt is not the ony way to authenticate with xaptum and get tls cert and key
%%-callback get_key(Creds :: tuple()) -> Key :: binary().
%%-callback get_cert(Creds :: tuple()) -> Cert :: binary().
%%-callback get_identity(Creds :: tuple()) -> Identity :: binary().

%%%===================================================================
%%% API
%%%===================================================================

send_request(EndpointPid, Request)->
  gen_server:cast(EndpointPid, {send_request, Request}).

send_message(EndpointPid, Msg, Dest)->
  gen_server:cast(EndpointPid, {send_message, Msg, Dest}).

send_message(EndpointPid, Msg)->
  gen_server:cast(EndpointPid, {send_message, Msg}).

start_receiving(EndpointPid)->
  gen_server:cast(EndpointPid, start_receiving).

received(EndpointPid, Msg)->
  gen_server:cast(EndpointPid, {received, Msg}).

get_data(EndpointPid)->
  gen_server:call(EndpointPid, get_data).

ssl_error(EndpointPid, Error) ->
  gen_server:cast(EndpointPid, {ssl_error, Error}).

connect(EndpointPid)->
  gen_server:cast(EndpointPid, maybe_connect).

reconnect(EndpointPid)->
  gen_server:cast(EndpointPid, maybe_reconnect).

disconnect(EndpointPid) ->
  gen_server:stop(EndpointPid).

start_link(#hosts_config{} = HostsConfig, CallbackModule, CallbackData, Creds) ->
  %% This endpoint is unregistered until a later point when it gets an identity assigned
  gen_server:start_link(?MODULE, [HostsConfig, CallbackModule, CallbackData, Creds], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([#hosts_config{} = HostsConfig,
    CallbackModule, CallbackData, AuthInputs]) ->
  gen_server:cast(self(), {auth, AuthInputs}),
  {ok, #state{
    callback_module = CallbackModule, callback_data = CallbackData,
    hosts_config = HostsConfig}}.

handle_call(get_data, _From, #state{callback_data = CallbackData} = State) ->
  {reply, CallbackData, State}.

handle_cast({auth, Inputs}, #state{
  callback_module = CallbackModule, callback_data = CallbackData0,
  hosts_config = HostsConfig} = State) ->
  AuthResult = CallbackModule:auth(HostsConfig, Inputs, CallbackData0),
  lager:info("AuthResult: ~p", [AuthResult]),
  case AuthResult of
    {ok, #tls_creds{identity = Identity, pseudonym = Pseudonym, cert = Cert, key = Key}, CallbackData1} ->
        register(binary_to_atom(Identity, latin1), self()),
        connect(self()),
        {noreply, State#state{ipv6 = Identity, pseudonym = Pseudonym, cert = Cert, key = Key, callback_data = CallbackData1}};
    Other ->
      lager:warning("Invalid auth result ~p", [Other]),
      {stop, failed_auth, State}
  end;

%% Create new tls connection if there isn't one
handle_cast(maybe_connect, #state{ tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = _ExistingTlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State) when is_port(TcpSocket), is_pid(SslPid) ->
  {ok, CallbackData1} = CallbackModule:on_connect(CallbackData0),
  start_receiving(self()),
  {noreply, State#state{callback_data = CallbackData1}};
handle_cast(maybe_connect, #state{ tls_socket = undefined,
  callback_module = CallbackModule, callback_data = CallbackData0} = State)->
  {ok, TlsSocket} = do_tls_connect(State),
  {ok, CallbackData1} = CallbackModule:on_connect(CallbackData0),
  start_receiving(self()),
  {noreply, State#state{tls_socket = TlsSocket, callback_data = CallbackData1}};

%% Connect if not connected, force reconnect if it is
handle_cast(maybe_reconnect, #state{
  tls_socket = MaybeExistingTlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State) ->
  {ok, TlsSocket} = do_tls_connect(State),
  case MaybeExistingTlsSocket of
    undefined -> %% this WAS NOT a REconnect
      {ok, CallbackData1} = CallbackModule:on_connect(CallbackData0);
    #tlssocket{} -> %% yes, this WAS a REconnect
      erltls:close(MaybeExistingTlsSocket),
      {ok, CallbackData1} = CallbackModule:on_reconnect(CallbackData0)
  end,
  start_receiving(self()),
  {noreply, State#state{tls_socket = TlsSocket, callback_data = CallbackData1}};

handle_cast({send_message, Payload, Dest}, #state{
  tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State)
  when is_port(TcpSocket), is_pid(SslPid) ->
  case CallbackModule:on_send(Payload, Dest, CallbackData0) of
    {error, retry_later} -> {noreply, CallbackData0};
    {ok, Message, CallbackData1} ->
      ok = erltls:send(TlsSocket, Message),
      lager:info("Sent message ~p to ~p", [Message, Dest]),
      {noreply, State#state{callback_data = CallbackData1}}
  end;
handle_cast({send_message, Payload}, #state{
  tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
  case CallbackModule:on_send(Payload, CallbackData0) of
    {error, retry_later} ->
      timer:sleep(100),
      gen_server:cast(self(), maybe_connect),
      gen_server:cast(self(), {send_message, Payload}),
      {noreply, CallbackData0};
    {ok, Message, CallbackData1} ->
      ok = erltls:send(TlsSocket, Message),
      lager:info("Sent message ~p to ~p", [Message]),
      {noreply, State#state{callback_data = CallbackData1}}
  end;
handle_cast({send_request, Request}, #state{
  tls_socket = #tlssocket{} = TlsSocket} = State)->
  case erltls:send(TlsSocket, Request) of
    ok ->
      lager:info("Sent request ~p", [Request]),
      {noreply, State};
    {error, Error} ->
      lager:error("Failed to send request ~p due to error ~p", [Request, Error]),
      {stop, {tls_error, Error}, State}
  end;

%% Handle {active, false} mode
handle_cast(start_receiving, #state{
    tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
    callback_module = CallbackModule, callback_data = CallbackData} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
  EndpointPid = self(),
  lager:info("Spawning receive_loop with args: ~p, ~p, ~p", [TlsSocket, EndpointPid, CallbackModule]),
  ReceiverPid = spawn_link(?MODULE, receive_loop, [TlsSocket, EndpointPid, CallbackModule]),
  {noreply, State#state{receiver_pid = ReceiverPid, callback_data = CallbackData}};

handle_cast({received, Msg}, #state{callback_module = CallbackModule, callback_data = CallbackData} = State) ->
  CallbackModule:on_receive(Msg, CallbackData);

handle_cast({ssl_error, _Error}, #state{tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
    erltls:close(TlsSocket),
    connect(self()),
  {noreply, State}.

%% Handle {active, [once|N]} mode -- TODO maybe remove as there is less control over how messages are received
handle_info({ssl, TlsSocket, Msg}, #state{tls_socket = TlsSocket, callback_module = CallbackModule, callback_data = CallbackData0} = State) ->
  {ok, CallbackData1} = CallbackModule:on_receive(Msg, CallbackData0),
  {noreply, State#state{callback_data = CallbackData1}};
handle_info({ssl_error, TlsSocket, _Error}, #state{tls_socket = TlsSocket} = State)->
  erltls:close(TlsSocket),
  connect(self()),
  {noreply, State}.

terminate(_Reason, #state{tls_socket = undefined}) ->
  ok;
terminate(_Reason, #state{tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0}) when is_port(TcpSocket), is_pid(SslPid) ->
  erltls:close(TlsSocket),
  {ok, _CallbackData1} = CallbackModule:on_disconnect(CallbackData0),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_tls_connect(#state{
  hosts_config = #hosts_config{xaptum_host = XaptumHost, tls_port = TlsPort},
  cert = Cert, key = Key}) when is_binary(Cert), is_binary(Key) ->
  erltls:connect(XaptumHost, TlsPort,
    [binary,
      {active, false},
      {reuseaddr, true},
      {packet, 0},
      {keepalive, true},
      {nodelay, true},
      {verify, verify_none},
      {fail_if_no_peer_cert, false},
      {cert, Cert},
      {key, Key}
    ],2000).


receive_loop(TlsSocket, EndpointPid, CallbackModule) ->
  lager:debug("receive_loop(~p, ~p, ~p)", [TlsSocket, EndpointPid, CallbackModule]),
  case CallbackModule:do_receive(TlsSocket) of
    {ok, Msg} ->
      lager:info("Received ~p", [Msg]),
      xaptum_endpoint:received(Msg, EndpointPid),
      receive_loop(TlsSocket, EndpointPid, CallbackModule);
    {error, Error} ->
      xaptum_endpoint:ssl_error(EndpointPid, Error)
  end.