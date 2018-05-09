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
-export([start_link/5,
  send_message/2,
  send_message/3,
  send_request/2,
  start_receiving/1,
  get_data/1,
  set_data/2,
  ssl_error/2,
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
  xaptum_host, xaptum_port,
  ipv6, pseudonym, cert, key, tls_socket,
  callback_data, callback_module,
  receiver_pid}).


-callback auth(Host :: list(), Port :: integer(), Creds :: term(), CallbackData :: any())->
  {Identity :: binary() , Cert :: binary(), Key :: binary()}.
-callback on_receive(Msg :: binary(), EndpointPid :: pid(), CallbackData :: any()) -> {ok, CallbackData :: any()}.
-callback receive_loop(TlsSocket :: tuple(), EndpointPid :: pid(), CallbackData :: any()) -> ok.
-callback on_send(Msg :: binary(), Dest :: any(), CallbackData :: any()) ->
  {Msg :: binary(), CallbackData :: any()}.
-callback on_send(Msg :: binary(), CallbackData :: any()) ->
  {Msg :: binary(), CallbackData :: any()}.
-callback on_disconnect(EndpointPid :: pid(), CallbackData :: any()) -> {ok, CallbackData :: any}.
-callback on_connect(EndpointPid :: pid(), CallbackData :: any()) -> {ok, CallbackData :: any()}.
-callback on_reconnect(EndpointPid :: pid(), CallbackData :: any()) -> {ok, Callbackdata :: any()}.

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

get_data(EndpointPid)->
  gen_server:call(EndpointPid, get_data).

set_data(EndpointPid, NewData)->
  gen_server:cast(EndpointPid, {set_data, NewData}).

ssl_error(EndpointPid, Error) ->
  gen_server:cast(EndpointPid, {ssl_error, Error}).

disconnect(EndpointPid) ->
  gen_server:stop(EndpointPid).

start_link(CallbackModule, CallbackData, Creds, XaptumHost, XaptumPort) ->
  %% This endpoint is unregistered until a later point when it gets an identity assigned
  gen_server:start_link(?MODULE, [CallbackModule, CallbackData, Creds, XaptumHost, XaptumPort], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([XaptumHost, XaptumPort, CallbackModule, CallbackData, Creds]) ->
  gen_server:cast(self(), {auth, Creds}),
  {ok, #state{
    callback_module = CallbackModule, callback_data = CallbackData,
    xaptum_host = XaptumHost, xaptum_port = XaptumPort}}.

handle_call(get_data, _From, #state{callback_data = CallbackData} = State) ->
  {reply, CallbackData, State}.

handle_cast({auth, Creds}, #state{
  callback_module = CallbackModule, callback_data = CallbackData0,
  xaptum_host = XaptumHost, xaptum_port = XaptumPort} = State) ->
  %% TODO currently xtt is the only way, we'll have to add a level of abstraction later
  {#xtt_creds{identity = Identity,
    pseudonym = Pseudonym,
    cert = Cert,
    key = Key}, CallbackData1} = CallbackModule:auth(XaptumHost, XaptumPort, Creds, CallbackData0),
  gen_server:cast(self(), connect, State),
  register(binary_to_atom(Identity, utf8), self()),
  {noreply, State#state{ipv6 = Identity, pseudonym = Pseudonym, cert = Cert, key = Key, callback_data = CallbackData1}};

handle_cast(connect, #state{
  tls_socket = MaybeExistingTlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0,
  xaptum_host = XaptumHost, xaptum_port = XaptumPort, cert = Cert, key = Key} = State)
    when is_binary(Cert), is_binary(Key)->
  {ok, TlsSocket} = erltls:connect(XaptumHost, XaptumPort,
    [binary,
      {active, false}, %% TODO make configurable
      {reuseaddr, true},
      {packet, 0},
      {keepalive, true},
      {nodelay, true},
      {verify, verify_none},
      {fail_if_no_peer_cert, false},
      {cert, Cert},
      {key, Key}
    ],2000),
  case MaybeExistingTlsSocket of
    undefined ->
      {ok, CallbackData1} = CallbackModule:on_connect(CallbackData0);
    #tlssocket{} ->
      {ok, CallbackData1} = CallbackModule:on_reconnect(CallbackData0)
  end,

  {noreply, State#state{tls_socket = TlsSocket, callback_data = CallbackData1}};

handle_cast({send_message, {Payload, Dest}}, #state{
  tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
  {Message, CallbackData1} = CallbackModule:on_send(Payload, Dest, self(), CallbackData0),
  erltls:send(TlsSocket, Message),
  {noreply, State#state{callback_data = CallbackData1}};

handle_cast({send_message, Payload}, #state{
  tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
  {Message, CallbackData1} = CallbackModule:on_send(Payload, self(), CallbackData0),
  erltls:send(TlsSocket, Message),
  {noreply, State#state{callback_data = CallbackData1}};
handle_cast({send_request, Request}, #state{
  tls_socket = #tlssocket{} = TlsSocket} = State)->
  erltls:send(TlsSocket, Request),
  {noreply, State};
%% Handle {active, false} mode
handle_cast(start_receiving, #state{
    tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
    callback_module = CallbackModule, callback_data = CallbackData} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
  Parent = self(),
  ReceiverPid = spawn(CallbackModule, receive_loop, [TlsSocket, Parent, CallbackData]),
  {noreply, State#state{receiver_pid = ReceiverPid, callback_data = CallbackData}};

handle_cast({ssl_error, _Error}, #state{tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
    erltls:close(TlsSocket),
    gen_server:cast(self(), connect),
  {noreply, State};

handle_cast({set_data, NewData}, State)->
  {noreply, State#state{callback_data = NewData}}.

%% Handle {active, [once|N]} mode
handle_info({ssl, TlsSocket, Msg}, #state{tls_socket = TlsSocket, callback_module = CallbackModule, callback_data = CallbackData0} = State) ->
  {ok, CallbackData1} = CallbackModule:on_receive(Msg, self(), CallbackData0),
  {noreply, State#state{callback_data = CallbackData1}};
handle_info({ssl_error, TlsSocket, _Error}, #state{tls_socket = TlsSocket} = State)->
  erltls:close(TlsSocket),
  gen_server:cast(self(), connect),
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
