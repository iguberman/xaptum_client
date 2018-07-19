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
  hosts_config, xtt_port, tls_port,
  ipv6, pseudonym, cert, key, tls_socket,
  callback_data, callback_module}).


-callback auth(HostsConfig :: tuple(), Inputs :: term(), CallbackData :: any())->
  {Identity :: binary() , Cert :: binary(), Key :: binary()}.
-callback on_receive(Msg :: binary(), CallbackData :: any()) -> {ok, CallbackData :: any()}.
-callback on_send(Msg :: binary(), Dest :: any(), CallbackData :: any()) ->
  {ok, Msg :: binary(), CallbackData :: any()}.
-callback on_send(Msg :: binary(), CallbackData :: any()) ->
  {ok, Msg :: binary(), CallbackData :: any()}.
-callback on_disconnect(TlsSocket :: tuple(), CallbackData :: any()) -> {ok, CallbackData :: any}.
-callback on_connect(TlsSocket :: tuple(), CallbackData :: any()) -> {ok, CallbackData :: any()} | {error, Error :: any}.
-callback on_reconnect(TlsSocket :: tuple(), CallbackData :: any()) -> {ok, Callbackdata :: any()} | {error, Error :: any}.

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

get_data(EndpointPid)->
  gen_server:call(EndpointPid, get_data, 30000).

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
handle_cast(maybe_connect, #state{ tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = ExistingTlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State) when is_port(TcpSocket), is_pid(SslPid) ->
  case CallbackModule:on_connect(ExistingTlsSocket, CallbackData0) of
    {ok, CallbackData1} ->
      lager:info("on_connect success, resulting callback data ~p", [CallbackData1]),
      {noreply, State#state{callback_data = CallbackData1}};
    {error, Error} ->
      lager:error("Connect failure due to error ~p", [Error]),
      {stop, {error, Error}, State}
  end;

handle_cast(maybe_connect, #state{ tls_socket = undefined, tls_port = TlsPort, cert = Cert, key = Key,
  callback_module = CallbackModule, callback_data = CallbackData0} = State0)->
  case do_tls_connect(TlsPort, Cert, Key) of
    {ok, #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket} when is_pid(SslPid), is_port(TcpSocket)->
      {ok, CallbackData1} = CallbackModule:on_connect(TlsSocket, CallbackData0),
      lager:info("on_connect success, resulting callback data ~p", [CallbackData1]),
      {noreply, State0#state{tls_socket = TlsSocket, callback_data = CallbackData1}};
    Other ->
      lager:error("Couldn't tls connect, result: ~p", Other),
      {stop, {error, tls_connect_error}, State0}
  end;


%% Connect if not connected, force reconnect if it is
handle_cast(maybe_reconnect, #state{
  tls_socket = MaybeExistingTlsSocket, tls_port = TlsPort, cert = Cert, key = Key,
  callback_module = CallbackModule, callback_data = CallbackData0} = State) ->
  {ok, TlsSocket} = do_tls_connect(TlsPort, Cert, Key),
  case MaybeExistingTlsSocket of
    undefined -> %% this WAS NOT a REconnect
      lager:info("CONNECTED to ~p", [TlsSocket]),
      {ok, CallbackData1} = CallbackModule:on_connect(TlsSocket, CallbackData0);
    #tlssocket{} = NewTlsSocket -> %% yes, this WAS a REconnect
      erltls:close(MaybeExistingTlsSocket),
      lager:info("RECONNECTING... closed ~p and opened ~p", [MaybeExistingTlsSocket, NewTlsSocket]),
      {ok, CallbackData1} = CallbackModule:on_reconnect(NewTlsSocket, CallbackData0)
  end,
  lager:info("MAYBE reconnect successful!"),
  {noreply, State#state{tls_socket = TlsSocket, callback_data = CallbackData1}};

handle_cast({send_message, Payload, Dest}, #state{
  tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0} = State)
  when is_port(TcpSocket), is_pid(SslPid) ->
  case CallbackModule:on_send(Payload, Dest, CallbackData0) of
    {error, retry_later} -> {noreply, CallbackData0};
    {ok, Message, CallbackData1} ->
      ok = erltls:send(TlsSocket, Message),
      lager:info("Sent message ~p to destination ~p", [Message, Dest]),
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
      lager:info("Published message ~p", [Message]),
      {noreply, State#state{callback_data = CallbackData1}}
  end;
handle_cast({send_message, Payload}, State)->
  lager:error("Send message ~p during invalid state ~p", [Payload, State]),
  {stop, invalid_state, State};
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

handle_cast({ssl_error, _Error}, #state{tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket} = State)
    when is_port(TcpSocket), is_pid(SslPid) ->
    erltls:close(TlsSocket),
    connect(self()),
  {noreply, State}.

handle_info({ssl, TlsSocket, Msg}, #state{tls_socket = TlsSocket, callback_module = CallbackModule, callback_data = CallbackData0} = State) ->
  {ok, CallbackData1} = CallbackModule:on_receive(Msg, CallbackData0),
  erltls:setopts(TlsSocket, [{active, once}]),
  {noreply, State#state{callback_data = CallbackData1}};
handle_info({ssl_error, TlsSocket, Error}, #state{tls_socket = TlsSocket} = State)->
  lager:warning("ssl_error ~p, closing TLS socket ~p and reconnecting", [Error, TlsSocket]),
  erltls:close(TlsSocket),
  connect(self()),
  {noreply, State};
handle_info(UnexpectedInfo, State)->
  lager:warning("UnexpectedInfo ~p", [UnexpectedInfo]),
  {noreply, State}.

terminate(_Reason, #state{tls_socket = undefined}) ->
  ok;
terminate(_Reason, #state{tls_socket = #tlssocket{tcp_sock = TcpSocket, ssl_pid = SslPid} = TlsSocket,
  callback_module = CallbackModule, callback_data = CallbackData0}) when is_port(TcpSocket), is_pid(SslPid) ->
  erltls:close(TlsSocket),
  {ok, _CallbackData1} = CallbackModule:on_disconnect(TlsSocket, CallbackData0),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_tls_connect(TlsPort, Cert, Key) when is_binary(Cert), is_binary(Key) ->
  XaptumHost = dist_utils:xaptum_host(),
  lager:debug("CONNECTING TO ~p:~b", [XaptumHost, TlsPort]),
  erltls:connect(XaptumHost, TlsPort,
    [binary,
      {active, once},
      {reuseaddr, true},
      {packet, 0},
      {keepalive, true},
      {nodelay, true},
      {verify, verify_none},
      {fail_if_no_peer_cert, false},
      {cert, Cert},
      {key, Key}
    ],2000).

%%receive_loop(TlsSocket, EndpointPid, CallbackModule) ->
%%  lager:debug("receive_loop(~p, ~p, ~p)", [TlsSocket, EndpointPid, CallbackModule]),
%%  case CallbackModule:do_receive(TlsSocket) of
%%    {ok, Msg} ->
%%      lager:info("Received ~p", [Msg]),
%%      xaptum_endpoint:received(Msg, EndpointPid),
%%      receive_loop(TlsSocket, EndpointPid, CallbackModule);
%%    {error, Error} ->
%%      xaptum_endpoint:ssl_error(EndpointPid, Error)
%%  end.