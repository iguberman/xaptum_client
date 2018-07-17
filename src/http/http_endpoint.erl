%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2018 12:29 PM
%%%-------------------------------------------------------------------
-module(http_endpoint).
-author("iguberman").

-behavior(xaptum_endpoint).

-include("xtt_endpoint.hrl").

%% API
-export([]).

%% xaptum_endpoint callbacks
-export([
  auth/4,
  on_receive/3,
  do_receive/1,
  on_send/2,
  on_send/3,
  on_connect/2,
  on_reconnect/2,
  on_disconnect/2
]).


auth(XttServerHost, XttServerPort, Creds, CallbackData)->
  xtt_endpoint:auth(XttServerHost, XttServerPort, Creds, CallbackData).

%% TODOs
on_receive(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

do_receive(TlsSocket) ->
  xtt_endpoing:do_receive(TlsSocket).

on_send(Msg, Dest, CallbackData) ->
  xtt_endpoint:on_send(Msg, Dest, CallbackData).

on_send(Msg, CallbackData) ->
  xtt_endpoint:on_send(Msg, CallbackData).

on_connect(TlsSocket, CallbackData) ->
  xtt_endpoint:on_connect(TlsSocket, CallbackData).

on_reconnect(TlsSocket, CallbackData) ->
  xtt_endpoint:on_reconnect(TlsSocket, CallbackData).

on_disconnect(TlsSocket, CallbackData) ->
  xtt_endpoint:on_disconnect(TlsSocket, CallbackData).

