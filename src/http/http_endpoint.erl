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

-define(SUPERCLASS, xtt_endpoint).

-include("xtt_endpoint.hrl").

%% API
-export([]).

%% xaptum_endpoint callbacks
-export([
  auth/4,
  on_receive/3,
  receive_loop/3,
  on_send/2,
  on_send/3,
  on_connect/2,
  on_reconnect/2,
  on_disconnect/2
]).


auth(XttServerHost, XttServerPort, Creds, CallbackData)->
  ?SUPERCLASS:auth(XttServerHost, XttServerPort, Creds, CallbackData).

%% TODOs
on_receive(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

receive_loop(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

on_send(Msg, Dest, CallbackData) ->
  ?SUPERCLASS:on_send(Msg, Dest, CallbackData).

on_send(Msg, CallbackData) ->
  ?SUPERCLASS:on_send(Msg, CallbackData).

on_connect(EndpointPid, CallbackData) ->
  ?SUPERCLASS:on_connect(EndpointPid, CallbackData).

on_reconnect(EndpointPid, CallbackData) ->
  ?SUPERCLASS:on_reconnect(EndpointPid, CallbackData).

on_disconnect(EndpointPid, CallbackData) ->
  ?SUPERCLASS:on_disconnect(EndpointPid, CallbackData).

