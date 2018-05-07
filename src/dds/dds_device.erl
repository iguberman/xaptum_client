%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2018 4:14 PM
%%%-------------------------------------------------------------------
-module(dds_device).
-author("iguberman").

-behavior(xaptum_endpoint).

%% API
-export([]).


auth(XttServerHost, XttServerPort, Creds, _CallbackData)->
  xtt_endpoint:auth(XttServerHost, XttServerPort, Creds, _CallbackData).

%% DDS DEVICE sends reg messages to subscriber Queue
on_message(Msg, Dest, SessionToken)->
  ddslib:build_reg_message(SessionToken, Msg)


