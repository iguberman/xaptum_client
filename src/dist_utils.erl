%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 14. Jun 2018 3:34 PM
%%%-------------------------------------------------------------------
-module(dist_utils).
-author("iguberman").

%% API
-export([
  xaptum_host/0,
  xaptum_host/1]).

xaptum_host()->
  {ok, XaptumCluster} = application:get_env(xaptum_client, xaptum_cluster),
  xaptum_host(XaptumCluster).

xaptum_host(XaptumCluster)->
  Host = lists:nth(rand:uniform(length(XaptumCluster)), XaptumCluster),
  lager:debug("Chose host ~p from ~p", [Host, XaptumCluster]),
  Host.


