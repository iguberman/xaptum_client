%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2018 4:28 PM
%%%-------------------------------------------------------------------
-module(xaptum_endpoint_sup).
-author("iguberman").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([
  init/1,
  num_children/0,
  create_endpoint/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(HostsConfig) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [HostsConfig]).

num_children()->
  supervisor:count_children(?MODULE).

create_endpoint(CallbackModule, CallbackData, Inputs)->
  %% calls xaptum_endpoint:start_link(HostsConfig, CallbackModule, CallbackData, Inputs)
  supervisor:start_child(?MODULE, [CallbackModule, CallbackData, Inputs]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([HostsConfig]) ->
  RestartStrategy = {simple_one_for_one, 5, 1000},

  EndpointSpec =
    #{id => xaptum_endpoint,
      start => {xaptum_endpoint, start_link, [HostsConfig]},
      restart => temporary,
      shutdown => 1000},

  lager:info("Starting xaptum_endpoint_sup on ~p", [HostsConfig]),

  {ok, {RestartStrategy, [EndpointSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

