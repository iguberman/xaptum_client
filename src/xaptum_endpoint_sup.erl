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
-export([start_link/3]).

%% Supervisor callbacks
-export([
  init/1,
  num_children/0,
  create_endpoint/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(XaptumHost, XttPort, TlsPort) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [XaptumHost, XttPort, TlsPort]).

num_children()->
  supervisor:count_children(?MODULE).

create_endpoint(CallbackModule, CallbackData, Creds)->
  %% calls xaptum_endpoint:start_link(XttServer, XttPort, CallbackModule, CallbackData)
  supervisor:start_child(?MODULE, [CallbackModule, CallbackData, Creds]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([XaptumHost, XttPort, TlsPort]) ->
  RestartStrategy = {simple_one_for_one, 60, 3600},

  EndpointSpec =
    #{id => xaptum_endpoint,
      start => {xaptum_endpoint, start_link, [XaptumHost, XttPort, TlsPort]},
      restart => permanent,
      shutdown => 1000},

  lager:info("Starting xaptum_endpoint_sup on ~p:~p,~p", [XaptumHost, XttPort, TlsPort]),

  {ok, {RestartStrategy, [EndpointSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

