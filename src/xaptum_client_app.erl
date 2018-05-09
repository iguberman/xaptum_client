%%%-------------------------------------------------------------------
%% @doc xaptum_client public API
%% @end
%%%-------------------------------------------------------------------

-module(xaptum_client_app).


%% Application behaviour and callbacks
-behaviour(application).

-export([start/2,
         stop/1
]).

-export([
	 get_application/0,
	 get_env/2,
	 priv_dir/0
]).

%% supervisor behaviour and callbacks
-behaviour(supervisor).

%% Supervisor callbacks
-export([
	 start_link/0,
	 init/1
	]).

-define(SERVER, ?MODULE).

%%====================================================================
%% application behaviour implementation
%%====================================================================
start(_StartType, _StartArgs) ->
    lager:info("Starting xaptum_client application"),

    %% Start root supervisor
    ?MODULE:start_link().

stop(_State) ->
    lager:info("Application receive stop. State is ~p", [_State]),
    ok.

%%====================================================================
%% application helper API
%%====================================================================
get_application() ->
    application:get_application(?MODULE).

get_env(App, EnvVar) ->
    case application:get_env(App, EnvVar) of
	undefined -> no_default_env_value;
	V -> V
    end.

priv_dir() ->
    case code:priv_dir(xaptum_client) of
	{error, bad_name} ->
	    lager:info("Couldn't find priv dir for the application, using ./priv~n"), "./priv";
	PrivDir -> filename:absname(PrivDir)
    end.

%%====================================================================
%% supervisor behaviour implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    %% Restart Strategy
    RestartStrategy = {one_for_one, 40, 3600},

    %% Create device/subscriber child spec
    {ok, App} = get_application(),
    {ok, Type} = get_env(App, type),
    Child = bacnet_child_spec(Type),

  {ok, XttHost} = application:get_env(xtt_host),
  {ok, XttPort} = application:get_env(xtt_port),

    EndpointSupervisorSpec =
      #{id => xaptum_endpoint_sup,
      start => {xaptum_endpoint_sup, start_link, [XttHost, XttPort]},
      shutdown => 10000,
      type => supervisor},

    %% Create elli child spec
    {ok, ElliPort} = get_env(App, stat_port), 
    ElliOpts = [{callback, xaptum_client_http}, {port, ElliPort}],
    Elli = {
        xaptum_client_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    {ok, {RestartStrategy, [EndpointSupervisorSpec, Child, Elli]}}.


%% dds child spec
child_spec(xaptum_device) ->
    {xaptum_device, {xaptum_client, start_device, []}, permanent, 2000, worker, [xaptum_client]};
child_spec(xaptum_subscriber) ->
    {xaptum_subscriber, {xaptum_client, start_subscriber, []}, permanent, 2000, worker, [xaptum_client]}.

%% Bacnet child spec
bacnet_child_spec(xaptum_device) ->
    {xaptum_device, {bacnet_proxy, start_proxy, []}, permanent, 2000, worker, [xaptum_client]};
bacnet_child_spec(xaptum_subscriber) ->
    {xaptum_subscriber, {bacnet_control, start_control, []}, permanent, 2000, worker, [xaptum_client]}.
