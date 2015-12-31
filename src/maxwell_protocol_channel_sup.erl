-module(maxwell_protocol_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    PoolsSpec = ?CHILD(maxwell_protocol_channel_client_pools,supervisor),
    ClientPoolManager = ?CHILD(maxwell_protocol_channel_client_pool_manager,worker),
    ManagerSpec = ?CHILD(maxwell_protocol_channel_handler_manager, worker),
    {ok, {{one_for_one, 5, 10}, [ManagerSpec,ClientPoolManager,PoolsSpec]}}.

