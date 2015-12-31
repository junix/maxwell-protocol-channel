-module(maxwell_protocol_channel_client_pools).
-behaviour(supervisor).

-export([call/2]).
-export([cast/2]).
-export([start_link/0]).
-export([stop/1]).
-export([init/1]).

-define(ETS_TABLE,?MODULE).


%%% ==========================
%%% API
%%% ==========================
call(PoolName, Command) ->
  ok = ensure_pool_started(PoolName),
  poolboy:transaction(PoolName,
    fun(Worker) ->
      gen_server:call(Worker, {call, Command},1000*60)
    end).

cast(PoolName, Command) ->
  ok = ensure_pool_started(PoolName),
  poolboy:transaction(PoolName,
    fun(Worker) ->
      gen_server:cast(Worker, {cast, Command})
    end).

start_link() ->
  ?ETS_TABLE = ets:new(?ETS_TABLE, [set, public, named_table]),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ok.

%%% ==========================
%%% callbacks
%%% ==========================
init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.

%%% ==========================
%%% internal functions
%%% ==========================
pool_spec(Name) when is_atom(Name) ->
  PoolArgs = [
    {name,          {local, Name}},
    {worker_module, maxwell_protocol_channel_client_pool_worker},
    {size,          10},
    {max_overflow,  10}
  ],
  poolboy:child_spec(Name, PoolArgs,worker_args(Name)).

worker_args(FullName) when is_atom(FullName) ->
  NameStr = atom_to_list(FullName),
  {Name, IP} = maxwell_protocol_channel_lib:split_name_and_ip(NameStr, []),
  {ok, PoolEnvs} = application:get_env(maxwell_backend, list_to_atom(Name)),
  Port = proplists:get_value(port, PoolEnvs),
  [{hostname, IP}, {port, Port}].

ensure_pool_started(PoolName) ->
  case ets:lookup(?ETS_TABLE, PoolName) of
    [] ->
      Spec = pool_spec(PoolName),
      case supervisor:start_child(?MODULE, Spec) of
        {ok, Child} ->
          true = ets:insert(?ETS_TABLE, {PoolName, Child}),
          ok;
        {error, already_present} ->
          ok;
        {error, {already_started, _Child}} ->
          ok;
        {error, _} = Error ->
          Error
      end;
    _ ->
      ok
  end.


