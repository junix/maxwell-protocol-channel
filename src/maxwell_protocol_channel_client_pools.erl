-module(maxwell_protocol_channel_client_pools).
-behaviour(supervisor).

-export([call/2]).
-export([cast/2]).
-export([start_link/0]).
-export([stop/1]).
-export([init/1]).

%%% ==========================
%%% API
%%% ==========================
call(PoolName, Command) ->
  poolboy:transaction(PoolName,
    fun(Worker) ->
      gen_server:call(Worker, {call, Command},1000*60)
    end).

cast(PoolName, Command) ->
  poolboy:transaction(PoolName,
    fun(Worker) ->
      gen_server:cast(Worker, {cast, Command})
    end).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ok.

%%% ==========================
%%% callbacks
%%% ==========================
init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.

