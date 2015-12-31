-module(maxwell_protocol_channel_client_pool_manager).
-behaviour(gen_server).

%% API
-export([
         start_pool/2,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE,maxwell_protocol_channel_client_pools).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_pool(PoolName,Port) ->
    gen_server:call(?SERVER, {start_pool, PoolName, Port}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({start_pool, PoolName, Port}, _From, State) ->
    Res = start_pool_aux(PoolName,Port),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% ==========================
%%% internal functions
%%% ==========================
pool_spec(Name,Port) when is_atom(Name) ->
  PoolArgs = [
    {name,          {local, Name}},
    {worker_module, maxwell_protocol_channel_client_pool_worker},
    {size,          10},
    {max_overflow,  10}
  ],
  poolboy:child_spec(Name, PoolArgs,worker_args(Name,Port)).

worker_args(FullName, Port) when is_atom(FullName) ->
  NameStr = atom_to_list(FullName),
  {_Name, IP} = maxwell_protocol_channel_lib:split_name_and_ip(NameStr, []),
  [{hostname, IP}, {port, Port}].

start_pool_aux(PoolName,Port) ->
  case ets:lookup(?ETS_TABLE, PoolName) of
    [] ->
      Spec = pool_spec(PoolName,Port),
      case supervisor:start_child(maxwell_protocol_channel_client_pools, Spec) of
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


