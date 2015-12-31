-module(maxwell_backend_app).

-behaviour(application).

%% callbacks
-export([start/2,
         stop/1]).

%% =========================================================
%% API implementations
%% =========================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    ok = application:start(ranch),
    ok = application:start(gproc),
    maxwell_backend_db = ets:new(maxwell_backend_db, [set, named_table]),
    maxwell_backend_event_notify = ets:new(maxwell_backend_event_notify, [bag, named_table]),
    maxwell_backend_agents = ets:new(maxwell_backend_agents, [set, named_table]),
    {ok, EventStoreProps} = application:get_env(maxwell_backend, event_store),
    EventStorePath = proplists:get_value(path, EventStoreProps),
    {ok, DbRef} = eleveldb:open(EventStorePath, [{create_if_missing, true}, {total_leveldb_mem_percent, 70}]),
    true = ets:insert_new(maxwell_backend_db, {event_store, DbRef}),
    {ok, _} = msgpack_rpc_server:start(testlistener, tcp, maxwell_backend_receiver, [{port, 9199}, {timeout, 1000 * 60 * 10}]),
    {ok, _} = ranch:start_listener(command, 1, ranch_tcp, [{port, 2016}], command_protocol, []),
    maxwell_backend_sup:start_link().

stop(_State) ->
    ok.

%% =========================================================
%% Internal functions
%% =========================================================

