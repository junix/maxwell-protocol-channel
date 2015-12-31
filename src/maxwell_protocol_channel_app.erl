-module(maxwell_protocol_channel_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(HANDLERS,maxwell_command_chan_handler_tab).
-define(CLIENT_POOLS,maxwell_protocol_channel_client_pools).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
   ?HANDLERS = ets:new(?HANDLERS,[set,public,named_table,{read_concurrency,true}]),
   ?CLIENT_POOLS = ets:new(?CLIENT_POOLS, [set, public, named_table,{read_concurrency,true}]),
   ok = lager:start(),
   ok = application:ensure_started(ranch),
   maxwell_protocol_channel_sup:start_link().

stop(_State) ->
    ok.
