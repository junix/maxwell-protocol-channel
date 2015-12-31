-module(maxwell_protocol_channel).
-include("maxwell_protocol_channel_pb.hrl").

%% APIs
-export([
  start_server/1,
  connect/2,
  install_command_handler/2,
  delete_command_handler/1,
  call/2,
  cast/2
]).

%% =========================================================
%% API implementations
%% =========================================================

start_server(Port) ->
  ranch:start_listener(command_channel, 10, ranch_tcp,
    [{port, Port}],
    maxwell_protocol_channel_command_protocol, []).

connect(ChanName,Port) ->
  maxwell_protocol_channel_client_pool_manager:start_pool(ChanName,Port).

install_command_handler(Type, Handler) when is_integer(Type) ->
  maxwell_protocol_channel_handler_manager:install_handler(Type, Handler).

delete_command_handler(Type) when is_integer(Type) ->
  maxwell_protocol_channel_handler_manager:delete_handler(Type).

call(ChanName, {Type, Command}) ->
  ChanMessage = {chan_msg_t, 'CALL', Type, Command},
  BinMessage = maxwell_protocol_channel_pb:encode(ChanMessage),
  maxwell_protocol_channel_client_pools:call(ChanName, BinMessage).

cast(ChanName, {Type, Command}) ->
  ChanMessage = {chan_msg_t, 'CALL', Type, Command},
  BinMessage = maxwell_protocol_channel_pb:encode(ChanMessage),
  maxwell_protocol_channel_client_pools:cast(ChanName, BinMessage).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.


