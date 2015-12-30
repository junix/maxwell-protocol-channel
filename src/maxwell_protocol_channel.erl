-module(maxwell_protocol_channel).
-include("maxwell_protocol_channel_pb.hrl").

%% APIs
-export([
  pack/1,
  unpack/1,
  build_command_to_event_agent/3,
  build_command_to_frontend_agent/3,
  decode_dest/1,
  decode_src/1,
  decode_addr/2
]).

%% =========================================================
%% API implementations
%% =========================================================
pack(Record) ->
  maxwell_protocol_channel_pb:encode(Record).

build_command_to_event_agent(UserId, AgentKey, PayLoad) ->
  AgentAddr = maxwell_protocol_channel_pb:encode({chan_agent_id_t, UserId, AgentKey}),
  maxwell_protocol_channel_pb:encode({chan_command_t, 'PEER_TYPE_EVENT_AGENT', AgentAddr, PayLoad, undefined, undefined}).

build_command_to_frontend_agent(UserId, AgentKey, PayLoad) ->
  AgentAddr = maxwell_protocol_channel_pb:encode({chan_agent_id_t, UserId, AgentKey}),
  maxwell_protocol_channel_pb:encode({chan_command_t, 'PEER_TYPE_FRONTEND_AGENT', AgentAddr, PayLoad, undefined, undefined}).

unpack(Bin) ->
  maxwell_protocol_channel_pb:decode(chan_command_t, Bin).

decode_dest(#chan_command_t{dest_type = DestType, dest_id = DestAddr}) ->
  decode_addr(DestType, DestAddr).

decode_src(#chan_command_t{src_type = undefined, src_id = undefined}) ->
  {error, undefined};
decode_src(#chan_command_t{src_type = SrcType, src_id = SrcAddr}) ->
  decode_addr(SrcType, SrcAddr).

decode_addr('PEER_TYPE_EVENT_AGENT', DestAddr) ->
  {ok, maxwell_protocol_channel_pb:decode(chan_agent_id_t, DestAddr)};
decode_addr('PEER_TYPE_FRONTEND_AGENT', DestAddr) ->
  {ok, maxwell_protocol_channel_pb:decode(chan_agent_id_t, DestAddr)};
decode_addr(_Type, _DestAddr) ->
  {error, unknown_type}.

encode_addr('PEER_TYPE_EVENT_AGENT', {UserId, AgentKey}) ->
  maxwell_protocol_channel_pb:encode({chan_agent_id_t, UserId, AgentKey});
encode_addr('PEER_TYPE_FRONTEND_AGENT', {UserId, AgentKey}) ->
  maxwell_protocol_channel_pb:encode({chan_agent_id_t, UserId, AgentKey}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

x01_test() ->
  Bin = iolist_to_binary(build_command_to_event_agent(1, <<"agentkey">>, <<"content">>)),
  Ex = {chan_command_t, 'PEER_TYPE_EVENT_AGENT', iolist_to_binary(encode_addr('PEER_TYPE_EVENT_AGENT', {1, <<"agentkey">>})), <<"content">>, undefined, undefined},
  ?assertEqual(unpack(Bin), Ex).

x02_test() ->
  Bin = iolist_to_binary(build_command_to_event_agent(1, <<"agentkey">>, <<"content">>)),
  Cmd = unpack(Bin),
  {ok, D} = decode_dest(Cmd),
  ?assertEqual(D, {chan_agent_id_t, 1, <<"agentkey">>}).

-endif.


