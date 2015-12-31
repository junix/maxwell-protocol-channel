-module(maxwell_protocol_channel_command_protocol).%{{{
-behaviour(gen_server).
%%-behaviour(ranch_protocol).

-include_lib("maxwell_protocol_channel_pb.hrl").

%% API.
-export([start_link/5]).
-export([init/5]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(TIMEOUT, 1000 *  60).
-define(HEARTBEAT, <<1:8>>).
-define(RESPONSE,  <<2:8>>).

-record(state, {
  socket,
  transport,
  mod,
  peer_ip,
  last_updated_time
}).%}}}

-callback start_link(Ref::ranch:ref(),
    Socket::any(),
    Transport::module(),
    ProtocolOptions::any()) -> {ok, ConnectionPid::pid()}.
-callback proc_command(Command::any()) ->
  noreply | {reply,Reply::binary()} | {error, Reason::atom()}.


%%--------------------------------------------------------------------
%%    API.
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts, Mod) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts, Mod]).

%%--------------------------------------------------------------------
%% gen_fsm.
%%--------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, Opts, Mod) ->
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, once}]),
  {ok,{PeerIP,_Port}} = Transport:peername(Socket),
  State = #state{
    socket = Socket,
    transport = Transport,
    mod = Mod,
    peer_ip = maxwell_protocol_channel_lib:peer_chan_name(PeerIP),
    last_updated_time = cloud_common_lib:current()},
  gen_server:enter_loop(?MODULE, [], State, ?TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State, ?TIMEOUT}.

handle_cast(_Msg, State) ->
  {noreply, State, ?TIMEOUT}.

handle_info({tcp_closed, _Port}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
  {stop, Reason, State};
handle_info({tcp, Socket, Data}, State = #state{
  socket = Socket,
  mod = Mod,
  transport = Transport}) ->
  Transport:setopts(Socket, [{active, once}]),
  case Mod:proc_command(Data) of
    noreply ->
      {noreply, State, ?TIMEOUT};
    {reply, BinRep} when is_binary(BinRep) ->
      Len = byte_size(BinRep),
      ok = Transport:send(Socket, [?RESPONSE, <<Len:32>> | BinRep]),
      {noreply, State, ?TIMEOUT};
    {error, Reason} ->
      {stop, Reason, State}
  end;
handle_info(timeout,
    State = #state{
  socket = Socket,
  transport = Transport
}) ->
  ok = Transport:send(Socket, ?HEARTBEAT),
  {noreply, State, ?TIMEOUT};
handle_info(Info, State) ->
  lager:info("recv ~p", [Info]),
  {noreply, State, ?TIMEOUT}.

terminate(Reason, _State) ->
  lager:info("term ~p,reason:~p", [self(), Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
