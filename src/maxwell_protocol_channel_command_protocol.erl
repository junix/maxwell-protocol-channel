-module(maxwell_protocol_channel_command_protocol).%{{{
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include_lib("maxwell_protocol_channel_pb.hrl").

%% API.
-export([start_link/4]).
-export([init/4]).

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


%%--------------------------------------------------------------------
%%    API.
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%%--------------------------------------------------------------------
%% gen_fsm.
%%--------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts) ->
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, once}]),
  {ok,{PeerIP,_Port}} = Transport:peername(Socket),
  State = #state{
    socket = Socket,
    transport = Transport,
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
  transport = Transport}) ->
  Transport:setopts(Socket, [{active, once}]),
  case proc_msg(Data) of
    noreply ->
      {noreply, State, ?TIMEOUT};
    {reply, {ok,BinRep}} when is_binary(BinRep) ->
      OkReply = maxwell_protocol_channel_pb:encode({chan_msg_t,'REPLY',0,BinRep}),
      Len = iolist_size(OkReply),
      ok = Transport:send(Socket, [?RESPONSE, <<Len:32>> | OkReply]),
      {noreply, State, ?TIMEOUT};
    {reply, {error,Reason}} ->
      ErrReply = maxwell_protocol_channel_pb:encode({chan_msg_t,'REPLY',1,term_to_binary(Reason)}),
      Len = iolist_size(ErrReply),
      ok = Transport:send(Socket, [?RESPONSE, <<Len:32>> | ErrReply]),
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
  {noreply, State, ?TIMEOUT}.

terminate(Reason, _State) ->
  lager:info("term ~p,reason:~p", [self(), Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

proc_msg(Data) when is_binary(Data) ->
  proc_msg(maxwell_protocol_channel_pb:decode(chan_msg_t, Data));
proc_msg(#chan_msg_t{rpc_type = 'CAST',msg_type = Type, payload = PayLoad}) ->
  case maxwell_protocol_channel_handler_manager:find_handler(Type) of
    {ok,Handler} ->
      case catch(Handler(cast,PayLoad)) of
        {'EXIT',Reason} ->
          lager:error("Fail to proc ~p:~p,reason:~p",[Type,PayLoad,Reason]);
        _ ->
          ok
      end;
    {error,Reason} ->
      lager:error("Fail to proc ~p:~p,reason:~p",[Type,PayLoad,Reason])
  end,
  noreply;
proc_msg(#chan_msg_t{rpc_type = 'CALL',msg_type = Type, payload = PayLoad}) ->
  Reply = case maxwell_protocol_channel_handler_manager:find_handler(Type) of
    {ok,Handler} ->
      case catch(Handler(call,PayLoad)) of
        {'EXIT',Reason} ->
          {error,Reason};
        Res when is_binary(Res) ->
          {ok,Res}
      end;
    {error,_Reason} = E ->
      E
  end,
  {reply,Reply}.

