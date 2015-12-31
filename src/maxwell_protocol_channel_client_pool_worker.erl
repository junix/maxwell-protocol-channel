-module(maxwell_protocol_channel_client_pool_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([call/2]).
-export([cast/2]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SCHED_TIMEOUT, 1000 * 30).
-define(CALL_TIMEOUT_STEP, 1000 * 5).
-define(HEARTBEAT, 1:8).
-define(RESPONSE, 2:8).
-define(RETRY_FAILED, {error, retry_failed}).

-record(state, {
  hostname = undefined,
  port = undefined,
  conn = undefined
}).

%%% ===================================
%%% API
%%% ===================================
start_link(Args) ->
  lager:info("args = ~p", [Args]),
  gen_server:start_link(?MODULE, Args, []).

call(Pid, Command) ->
  gen_server:call(Pid, {call, Command}, 1000 * 15).
cast(Pid, Command) ->
  gen_server:cast(Pid, {cast, Command}).

%%% ===================================
%%% callbacks
%%% ===================================
init(Args) ->
  lager:info("init args ~p", [Args]),
  process_flag(trap_exit, true),
  Host = proplists:get_value(hostname, Args),
  Port = proplists:get_value(port, Args),
  State = #state{hostname = Host, port = Port},
  {ok, NewState} = re_connect(State),
  {ok, NewState, ?SCHED_TIMEOUT}.

handle_call(Request, From, State = #state{conn = undefined}) ->
  case re_connect(State) of
    {ok, NewState} ->
      handle_call(Request, From, NewState);
    {{error, _} = Error, NewState} ->
      {reply, Error, NewState, ?SCHED_TIMEOUT}
  end;
handle_call({call, Command}, _From, State) ->
  {Response, NewState} = do_call(Command, State, 1),
  {reply, Response, NewState, ?SCHED_TIMEOUT};
handle_call(_Request, _From, State) ->
  {reply, ok, State, ?SCHED_TIMEOUT}.

handle_cast(Request, State = #state{conn = undefined}) ->
  case re_connect(State) of
    {ok, NewState} ->
      handle_cast(Request, NewState);
    {{error, _} = Error, NewState} ->
      lager:error("Fail to proc:~p:~p", [Request, Error]),
      {noreply, NewState, ?SCHED_TIMEOUT}
  end;
handle_cast({cast, Command}, State) ->
  do_cast(Command, State),
  {noreply, State, ?SCHED_TIMEOUT};
handle_cast(_Msg, State) ->
  {noreply, State, ?SCHED_TIMEOUT}.

handle_info({tcp_closed, Socket}, State = #state{conn = Socket}) ->
  {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State = #state{conn = Socket}) ->
  {stop, Reason, State};
handle_info(timeout, State) ->
  NewState = close_conn(State),
  {noreply, NewState, ?SCHED_TIMEOUT};
handle_info(Info, State) ->
  lager:info("recv ~p", [Info]),
  {noreply, State, ?SCHED_TIMEOUT}.

terminate(Reason, #state{}) ->
  lager:info("client close ~p:~p", [self(), Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% ===================================
%%% internal functions
%%% ===================================
close_conn(#state{conn = undefined} = State) ->
  State;
close_conn(#state{conn = Conn} = State) ->
  lager:info("close socket ~p",[Conn]),
  ok = gen_tcp:close(Conn),
  State#state{conn = undefined}.

do_call(_Command, State, RetryCount) when RetryCount > 3 ->
  {?RETRY_FAILED, State};
do_call(Command, State = #state{conn = undefined}, RetryCount) ->
  case re_connect(State) of
    {ok, NewState} ->
      do_call(Command, NewState, RetryCount);
    {Error, State} ->
      {Error, State}
  end;
do_call(Command, State = #state{conn = Conn}, RetryCount) ->
  ok = inet:setopts(Conn, [{active, false}]),
  case do_one_call(Command, Conn, RetryCount * ?CALL_TIMEOUT_STEP, RetryCount, State) of
    {{ok, _Rep}, _NewState} = OkState ->
      OkState;
    {{error, _}, _NewState} = ErrorState ->
      ErrorState;
    {{retry, NewRetryCount}, NewState} ->
      do_call(Command, close_conn(NewState), NewRetryCount)
  end.

do_one_call(Command, Conn, TimeOut, RetryCount, State) ->
  NewState = flush(Conn, State),
  case gen_tcp:send(Conn, Command) of
    ok ->
      case recv_util_rep(Conn, TimeOut, RetryCount) of
        {ok, Rep} ->
          {{ok, Rep}, NewState};
        {retry, _NewRetryCount} = Retry ->
          {Retry, NewState};
        {error, Reason} ->
          {{error, Reason}, NewState}
      end;
    {error, _Reason} ->
      {{retry, RetryCount}, NewState}
  end.

recv_util_rep(_Conn, _Timeout, RetryCount) when RetryCount > 3 ->
  ?RETRY_FAILED;
recv_util_rep(Conn, Timeout, RetryCount) ->
  case gen_tcp:recv(Conn, 0, Timeout) of
    {ok, Bin} ->
      lager:info("xxxxxxxxxxxxxxxxxxxxxxx~p", [Bin]),
      case parse_response(Bin, Conn, RetryCount) of
        {ok, Rep} ->
          ok = inet:setopts(Conn, [{active, true}]),
          {ok, Rep};
        continue ->
          recv_util_rep(Conn, Timeout, RetryCount + 1);
        {retry, _} = Retry ->
          Retry;
        {error, Reason} ->
          lager:error("Fail to recv,reason:~p", [Reason]),
          ?RETRY_FAILED
      end;
    {error, Reason} ->
      lager:error("Fail to recv,reason:~p", [Reason]),
      {retry, RetryCount}
  end.

parse_response(<<>>, _Socket, RetryCount) when RetryCount > 3 ->
  ?RETRY_FAILED;
parse_response(<<>>, _Socket, _RetryCount) ->
  continue;
parse_response(<<?HEARTBEAT, Rest/binary>>, Socket, RetryCount) ->
  parse_response(Rest, Socket, RetryCount);
parse_response(<<?RESPONSE, Len:32, Rest/binary>>, Socket, RetryCount) ->
  case read_len(Socket, Len - byte_size(Rest), RetryCount, [Rest]) of
    {ok, Rep} ->
      {ok, Rep};
    {retry, _} = Retry ->
      Retry;
    {error, _} = Error ->
      Error
  end;
parse_response(_Bin, _Socket, _RetryCount) ->
  {error, bad_format}.

read_len(_Socket, Left, _RetryCount, [Bin]) when Left =< 0 ->
  {ok, Bin};
read_len(_Socket, Left, _RetryCount, Sofar) when Left =< 0 ->
  {ok, iolist_to_binary(lists:reverse(Sofar))};
read_len(_Socket, _Left, RetryCount, _Sofar) when RetryCount > 3 ->
  ?RETRY_FAILED;
read_len(Socket, Left, RetryCount, Sofar) ->
  case gen_tcp:recv(Socket, Left, ?CALL_TIMEOUT_STEP * RetryCount) of
    {ok, Data} ->
      read_len(Socket, Left, RetryCount + 1, [Data | Sofar]);
    {error, timeout} ->
      read_len(Socket, Left, RetryCount + 1, Sofar);
    {error, _} ->
      {retry, RetryCount + 1}
  end.

do_cast(Command, #state{conn = Conn}) ->
  ok = gen_tcp:send(Conn, Command).

re_connect(State = #state{hostname = Host, port = Port, conn = undefined}) ->
  case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {keepalive, true}]) of
    {ok, Conn} ->
      {ok, State#state{conn = Conn}};
    {error, _} = Error ->
      {Error, State}
  end;
re_connect(State = #state{conn = Conn}) ->
  ok = gen_tcp:close(Conn),
  re_connect(State#state{conn = undefined}).

flush(Conn, State) ->
  case gen_tcp:recv(Conn, 0, 0) of
    {error, timeout} ->
      State;
    {ok, _} ->
      State
  end.


