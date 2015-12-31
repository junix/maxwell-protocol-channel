-module(maxwell_protocol_channel_handler_manager).
-behaviour(gen_server).

%% API
-export([
  install_handler/2,
  find_handler/1,
  delete_handler/1,
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
-define(HANDLERS, maxwell_command_chan_handler_tab).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

install_handler(Type, Handler) when is_integer(Type) ->
  gen_server:call(?SERVER, {install, Type, Handler}).

delete_handler(Type) when is_integer(Type) ->
  gen_server:call(?SERVER, {delete, Type}).


find_handler(Type) ->
  case ets:lookup(?HANDLERS, Type) of
    [{Type, Handler}] ->
      {ok, Handler};
    [] ->
      {error, no_such_handler}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({install, Type, Handler}, _From, State) ->
  true = ets:insert(?HANDLERS, {Type, Handler}),
  {reply, ok, State};
handle_call({delete, Type}, _From, State) ->
  true = ets:delete(?HANDLERS, Type),
  {reply, ok, State};
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
