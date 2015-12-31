-module(maxwell_protocol_channel_bert).
-export([install/0]).
-export([cast/2]).
-export([do_proc/2]).
-export([call/2]).

install() ->
  maxwell_protocol_channel:install_command_handler(0,
    fun(Type, MFA) ->
      maxwell_protocol_channel_bert:do_proc(Type, MFA)
    end).

cast(ChanName, MFA) ->
  PayLoad = erlang:term_to_binary(MFA),
  maxwell_protocol_channel:cast(ChanName, {0, PayLoad}).

call(ChanName, MFA) ->
  PayLoad = erlang:term_to_binary(MFA),
  case maxwell_protocol_channel:call(ChanName, {0, PayLoad}) of
    {ok, BinRep} ->
      binary_to_term(BinRep);
    {error, _} = Error ->
      Error
  end.

do_proc(call, Payload) when is_binary(Payload) ->
  {M, F, A} = erlang:binary_to_term(Payload),
  case catch erlang:apply(M, F, A) of
    Result ->
      term_to_binary(Result)
  end;
do_proc(cast, Payload) when is_binary(Payload) ->
  {M, F, A} = erlang:binary_to_term(Payload),
  erlang:apply(M, F, A).
  


