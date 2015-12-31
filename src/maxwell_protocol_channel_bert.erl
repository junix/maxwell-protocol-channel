-module(maxwell_protocol_channel_bert).
-export([install/0]).
-export([call/2]).
-export([cast/2]).

install() ->
  maxwell_protocol_channel:install_command_handler(0,
    fun(Type, MFA, From) ->
      command_handler(Type, MFA, From)
    end).

cast(ChanName, MFA) ->
  PayLoad = term_to_binary(MFA),
  maxwell_protocol_channel:cast(ChanName, {0, PayLoad}).

call(ChanName, MFA) ->
  PayLoad = term_to_binary(MFA),
  case maxwell_protocol_channel:call(ChanName, {0, PayLoad}) of
    {ok, BinRep} ->
      binary_to_term(BinRep);
    {error, _} = Error ->
      Error
  end.

command_handler(call, Payload, _From) when is_binary(Payload) ->
  {M, F, A} = binary_to_term(Payload),
  case catch apply(M, F, A) of
    Result ->
      term_to_binary(Result)
  end;
command_handler(cast, Payload, _From) when is_binary(Payload) ->
  {M, F, A} = binary_to_term(Payload),
  apply(M, F, A).
  


