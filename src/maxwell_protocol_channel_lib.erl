-module(maxwell_protocol_channel_lib).
-export([peer_chan_name/1]).
-export([split_name_and_ip/2]).

peer_chan_name({A, B, C, D}) ->
  IoList = io_lib:format("command_channel@~p.~p.~p.~p", [A, B, C, D]),
  Bin = iolist_to_binary(IoList),
  binary_to_atom(Bin, utf8).


split_name_and_ip([$@ | IP], Name) ->
  {lists:reverse(Name), IP};
split_name_and_ip([H | T], Name) ->
  split_name_and_ip(T, [H | Name]).
