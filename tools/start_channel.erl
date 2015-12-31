%%%-------------------------------------------------------------------
%%% @author junix
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2015 17:26
%%%-------------------------------------------------------------------
-module(start_channel).
-author("junix").

%% API
-export([start/0]).

%% =========================================================
%% API implementations
%% =========================================================

start() ->
  application:start(maxwell_protocol_channel),
  maxwell_protocol_channel:start_server(2233),
  maxwell_protocol_channel:connect('x@127.0.0.1',2233),
  maxwell_protocol_channel_bert:install(),
  sync:go().
