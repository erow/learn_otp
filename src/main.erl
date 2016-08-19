%%%-------------------------------------------------------------------
%%% @author PC
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 七月 2016 10:22
%%%-------------------------------------------------------------------
-module(main).
-author("PC").

%% API
-export([main/1]).

main(_Args)->
  chat_server:start_link(),
  chat_server:send(erow,"hi"),
  chat_server:send(nick,"hello!"),
  io:format("history:~p~n",[chat_server:show_history()]),
  io:format("recv:~p~n",[chat_server:recv()]),
  io:format("history:~p~n",[chat_server:show_history()]),
  io:format("recv:~p~n",[chat_server:recv()]),
  io:format
  chat_server ! "no use message!",
  chat_server:stop().
