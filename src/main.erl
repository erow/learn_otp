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
  chat_server:start(),
  {ok,U1}=chat_server_center:register("erow"),
  {ok,U2}=chat_server_center:register("erow2"),
  user_interface:send_msg(U1,"erow2","hello"),
  user_interface:send_msg(U1,"erow2","world!"),
  user_interface:send_msg(U2,"erow","hello world!"),
  io:format("~p~n",[user_interface:fetch_msg(U2)]),
  observer:start().
