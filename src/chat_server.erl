%%%-------------------------------------------------------------------
%%% @author PC
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 八月 2016 11:28
%%%-------------------------------------------------------------------
-module(chat_server).
-author("PC").

%% API
-export([start/0]).

start()->
  application:start(chat_server).

querry_user(UserName)->
  ok.

regiser(UserName)->
  ok.

send_message(Pid,Msg)->
  ok.
