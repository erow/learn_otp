%%%-------------------------------------------------------------------
%%% @author PC
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 七月 2016 10:04
%%%-------------------------------------------------------------------
-module(s_gen_server).
-author("PC").

%% API
-export([call/2, terminate/1, main/1]).
%% behaviour
-export([start_link/2, behaviour_info/1, handle/1, init/2]).
%%-spec init(Arg)->{ok,state}|{error,Reason}.
%%-spec handle_call(Request,From,State) -> {ok,new_state}|{error,Reason}.
%%-spec start_link(Module, Args)->Result.
%%Result={ok,Pid}.

behaviour_info(callbacks) ->
  [{init, 1},
    {handle_call, 3}];
behaviour_info(_Other) ->
  undefined.

handle(State) ->
  receive
    {Module, From, Request} ->
      case Module:handle_call(Request, From, State) of
        {_Type, _State} -> From ! {_Type, _State}, handle(_State)
      end;
    stop ->
      receive
        Bagger -> io:format("~p~n", [Bagger])
      after
        1 -> finish
      end
  end.


%%API
start_link(Module, Args) ->
  Pid=spawn(?MODULE, init, [Module, Args]),
  register(Module,Pid).

init(Module, Args) ->
  {ok, State} = Module:init(Args),
  handle(State).

call(Module, Request) ->
  Module ! {Module, self(), Request},
  receive
    {noreply, _State} -> ok;
    {reply, State} -> State
  end.

terminate(Module) ->
  Module ! stop.

main(_T) ->
  start_link(chat, "chat_center").