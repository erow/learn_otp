%%%-------------------------------------------------------------------
%%% @author PC
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 七月 2016 10:23
%%%-------------------------------------------------------------------
-module(chat_server).
-author("PC").

-behaviour(s_gen_server).
%% history=[Msg::{who,string()}]
-record(state, {history = [], remain = [], servername}).

-export([init/1, handle_call/3]).
%% API
-export([start_link/0, recv/0, send/2,stop/0,show_history/0]).

start_link() ->
  s_gen_server:start_link(?MODULE, "chat_center").

send(Who, Word) ->
  s_gen_server:call(?MODULE, {send, Who, Word}).

show_history() ->
  State = s_gen_server:call(?MODULE, fetch),
  State#state.history.

stop() ->
  s_gen_server:terminate(?MODULE).

-spec recv() -> #state{} .
recv() ->
  State = s_gen_server:call(?MODULE, fetch),
  s_gen_server:call(?MODULE, {update, State#state{remain = [], history = State#state.history ++ [State#state.remain]}}),
  State#state.remain.
%% behaviour


init(_Arg) ->
  {ok, #state{servername = _Arg}}.

handle_call({send, Who, String}, _From, State) ->
  {noreply, #state{remain = State#state.remain ++ [{Who, String}]}};
handle_call(fetch, _From, State) ->
  {reply, State};
handle_call({update, NewState}, _From, _) ->
  {noreply, NewState}.


