%%%-------------------------------------------------------------------
%%% @author erow <>
%%% @copyright (C) 2016, erow
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2016 by erow <>
%%%-------------------------------------------------------------------
-module(user_interface).

-behaviour(gen_server).
-include("chat_server.hrl").
%% API
-export([start_link/1, fetch_msg/1, send_msg/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(state, {name, history = [], remain = queue:new()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec fetch_msg(Pid :: pid()) -> Data :: term().
fetch_msg(Pid) ->
  gen_server:call(Pid, fetch_msg).
-spec send_msg(Pid :: pid(), To :: string(), Data :: term()) -> ok|{error, Reason :: string()}.
send_msg(Pid, To, Data) ->
  gen_server:call(Pid, {send_msg, To, Data}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(UserName) ->
  gen_server:start_link(?MODULE, [UserName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([UserName]) ->
  {ok, #state{name = UserName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


handle_call(fetch_msg, _From, State) ->
  case queue:out(State#state.remain) of
    {{value, Item}, Q2} ->
      {reply, Item#message.msg, State#state{remain = Q2, history = State#state.history ++ [Item]}};
    {empty, _} ->
      {reply, {error, "no message!"}, State}
  end;
handle_call({send_msg, To, Data}, _From, State) ->
  case chat_server_center:distribution(To, Data) of
    ok ->
      {reply, ok, State};
    {error, _} ->
      {reply, {error, "wrong user!"}, State}
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({recv, Msg}, State) ->
  {noreply, State#state{remain = queue:in(Msg, State#state.remain)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

