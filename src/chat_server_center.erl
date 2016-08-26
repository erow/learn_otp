%%%-------------------------------------------------------------------
%%% @author erow <>
%%% @copyright (C) 2016, erow
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2016 by erow <>
%%%-------------------------------------------------------------------
-module(chat_server_center).

-behaviour(gen_server).

-include("chat_server.hrl").
%% API
-export([start_link/0, distribution/2, register/1, query/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {history = [], remain = queue:new(), users = #{}}).

%%%===================================================================
%%% API
%%%===================================================================
-spec distribution(To :: string(), Data :: term()) -> {error, Reason :: string()}|ok.
distribution(To, Data) ->
  gen_server:call(?SERVER, {distribution, To, Data}).
-spec register(UserName :: string()) -> {ok, Pid :: pid()}|error.
register(UserName) ->
  gen_server:call(?SERVER, {register, UserName}).
-spec query(UserName :: string()) -> {ok, Pid :: pid()}|not_find.
query(UserName) ->
  gen_server:call(?SERVER, {query, UserName}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  {ok, #state{}}.

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

handle_call({register, UserName}, _From, State) ->
  case maps:find(UserName, State#state.users) of
    {ok, _} ->
      {reply, error, State};
    error ->
      {ok, Child} = chat_user_sup:start_child(UserName),
      Users = State#state.users,
      {reply, {ok, Child}, State#state{users = Users#{UserName=>Child}}}
  end;
handle_call({query, UserName}, _From, State) ->
  query(UserName, State);
handle_call({distribution, To, Data}, _From, State) ->
  case maps:find(To, State#state.users) of
    {ok, Pid} ->
      From = maps:fold(fun(K, V, Name) -> case V == _From of
                                            true ->
                                              K;
                                            false ->
                                              Name
                                          end end,
        undefined, State#state.users),
      gen_server:cast(Pid, {recv, #message{msg = Data, timestamp = time(), reciever = To, sender = From}}),
      {reply, ok, State};
    error ->
      {reply, {error, "not find"}, State}
  end
.
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
handle_cast(msg, State) ->
  {noreply, State}.

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


%% internal function
query(UserName, State) ->
  case maps:find(UserName, State#state.users) of
    {ok, Pid} ->
      {reply, {ok, Pid}, State};
    error ->
      {reply, not_find, State}
  end.
