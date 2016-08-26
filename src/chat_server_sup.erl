%%%-------------------------------------------------------------------
%%% @author erow <>
%%% @copyright (C) 2016, erow
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2016 by erow <>
%%%-------------------------------------------------------------------
-module(chat_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Center = #{id => chat_server_center_id,
               start => {chat_server_center, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [chat_server_center]},
    Users = #{id => user_sup_id,
               start => {user_sup, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => supervisor,
               modules => [user_sup]},
    {ok, {SupFlags, [Center]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
