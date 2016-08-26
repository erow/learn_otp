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

main(_Args)-> chat_server:start_link().
