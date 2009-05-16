%%%-------------------------------------------------------------------
%%% File    : chat.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 24 Apr 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(chat_server).

-compile(export_all).



start() ->
    spawn(fun() -> start_server() end).



start_server() ->		  
    gen_tcp:listen(1337, [list]),
    receive 
	Any ->
	    io:format("~p\n", [Any])
    end.
