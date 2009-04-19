%%%-------------------------------------------------------------------
%%% File    : pong.erl
%%% Author  : Olle Mattsson <olle@mudkipz>
%%% Description : 
%%%
%%% Created : 31 Dec 2008 by Olle Mattsson <olle@mudkipz>
%%%-------------------------------------------------------------------
-module(pong).

-compile(export_all).

start() ->
    pong_wxgui:create_main_window().
