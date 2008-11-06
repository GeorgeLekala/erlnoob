%%%-------------------------------------------------------------------
%%% File    : snake.erl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created : 19 Sep 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------
-module(snake).

-export([start/0]).


start() ->
    spawn(snake_wxgui, init, []).
