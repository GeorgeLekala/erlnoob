%%%-------------------------------------------------------------------
%%% File    : tarning.erl
%%% Author  : Olle Mattsson <olle@baltasar>
%%% Description : 
%%%
%%% Created : 12 Sep 2008 by Olle Mattsson <olle@baltasar>
%%%-------------------------------------------------------------------
-module(tarning).

-export([start/0]).

start() ->
    roll().

roll() ->
    Datorn = random:uniform(6),
    Du = random:uniform(6),
    if 
	Datorn > Du ->
	    io:format("Datorn fick ~p och du ~p. Du forlorade!\nForsok igen\n", [Datorn, Du]);
	Datorn =:= Du ->
	    io:format("Datorn fick ~p och du ~p. Det blev lika!\nForsok igen\n", [Datorn, Du]);
	true ->
	    io:format("Datorn fick ~p och du ~p. Du vann!\nSpela igen?\n", [Datorn, Du])
    end.
