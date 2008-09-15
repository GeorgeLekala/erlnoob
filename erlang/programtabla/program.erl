%%%-------------------------------------------------------------------
%%% File    : program.erl
%%% Author  : Olle Mattsson <olle@baltasar>
%%% Description : 
%%%
%%% Created : 15 Sep 2008 by Olle Mattsson <olle@baltasar>
%%%-------------------------------------------------------------------
-module(program).

-compile(export_all).

tabla(Day, Channel) ->
    {ok, Content} = file:consult(filename:join([program, Day, integer_to_list(Channel)])),
    io:format("~p\n", [Content]).
    
	
