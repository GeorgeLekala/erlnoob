%%%-------------------------------------------------------------------
%%% File    : socket.erl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created :  6 Nov 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------
-module(socket).
-compile(export_all).

setup_socket() ->
    spawn_link(fun() -> setup_socket("www.google.com", 1) end).

setup_socket(Host, Counter) when Counter =< 30 ->
    {ok,Socket} = gen_tcp:connect(Host,80,[binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n" ),
    receive_data(Socket, []),
    spawn_link(fun() -> setup_socket(Host, Counter +1) end);
setup_socket(_Host, _Counter) ->
    io:format("ok\n", []).



test(Count) ->
    {Time, _Value} = timer:tc(?MODULE, spawn_procs, [Count]),
    io:format("Time: ~p\n", [Time/1000000]).

spawn_procs(Count) when Count > 0 ->
    spawn_link(fun() -> loop() end),
    spawn_procs(Count -1);
spawn_procs(_Count) ->
    io:format("Done\n", []).

loop() ->
    ok.


time_tibia() ->
    {Time, Value} = timer:tc(?MODULE, get_tibia_status, [1]),
    io:format("Time: ~p\nResult: ~p\n", [Time /1000000, Value]).

get_tibia_status() ->
    [get_tibia_status(Type) || Type <-  [1,2]].

get_tibia_status(Type) ->
    Info = 
	case Type of
	    1 -> %% Server Info
		[6, 0, 255, 255, "info"];
	    2 -> %% Player Info
		[6, 0, 255, 253, "info"]
	end,
    Host = "narozia.com",
    Port = 7171,
    connect_and_send(Host, Port, Info, 0).

connect_and_send(Host, Port, Info, Counter) ->
    {ok,Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, line}]),
    ok = gen_tcp:send(Socket, Info),
    case receive_data(Socket, []) of
	[] when Counter < 1000 ->
	    %% timer:sleep(timer:seconds(1)),
	    connect_and_send(Host, Port, Info, Counter + 1);
	Any ->
	    io:format("Retries=~p; chunks=~p; xml: ~p\n",
		      [Counter, length(Any), list_to_binary(Any)]),
	    ok
    end.
    

receive_data(Socket, SoFar) ->
    receive
	{tcp,Socket,Bin} ->
	    %% io:format("Got: ~p\n", [Bin]),
	    receive_data(Socket, [Bin|SoFar]);
	{tcp_closed,Socket} ->
	    %% io:format("Connection closed.\n", []),
	    lists:reverse(SoFar)
    after 30000 ->
	    exit(timeout)
    end.
