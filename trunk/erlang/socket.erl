%%%-------------------------------------------------------------------
%%% File    : socket.erl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created :  6 Nov 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------
-module(socket).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

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
    io:format("Time: ~p\nResult: ~p seconds\n", [Time /1000000, Value]).

get_tibia_status() ->
    [get_tibia_status(Type) || Type <-  [1,2]].

get_tibia_status(Type) ->
    {Info, Parse} = 
	case Type of
	    1 -> %% Server Info
		{[6, 0, 255, 255, "info"], true};
	    2 -> %% Player Info
		{[6, 0, 255, 253, "info"], false}
	end,
    Host = "127.0.0.1",
    Port = 7171,
    {Data, File} = connect_and_send(Host, Port, Info, 0),
    case Data of
	error ->
	    XmlContentOffline =
		"<?xml version=\"1.0\"?>\n"
		"<tsqp version=\"1.0\">"
		"<players online=\"offline\" />"
		"</tsqp>",
	    file:write_file(File, XmlContentOffline);
	_Other ->
	    ok
    end,
    case Parse of
	true ->
	    case Data of
		[] ->
		    ok;
		_Any ->
		    parse_xml(File)
	    end;
	false ->
	    ok
    end,
    timer:sleep(timer:minutes(5)),
    get_tibia_status(Type) .

connect_and_send(Host, Port, Info, Counter) ->
    File = "serverinfo.xml",
    case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, line}]) of
	{ok,Socket} ->
	    ok = gen_tcp:send(Socket, Info),
	    case receive_data(Socket, []) of
		[] when Counter < 1000 ->
		    connect_and_send(Host, Port, Info, Counter + 1);
		Any ->
		    io:format("Retries=~p; chunks=~p\n",
			      [Counter, length(Any)]),
		    file:write_file(File, Any),
		    {Any, File}
	    end;
	{error, Reason} ->
	    io:format("Error: ~p\n", [Reason]),
	    {error, File}
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


parse_xml(InFile) ->
    {XmlRec, []} = xmerl_scan:file(InFile),
    IoList = gen_ini(XmlRec#xmlElement.content, []),
    %%io:format("~p\n", [IoList]),
    file:write_file("serverinfo.ini", IoList).


gen_ini([#xmlElement{attributes = Attributes} | T], Data) ->
    IniAttributes = process_attrubutes(Attributes, []),
    gen_ini(T, [IniAttributes | Data]);
gen_ini([], Data) ->
    lists:reverse(Data).

process_attrubutes([H | T], Data) ->
    case H of
	#xmlAttribute{} ->
	    Name = H#xmlAttribute.name,
	    Value = H#xmlAttribute.value,
	    case Value of
		[] ->
		    process_attrubutes(T, Data);
		_Other ->
		    Data2 = [io_lib:format("~p = ~p\n", [Name, Value]) | Data],
		    process_attrubutes(T, Data2)
	    end;
	#xmlText{} ->
	    process_attrubutes(T, Data)
    end;
process_attrubutes([], Data) ->
    lists:reverse(Data).

get_players_rec([H | T]) ->
    case H#xmlElement.name of
	players ->
	    H;
	_Other ->
	    get_players_rec(T)
    end;
get_players_rec([]) ->
    notfound.

    
