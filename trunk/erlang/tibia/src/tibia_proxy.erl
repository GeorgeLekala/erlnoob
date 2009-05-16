%%%-------------------------------------------------------------------
%%% File    : tibia_proxy.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  4 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_proxy).

-compile(export_all).

-include("tibia.hrl").

-define(PROXY_PORT, 7172).
-define(SERVER_PORT, 7175).
-define(SERVER_IP, "localhost").


-record(state, {server_socket,
		client_socket,
		key}).


start() ->
    spawn_link(fun() -> tcp_start(?PROXY_PORT) end),
    crypto:start(),
    mnesia:start(),
    ok.

tcp_start(ListenPort) ->
    {ok, ListenSocket} = gen_tcp:listen(ListenPort, [binary, inet]),
    
    tcp_listen_loop(ListenSocket).


tcp_listen_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    Pid = spawn(?MODULE, tcp_init_connect,
			[#state{client_socket = Socket}]),
	    gen_tcp:controlling_process(Socket, Pid),
	    Pid;
	{error, Reason} ->
	    io:format("{error, ~p}\n", [Reason])
    end,
    ?MODULE:tcp_listen_loop(ListenSocket).


tcp_init_connect(State) ->
    case gen_tcp:connect(?SERVER_IP, ?SERVER_PORT, [binary, inet]) of
	{ok, ServerSocket} ->
	    tcp_loop(State#state{server_socket = ServerSocket});
	Any ->
	    io:format("tcp_init_connect failed\n Returned: ~p\n", [Any])
    end.

tcp_loop(State=#state{client_socket = ClientSocket,
		      server_socket = ServerSocket}) ->
    inet:setopts(ClientSocket, [{active, once}]),
    inet:setopts(ServerSocket, [{active, once}]),
    receive
	{tcp, ServerSocket,Msg= <<Len:16/unsigned-integer-little,
				 Data:Len/binary>>} when Len =:= 84 ->
	    %%parse_server_message(State#state.key, Data),
	    gen_tcp:send(ClientSocket, Msg),
	    ?MODULE:tcp_loop(State);
	{tcp, ServerSocket, Data} ->
	    gen_tcp:send(ClientSocket, Data),
	    ?MODULE:tcp_loop(State);
	{tcp, ClientSocket, Data} ->
	    %%Key = tibia_parse:parse_package(Data),
	    gen_tcp:send(ServerSocket, Data),
	    ?MODULE:tcp_loop(State);%#state{key = Key});
	{tcp, ClientSocket, Data} ->
	    gen_tcp:send(ServerSocket, Data),
	    ?MODULE:tcp_loop(State);
	{tcp_closed, ServerSocket} ->
	    gen_tcp:close(ClientSocket),
	    ok;
	{tcp_closed, ClientSocket} ->
	    gen_tcp:close(ServerSocket),
	    ok;
	Any ->
	    io:format("tcp unknown message ~p\n", [Any]),
	    ?MODULE:tcp_loop(State)
    end.


    
parse_server_message(#key{k1 = K1, k2 = K2, k3 = K3, k4 = K4}, Msg) ->
    io:format("D: ~p\n", [xtea:decrypt(Msg, [K1,K2,K3,K4])]).

%%io:format("MotdSize: ~p\n", [Msg]).


gcd(A,0) -> A;
gcd(A,B) -> gcd(B, A rem B).

