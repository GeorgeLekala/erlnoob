%%%-------------------------------------------------------------------
%%% File    : tibia_parse.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 15 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_parse).

%%-export([parse_server_packet/2,xtea_encrypt/2,xtea_decrypt/2,
%%	 parse_client_packet/2,test/0]).
-compile(export_all).

-include("tibia.hrl").

-record(character, {name,server_name,ip,port}).

parse_server_packet(State,Packet = <<Checksum:32/?UINT,Msg/binary>>) ->
    Decrypted = xtea:decrypt(State#state.key,Msg),
    <<_Size:16/?UINT, Protocol:8/?UINT,Message/binary>> = Decrypted,
    Reply =
	case Protocol of
	    16#14 ->
		modify_login_packet(State#state.key,Message);
	    _ ->
		<<(size(Packet)):16/?UINT,Packet/binary>>
	end,
    gen_tcp:send(State#state.client_socket, Reply),
    State.

modify_login_packet(Key, <<MotdSize:16/?UINT,Motd:MotdSize/binary,
			  16#64:8/?UINT,NumChars:8/?UINT,Chars/binary>>) ->
    {Characters,Rest} = get_characters(NumChars, Chars),
    <<PremDays:16/?UINT,_/binary>> = Rest,
    NewChars = build_characters(Characters),
    NewMsg = <<16#14:8/?UINT,(size(Motd)):16/?UINT,Motd/binary,
	      16#64:8/?UINT,(length(Characters)):8/?UINT,
	      NewChars/binary,PremDays:16/?UINT>>,
    Reply = xtea:encrypt(Key,<<(size(NewMsg)):16/?UINT,NewMsg/binary>>),
    <<(size(Reply)+4):16/?UINT,(erlang:adler32(Reply)):32/?UINT,Reply/binary>>.
    

build_characters(Chars) ->
    build_characters(Chars, <<>>).

build_characters([#character{name = Name,
			     server_name = ServerName}|Chars], Acc) ->
    build_characters(Chars, <<(size(Name)):16/?UINT,Name/binary,
			     (size(ServerName)):16/?UINT,ServerName/binary,
			     (list_to_binary([127,0,0,1]))/binary,7172:16/?UINT,Acc/binary>>);
build_characters([], Acc) ->
    Acc.
    
get_characters(NumChars,Chars) ->
    get_characters(NumChars, Chars, []).
    
get_characters(0,Rest, Acc) ->
    {Acc,Rest};
get_characters(NumChars, <<NameSize:16/?UINT,Name:NameSize/binary,
			  ServerSize:16/?UINT,Server:ServerSize/binary,
			  ServerIP:4/binary,Port:16/?UINT,Chars/binary>>, Acc) ->
    Char = #character{name = Name,
		      server_name = Server,
		      ip = list_to_tuple(binary_to_list(ServerIP)),
		      port = Port},
    get_characters(NumChars-1,Chars,[Char|Acc]).

parse_client_packet(State, Packet) when State#state.account =:= undefined ->
    <<Checksum:32/?UINT,ProtocolId:8/?UINT,_OS:16/?UINT,
     _Version:16/?UINT,Msg/binary>> = Packet,
    case ProtocolId of
	?LOGIN_PROTOCOL ->
	    tibia_login:parse_login_package(State, Msg);
	?GAME_PROTOCOL ->
	    tibia_login:parse_first_game_packet(State, Msg);
	_ ->
	    State
    end;
parse_client_packet(State, <<Checksum:32/?UINT,Msg/binary>>) ->
    Msg2 = xtea:decrypt(State#state.key, Msg),
    io:format("Msg: ~p\n", [Msg2]),
    State.






test(Key,
     <<_:6/binary, Reply/binary>>,
     <<_:6/binary, Packet/binary>>) ->
    io:format("Reply : ~p\n", [xtea:decrypt(Key, Reply)]),
    io:format("Packet: ~p\n", [xtea:decrypt(Key, Packet)]).
    
