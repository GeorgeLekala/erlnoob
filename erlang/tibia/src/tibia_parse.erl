%%%-------------------------------------------------------------------
%%% File    : tibia_parse.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 15 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_parse).

-compile(export_all).

-include("tibia.hrl").


parse_package(A= <<148:16/?UINT,_Checksum:4/binary,
	       _ProtocolId:8/?UINT, Version:16/?UINT,_:12/binary,
	       Msg:128/binary>>) when Version =:= 840 ->
    Key = parse_first_package(Msg),
    Key;
parse_package(Msg) when size(Msg) =:= 151 ->
    error_logger:format("Wrong size. ~p", [size(Msg)]),
    exit(wrong_packet_size).



parse_first_package(Msg) ->
    Decrypted = crypto:rsa_private_decrypt(Msg, [?e, ?n, ?d], rsa_no_padding),
    <<_:1/binary,
     K1:32/?UINT,K2:32/?UINT,
     K3:32/?UINT,K4:32/?UINT,
     AccSize:16/?UINT,_Acc:AccSize/binary,
     PassSize:16/?UINT,_Pass:PassSize/binary,_/binary>> = Decrypted,
    #key{k1 = K1,
	 k2 = K2,
	 k3 = K3,
	 k4 = K4}.
