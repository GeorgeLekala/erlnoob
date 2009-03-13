%%%-------------------------------------------------------------------
%%% File    : ex1_parser.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 12 Mar 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(ex1_parser).

-compile(export_all).

parse(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    <<Header:128/binary, Body/binary>> = Bin,
   
    <<16#E15E:16/little,16#0000:16/little,
     DataChunkSize:32/little,
     CreationInfo:120/binary>> = Header,

    <<DataChunk:DataChunkSize/binary, Body2/binary>> = Body,
    
    <<MaterialChunkSize:32/little, Body3/binary>> = Body2,
    <<MaterialChunk:MaterialChunkSize/binary, Body4/binary>> = Body3,

    <<ObjectRefChunkSize:32/little, ObjectRefChunk/binary>> = Body4,

    read_vertex(DataChunk).

read_vertex(Chunk) when size(Chunk) > 0 ->
    io:format("~p \n", [size(Chunk)]),
    <<Vertex:32/binary, Rest/binary>> = Chunk,
    <<Vx:32/integer-signed,Vy:32/integer-signed,Vz:32/integer-signed,
     Nx:32/float,Ny:32/float,Nz:32/float,
     U:32/little,S:32/little>> = Vertex,
    io:format("~p ~p ~p\n", [Vx, Vy, Vz]),
    read_vertex(Rest);
read_vertex(_) ->
    ok.

