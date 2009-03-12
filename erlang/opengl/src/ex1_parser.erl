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

    io:format("~p\n", [DataChunkSize]).

read_vertex(Chunk) ->
    <<Vertex:32>> = Chunk,
    <<Vx:4,Vy:4,Vz:4,Nx:4,Ny:4,Nz:4,U:4,V:4>> = Vertex.


