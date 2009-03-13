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

    read_vertex(DataChunk,[]).

read_vertex(Chunk, Acc) when size(Chunk) > 0 ->
    io:format("~p \n", [size(Chunk)]),
    <<Vertex:96/binary, Rest/binary>> = Chunk,
    <<V1:12/binary,N1:12/binary,U1:8/binary,
      V2:12/binary,N2:12/binary,U2:8/binary,
      V3:12/binary,N3:12/binary,U3:8/binary>> = Vertex,
    
    Vertexs = lists:map(fun({V,N,U}, Acc2) ->
				[{convert_from_binary(V),
				  convert_from_binary(N),
				  convert_from_binary(U)}| Acc2]
			end, [{V1, N1, U1},
			      {V2, N2, U2},
			      {V3, N3, U3}]),
    read_vertex(Rest, [Vertexs | Acc]);
read_vertex(<<>>, Acc) ->
    Acc.

 
convert_from_binary(<<X:32/float-little,Y:32/float-little,Z:32/float-little>>) ->
    {X, Y, Z};
convert_from_binary(<<U:32/float-little,V:32/float-little>>) ->
    {U,V}.

