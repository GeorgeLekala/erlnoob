%%%-------------------------------------------------------------------
%%% File    : ex1_parser.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 12 Mar 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(ex1_parser).

-compile(export_all).

-include("../include/ex1_parser.hrl").

parse(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    <<Header:128/binary, Body/binary>> = Bin,
   
    <<16#E15E:16/little,16#0000:16/little,
     DataChunkSize:32/little,
     _CreationInfo:120/binary>> = Header,

    <<DataChunk:DataChunkSize/binary, Body2/binary>> = Body,
    
    <<MaterialSize:32/little, Body3/binary>> = Body2,
    <<Material:MaterialSize/binary, Body4/binary>> = Body3,

    <<ObjectRefSize:32/little, Body5/binary>> = Body4,
    <<ObjectRef:ObjectRefSize/binary,Rest/binary>> = Body5,

    io:format("~p\n", [size(Rest)]),

    {read_vertex(DataChunk),
     read_material(Material),
     read_object_ref(ObjectRef)}.


%% Read Vertex
read_vertex(DataChunk) ->
    read_vertex(DataChunk, []).

read_vertex(Chunk, Acc) when size(Chunk) > 0 ->
    <<V:12/binary,N:12/binary,U:8/binary, Rest/binary>> = Chunk,
    Vertex = 
	{convert_from_binary(V),
	 convert_from_binary(N),
	 convert_from_binary(U)},
    read_vertex(Rest, [Vertex | Acc]);
read_vertex(<<>>, Acc) ->
    lists:reverse(lists:flatten(Acc)).


%% Read Material :: FIX ME - Needs implementation
read_material(Material) ->
    read_material(Material, []).

read_material(Material, _Acc) when size(Material) > 0 ->
    [];
read_material(<<>>, Acc) ->
    lists:reverse(Acc).


%% Read ObjectRef :: FIX ME - Needs implementation
read_object_ref(ObjectRef) ->
    read_material(ObjectRef, []).

read_object_ref(<<_ObjNameBinSize:32/little,_Rest/binary>>, _Acc) ->
    [];
read_object_ref(<<>>, Acc) ->
    lists:reverse(Acc).



 
convert_from_binary(<<X:32/float-little,
		      Y:32/float-little,
		      Z:32/float-little>>) ->
    {X, Y, Z};
convert_from_binary(<<U:32/float-little,
		      V:32/float-little>>) ->
    {U,V}.

