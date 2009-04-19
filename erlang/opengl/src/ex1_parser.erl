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
   
    %% Read out the header
    <<16#E15E:16/little,16#0000:16/little,
     DataChunkSize:32/little,
     _CreationInfo:120/binary>> = Header,

    %% Read DataChunk
    <<DataChunk:DataChunkSize/binary,
     MaterialSize:32/little,MaterialBin:MaterialSize/binary,
     ObjectRefSize:32/little,ObjectRefBin:ObjectRefSize/binary>> = Body,

    Materials = get_materials(MaterialBin),
    ObjRefs = get_objects(ObjectRefBin),
    %%gen(Header,ReadData, ReadMaterial, ReadObjRef),
    {DataChunk, Materials, ObjRefs}.


get_materials(Material) ->
    get_materials(Material, []).
get_materials(<<NSz:32/little, Name:NSz/binary, 
	       DR:32/float,DG:32/float,DB:32/float,DA:32/float,
	       AR:32/float,AG:32/float,AB:32/float,AA:32/float,
	       SR:32/float,SG:32/float,SB:32/float,SA:32/float,
	       ER:32/float,EG:32/float,EB:32/float,EA:32/float,
	       Sh:32/float,NoOfImages:32/little,Rest/binary>>, Acc) ->
    {Maps, Next} = get_maps(NoOfImages, Rest, []),
    Mat = {Name,{DR,DG,DB,DA},{AR,AG,AB,AA},
	   {SR,SG,SB,SA},{ER,EG,EB,EA},Sh,
	   Maps},
    get_materials(Next, [Mat|Acc]);
get_materials(<<>>, Acc) ->
    Acc.

get_maps(0, Next, Acc) -> {Acc,Next};
get_maps(I, <<TSz:32/little,Type:TSz/binary,
	     FSz:32/little,File:FSz/binary, Next/binary>>, Acc) ->
    get_maps(I-1, Next, [{Type,File}|Acc]).

get_objects(ObjectBin) ->
    get_objects(ObjectBin, []).
get_objects(<<BSz:32/little,ObjBlock:BSz/binary,Next/binary>>,Acc) ->
    Object = get_object(ObjBlock),
    get_objects(Next,Object++Acc);
get_objects(<<>>,Acc) ->
    Acc.

get_object(<<NameSz:32/little,_Name:NameSz/binary,
	    NoMeshes:32/little,Meshes/binary>>) ->
    MeshPtrs = get_mesh_ptr(NoMeshes,Meshes,[]),
    MeshPtrs.

get_mesh_ptr(0, _, Acc) ->
    lists:reverse(Acc);
get_mesh_ptr(I,<<MSz:32/little,Mat:MSz/binary,Start:32/little,Verts:32/little,Next/binary>>,Acc) ->
    get_mesh_ptr(I-1,Next,[{Mat,Start,Verts}|Acc]).


    
 
convert_from_binary(<<X:32/float-little,
		      Y:32/float-little,
		      Z:32/float-little>>) ->
    {X, Y, Z};
convert_from_binary(<<U:32/float-little,
		      V:32/float-little>>) ->
    {U,V}.

print(Filename) ->
    {_Data,Mat,ObjRef}= parse(Filename),
    %%Data2 = lists:keysort(1, Data),
    io:format("Material: ~p\nObject Refs: ~p\n",[Mat,ObjRef]).



