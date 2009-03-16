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
    <<DataChunk:DataChunkSize/binary, Body2/binary>> = Body,
    
    %% Read Materials
    <<MaterialSize:32/little, Body3/binary>> = Body2,
    <<MaterialBin:MaterialSize/binary, Body4/binary>> = Body3,

    %% Read ObjectRef
    <<ObjectRefSize:32/little, Body5/binary>> = Body4,
    <<ObjectRefBin:ObjectRefSize/binary>> = Body5,

    Material = parse_material(MaterialBin),
    ObjRef = read_object_ref(ObjectRefBin),
    %%gen(Header,ReadData, ReadMaterial, ReadObjRef),
    {DataChunk, Material, ObjRef}.



%% Read Material :: FIX ME - Needs implementation
parse_material(Material) ->
    parse_material(Material, []).

parse_material(Material, Acc) when size(Material) > 0 ->
    <<MatNameSize:32/little,Rest/binary>> = Material,
    <<MatName:MatNameSize/binary,Floats/binary>> = Rest,
    <<DR:32/float,DG:32/float,DB:32/float,DA:32/float,
     AR:32/float,AG:32/float,AB:32/float,AA:32/float,
     SR:32/float,SG:32/float,SB:32/float,SA:32/float,
     ER:32/float,EG:32/float,EB:32/float,EA:32/float,
     Sh:32/float,MapSize:32/little,Maps0/binary>> = Floats,
    

    {Material2,Textures} = read_image(Maps0, MapSize),
    parse_material(Material2, [#material{name      = get_name(MatName),
					 diffuse   = {DR,DG,DB,DA},
					 ambient   = {AR,AG,AB,AA},
					 specular  = {SR,SG,SB,SA},
					 emission  = {ER,EG,EB,EA},
					 shininess = Sh,
					 textures  = Textures}|Acc]);
parse_material(<<>>, Acc) ->
    lists:reverse(Acc).


read_image(Textures, Size) ->
    read_image(Textures, [], Size).

read_image(Textures, Acc, Size) when Size =/= 0 ->
    <<TypeBinSize:32/little, Rest/binary>> = Textures,

    <<TypeBin:TypeBinSize/binary,
     FileBinSize:32/little, Rest2/binary>> = Rest,

    <<FileBin:FileBinSize/binary, Textures2/binary>> = Rest2,
    
    Texture = {get_name(TypeBin), get_name(FileBin)},
    read_image(Textures2, [Texture| Acc], Size-1);
read_image(Mat, Acc, _) ->
    {Mat, lists:reverse(Acc)}.


%% Read ObjectRef
read_object_ref(ObjBin) ->
    read_object_ref(ObjBin, []).

read_object_ref(<<ObjNameBinSize:32/little,Rest/binary>>, Acc) ->
    <<ObjNameBin:ObjNameBinSize/binary, Rest2/binary>> = Rest,
    case size(Rest2) of
	0 ->
	    read_object_ref(Rest2, Acc);
	_ ->
	    <<MatsLen:32/little,Rest3/binary>> = Rest2,
	    {Rest4, Refs} = read_refs(Rest3, MatsLen),
	    read_object_ref(Rest4, [{get_name(ObjNameBin),
				     get_name(Refs)} | Acc])
    end;
read_object_ref(<<>>, Acc) ->
    lists:reverse(Acc).


read_refs(Bin, Length) ->
    read_refs(Bin, [], Length).

read_refs(<<Size:32/little,Bin/binary>>, Acc, Length) when Length =/= 0 ->
    <<MatName:Size/binary,Bin2/binary>> = Bin,
    <<Start:32/little,Verts:32/little,Bin3/binary>> = Bin2,
    read_refs(Bin3, [{get_name(MatName), Start, Verts}|Acc], Length-1);
read_refs(Rest, Acc, 0) ->
    {Rest, lists:reverse(Acc)}.


get_name(BinList) ->
    [_|T] = lists:reverse(binary_to_list(BinList)),
    lists:reverse(T).
    
 
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



