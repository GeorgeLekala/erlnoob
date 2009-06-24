%%%-------------------------------------------------------------------
%%% File    : tibia_iomap.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  2 Jun 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_iomap).

-compile(export_all).

-include("tibia.hrl").

-define(OTBM_ATTR_DESCRIPTION, 1).
-define(OTBM_ATTR_EXT_FILE, 2).
-define(OTBM_ATTR_TILE_FLAGS, 3).
-define(OTBM_ATTR_ACTION_ID, 4).
-define(OTBM_ATTR_UNIQUE_ID, 5).
-define(OTBM_ATTR_TEXT, 6).
-define(OTBM_ATTR_DESC, 7).
-define(OTBM_ATTR_TELE_DEST, 8).
-define(OTBM_ATTR_ITEM, 9).
-define(OTBM_ATTR_DEPOT_ID, 10).
-define(OTBM_ATTR_EXT_SPAWN_FILE, 11).
-define(OTBM_ATTR_RUNE_CHARGES, 12).
-define(OTBM_ATTR_EXT_HOUSE_FILE, 13).
-define(OTBM_ATTR_HOUSEDOORID, 14).
-define(OTBM_ATTR_COUNT, 15).
-define(OTBM_ATTR_DURATION, 16).
-define(OTBM_ATTR_DECAYING_STATE, 17).
-define(OTBM_ATTR_WRITTENDATE, 18).
-define(OTBM_ATTR_WRITTENBY, 19).
-define(OTBM_ATTR_SLEEPERGUID, 20).
-define(OTBM_ATTR_SLEEPSTART, 21).
-define(OTBM_ATTR_CHARGES, 22).
-define(OTBM_ATTR_NAME, 30).
-define(OTBM_ATTR_PLURALNAME, 31).
-define(OTBM_ATTR_ARTICLE, 32).
-define(OTBM_ATTR_ATTACK, 33).
-define(OTBM_ATTR_EXTRAATTACK, 34).
-define(OTBM_ATTR_DEFENSE, 35).
-define(OTBM_ATTR_EXTRADEFENSE, 36).
-define(OTBM_ATTR_ARMOR, 37).
-define(OTBM_ATTR_ATTACKSPEED, 38).
-define(OTBM_ATTR_HITCHANCE, 39).


-define(OTBM_ROOTV1, 1).
-define(OTBM_MAP_DATA, 2).
-define(OTBM_ITEM_DEF, 3).
-define(OTBM_TILE_AREA, 4).
-define(OTBM_TILE, 5).
-define(OTBM_ITEM, 6).
-define(OTBM_TILE_SQUARE, 7).
-define(OTBM_TILE_REF, 8).
-define(OTBM_SPAWNS, 9).
-define(OTBM_SPAWN_AREA, 10).
-define(OTBM_MONSTER, 11).
-define(OTBM_TOWNS, 12).
-define(OTBM_TOWN, 13).
-define(OTBM_HOUSETILE, 14).
-define(OTBM_WAYPOINTS, 15).
-define(OTBM_WAYPOINT, 16).



-record(map, {header,
	      data}).

-record(header, {description,
		 spawn_file,
		 house_file,
		 version,
		 width,
		 height,
		 major_version_items,
		 minor_version_items}).

-record(tile, {x,y,z,
	       type,
	       house_id,
	       items,
	       flags}).
-record(item, {id,
	       flags,
	       unique_id,
	       action_id,
	       name,
	       plural_name,
	       article,
	       attack,
	       extra_attack,
	       defense,
	       extra_defense,
	       armor,
	       attack_speed,
	       hit_chance,
	       text,
	       written_date,
	       written_by,
	       special_description,
	       rune_charges,
	       charges,
	       duration,
	       decaying_state,
	       tele_destination,
	       count}).


load() ->
    load("test.otbm").

load(Filename) ->
    Data = open(Filename),
    {Header,Data2} = get_header(Data),
    
    #map{header = Header,
	 data = Data2},
    file:write_file("test.log", io_lib:format("~p", [load_map(Data2)])).

open(Filename) ->	
    {ok, File} = file:read_file(Filename),
    tibia_files:parse_file(File).




load_map(Nodes) ->
    load_map(Nodes, []).

load_map([], Acc) ->
    lists:reverse(lists:flatten(Acc));
load_map([#node{type = ?OTBM_TILE_AREA,data = Data,children = Children}|Nodes], Acc) ->
    <<X:16/?UINT,Y:16/?UINT,Z:8/?UINT>> = Data,
    Base = #tile{x=X,y=Y,z=Z},
    Tiles = get_tiles(Children,Base),
    load_map(Nodes, [Tiles|Acc]);
load_map([#node{type = ?OTBM_TOWNS}|Nodes], Acc) ->
    load_map(Nodes,Acc);
load_map([#node{type = ?OTBM_WAYPOINTS}|Nodes], Acc) ->
    load_map(Nodes,Acc).



get_tiles(Nodes, Base) ->
    get_tiles(Nodes, Base, []).

get_tiles([], _Base, Acc) ->
    Acc;
get_tiles([#node{type = ?OTBM_TILE,data = Data,children = Children}|Tiles], Base, Acc) ->
    try <<X:8/?UINT,Y:8/?UINT,Rest/binary>> = Data,
	Attributes = get_tile_attributes(Rest),
	get_tiles(Tiles,Base,[#tile{x = Base#tile.x + X,
				    y = Base#tile.y + Y,
				    z = Base#tile.z,
				    type  = proplists:get_value(item, Attributes),
				    items = get_items(Children),
				    flags = proplists:get_value(flags, Attributes)}|Acc])
    catch _:_ -> get_tiles(Tiles, Base, Acc)
    end;
get_tiles([#node{type = ?OTBM_HOUSETILE,data = Data,children = Children}|Tiles], Base, Acc) ->
    try <<X:8/?UINT,Y:8/?UINT,HouseId:32/?UINT,Rest/binary>> = Data,
	Attributes = get_tile_attributes(Rest),
	get_tiles(Tiles,Base,[#tile{x = Base#tile.x + X,
				    y = Base#tile.y + Y,
				    z = Base#tile.z,
				    house_id = HouseId,
				    type  = proplists:get_value(item, Attributes),
				    items = get_items(Children),
				    flags = proplists:get_value(flags, Attributes)}|Acc])
    catch _:_ -> get_tiles(Tiles, Base, Acc)
    end;
get_tiles([#node{}|Tiles], Base,Acc) ->
    get_tiles(Tiles, Base, Acc).




get_tile_attributes(Data) ->
    get_tile_attributes(Data, []).

get_tile_attributes(<<>>, Acc) ->
    Acc;
get_tile_attributes(<<Attr:8/?UINT,Rest/binary>>, Acc) ->
    case Attr of
	?OTBM_ATTR_ITEM ->
	    <<ItemID:16/?UINT,Rest2/binary>> = Rest,
	    get_tile_attributes(Rest2, [{item, #item{id = ItemID}}|Acc]);
	?OTBM_ATTR_TILE_FLAGS ->
	    <<Flags:32/?UINT,Rest2/binary>> = Rest,
	    get_tile_attributes(Rest2, [{flags, Flags}|Acc]);
	_ ->
	    io:format("~p\n", [Attr]),
	    get_tile_attributes(Rest,Acc)
    end.

get_items(Nodes) ->
    get_items(Nodes, []).

get_items(undefined, Acc) ->
    Acc;
get_items([], Acc) ->
    Acc;
get_items([#node{type = ?OTBM_ITEM,data = Data}|Nodes], Acc) ->
    try <<Id:16/?UINT,Rest/binary>> = Data,
	case Rest of
	    <<>> ->
		get_items(Nodes, [#item{id = Id}|Acc]);
	    _ ->
		Item = get_item_attributes(#item{id = Id},Rest),
		get_items(Nodes, [Item|Acc])
	end
    catch _:_ ->
	  get_items(Nodes, Acc)
    end;
get_items([#node{}|Nodes], Acc) ->
    get_items(Nodes,Acc).

get_item_attributes(Item, <<>>)  ->
    Item;
get_item_attributes(Item, <<Attr:8/?UINT,Rest/binary>>)  ->
    case Attr of
	?OTBM_ATTR_COUNT ->
	    <<Count:8/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{count = Count}, Rest2);
	?OTBM_ATTR_ACTION_ID ->
	    <<ActionId:16/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{action_id = ActionId}, Rest2);
	?OTBM_ATTR_UNIQUE_ID ->
	    <<UniqueId:16/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{unique_id = UniqueId}, Rest2);
	?OTBM_ATTR_NAME ->
	    <<Len:16/?UINT,Name:Len/binary,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{name = Name}, Rest2);
	?OTBM_ATTR_PLURALNAME ->
	    <<Len:16/?UINT,PluralName:Len/binary,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{plural_name = PluralName}, Rest2);
	?OTBM_ATTR_ARTICLE ->
	    <<Len:16/?UINT,Article:Len/binary,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{article = Article}, Rest2);
	?OTBM_ATTR_ATTACK ->
	    <<Attack:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{attack = Attack}, Rest2);
	?OTBM_ATTR_EXTRAATTACK ->
	    <<ExtraAttack:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{extra_attack = ExtraAttack}, Rest2);
	?OTBM_ATTR_DEFENSE ->
	    <<Defense:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{defense = Defense}, Rest2);
	?OTBM_ATTR_EXTRADEFENSE ->
	    <<ExtraDefense:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{extra_defense = ExtraDefense}, Rest2);
	?OTBM_ATTR_ARMOR ->
	    <<Armor:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{armor = Armor}, Rest2);
	?OTBM_ATTR_ATTACKSPEED ->
	    <<AttackSpeed:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{attack_speed = AttackSpeed}, Rest2);
	?OTBM_ATTR_HITCHANCE ->
	    <<HitChance:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{hit_chance = HitChance}, Rest2);
	?OTBM_ATTR_TEXT ->
	    <<Len:16/?UINT,Text:Len/binary,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{text = Text}, Rest2);
	?OTBM_ATTR_WRITTENDATE ->
	    <<WrittenDate:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{written_date = WrittenDate}, Rest2);
	?OTBM_ATTR_WRITTENBY ->
	    <<Len:16/?UINT,WrittenBy:Len/binary,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{written_by = WrittenBy}, Rest2);
	?OTBM_ATTR_DESC ->
	    <<Len:16/?UINT,SpecialDescription:Len/binary,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{special_description = SpecialDescription}, Rest2);
	?OTBM_ATTR_RUNE_CHARGES ->
	    <<RuneCharges:8/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{rune_charges = RuneCharges}, Rest2);
	?OTBM_ATTR_CHARGES ->
	    <<Charges:16/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{charges = Charges}, Rest2);
	?OTBM_ATTR_DURATION ->
	    <<Duration:32/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{duration = Duration}, Rest2);
	?OTBM_ATTR_DECAYING_STATE ->
	    <<State:8/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{decaying_state = State}, Rest2);
	?OTBM_ATTR_TELE_DEST ->
	    <<X:16/?UINT,Y:16/?UINT,Z:8/?UINT,Rest2/binary>> = Rest,
	    get_item_attributes(Item#item{tele_destination = #tile{x=X,y=Y,z=Z}}, Rest2)
	    
    end.






get_header([#node{type = 0,
		  data = <<Version:32/?UINT,
			  Width:16/?UINT,Height:16/?UINT,
			  MajorVersionItems:32/?UINT,
			  MinorVersionItems:32/?UINT>>,
		  children = [#node{type = 2,
				    data = Data,
				    children = Children}|_]}|_]) ->
    {List, _} = parse_header(Data),
    {#header{description = proplists:get_value(description, List),
	     spawn_file = proplists:get_value(spawn_file, List),
	     house_file = proplists:get_value(house_file, List),
	     version = Version,
	     width = Width,
	     height = Height,
	     major_version_items = MajorVersionItems,
	     minor_version_items = MinorVersionItems}, Children}.

parse_header(Bin) ->
    parse_header(Bin, []).

parse_header(<<>>, Attrs) ->
    {Attrs, <<>>};
parse_header(<<Attr:8/?UINT,Rest/binary>>, Attrs) ->
    case Attr of
	?OTBM_ATTR_DESCRIPTION ->
	    <<Size:16/?UINT,Description:Size/binary,Rest2/binary>> = Rest,
	    parse_header(Rest2, [{description,Description}|Attrs]);
	?OTBM_ATTR_EXT_HOUSE_FILE ->
	    <<Size:16/?UINT,HouseFile:Size/binary,Rest2/binary>> = Rest,
	    parse_header(Rest2, [{house_file,HouseFile}|Attrs]);
	?OTBM_ATTR_EXT_SPAWN_FILE ->
	    <<Size:16/?UINT,SpawnFile:Size/binary,Rest2/binary>> = Rest,
	    parse_header(Rest2, [{spawn_file,SpawnFile}|Attrs]);
	254 -> {Attrs, <<254:8/?UINT,Rest/binary>>}
    end.









