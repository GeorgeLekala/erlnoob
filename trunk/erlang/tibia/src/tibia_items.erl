%%%-------------------------------------------------------------------
%%% File    : tibia_items.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  3 Jun 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_items).

-compile(export_all).

-include("tibia.hrl").

-define(ROOT_ATTR_VERSION, 16#01).

-define(ITEM_GROUP_NONE, 0).
-define(ITEM_GROUP_GROUND, 1).
-define(ITEM_GROUP_CONTAINER, 2).
-define(ITEM_GROUP_WEAPON, 3).
-define(ITEM_GROUP_AMMUNITION, 4).
-define(ITEM_GROUP_ARMOR, 5).
-define(ITEM_GROUP_CHARGES, 6).
-define(ITEM_GROUP_TELEPORT, 7).
-define(ITEM_GROUP_MAGICFIELD, 8).
-define(ITEM_GROUP_WRITEABLE, 9).
-define(ITEM_GROUP_KEY, 10).
-define(ITEM_GROUP_SPLASH, 11).
-define(ITEM_GROUP_FLUID, 12).
-define(ITEM_GROUP_DOOR, 13).
-define(ITEM_GROUP_DEPRECATED, 14).
-define(ITEM_GROUP_LAST, 15).


-define(ITEM_ATTR_FIRST, 16#10).
-define(ITEM_ATTR_SERVERID,16#10).
-define(ITEM_ATTR_CLIENTID,16#11).
-define(ITEM_ATTR_NAME,16#12).
-define(ITEM_ATTR_DESCR,16#13).
-define(ITEM_ATTR_SPEED,16#14).
-define(ITEM_ATTR_SLOT,16#15).
-define(ITEM_ATTR_MAXITEMS,16#16).
-define(ITEM_ATTR_WEIGHT,16#17).
-define(ITEM_ATTR_WEAPON,16#18).
-define(ITEM_ATTR_AMU,16#19).
-define(ITEM_ATTR_ARMOR,16#1A).
-define(ITEM_ATTR_MAGLEVEL,16#1B).
-define(ITEM_ATTR_MAGFIELDTYPE,16#1C).
-define(ITEM_ATTR_WRITEABLE,16#1D).
-define(ITEM_ATTR_ROTATETO,16#1E).
-define(ITEM_ATTR_DECAY,16#1F).
-define(ITEM_ATTR_SPRITEHASH,16#20).
-define(ITEM_ATTR_MINIMAPCOLOR,16#21).
-define(ITEM_ATTR_07,16#22).
-define(ITEM_ATTR_08,16#23).
-define(ITEM_ATTR_LIGHT,16#24).

-define(ITEM_ATTR_DECAY2,16#25).
-define(ITEM_ATTR_WEAPON2,16#26).
-define(ITEM_ATTR_AMU2,16#27).
-define(ITEM_ATTR_ARMOR2,16#28).
-define(ITEM_ATTR_WRITEABLE2,16#29).
-define(ITEM_ATTR_LIGHT2,16#2A).

-define(ITEM_ATTR_TOPORDER,16#2B).

-define(ITEM_ATTR_WRITEABLE3,16#2C).

-define(ITEM_ATTR_LAST,16#2D).

-record(item_type, {type,
		    server_id,
		    client_id,
		    speed,
		    light_level,
		    light_color,
		    always_on_top_order}).

-define(FLAG_SIZE, 4).
-define(ATTR_SIZE, 1).
-define(DATALEN_SIZE, 2).

test() ->
    load("items.otb").


load(File) ->
    Node = tibia_files:parse(File),
    parse_items(Node).



parse_items(#node{type = 0,
		  data = <<_:32/?UINT,
			  ?ROOT_ATTR_VERSION,
			  140:16/?UINT,
			  _MajorVersion:32/?UINT,
			  _MinorVersion:32/?UINT,
			  _BuildNumber:32/?UINT,
			  _CSDVersion:128/binary>>,
		  children = Children}) ->
    parse_items(Children, []).

parse_items([#node{type = Type,
		   data = <<Flags:32/?UINT,Rest/binary>>,
		   children = Children} | Nodes], Items) ->
    parse_items(Nodes, [get_attributes(Rest, #item_type{type=Type})|Items]);
parse_items([], Items) ->
    lists:reverse(Items).


get_attributes(<<>>, Item) ->
    Item;
get_attributes(<<?ITEM_ATTR_SERVERID,2:16/?UINT,ServerId:16/?UINT,Rest/binary>>, ItemType) ->
    get_attributes(Rest,ItemType#item_type{server_id = ServerId});
get_attributes(<<?ITEM_ATTR_CLIENTID,2:16/?UINT,ClientId:16/?UINT,Rest/binary>>, ItemType) ->
    get_attributes(Rest,ItemType#item_type{client_id = ClientId});
get_attributes(<<?ITEM_ATTR_SPEED,2:16/?UINT,Speed:16/?UINT,Rest/binary>>, ItemType) ->
    get_attributes(Rest,ItemType#item_type{speed = Speed});
get_attributes(<<?ITEM_ATTR_LIGHT2,4:16/?UINT,
		LightLevel:16/?UINT,
		LightColor:16/?UINT,
		Rest/binary>>, ItemType) ->
    get_attributes(Rest,ItemType#item_type{light_color = LightColor,
					   light_level = LightLevel});
get_attributes(<<?ITEM_ATTR_TOPORDER,1:16/?UINT,TopOrder,Rest/binary>>, ItemType) ->
    get_attributes(Rest,ItemType#item_type{always_on_top_order = TopOrder});
get_attributes(<<_,Len:16/?UINT,_:Len/binary,Rest/binary>>, ItemType) ->
    get_attributes(Rest,ItemType).





