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


-record(header, {major_version,
		 minor_version,
		 build_number,
		 csd_version}).

-record(item, {type,
	       flags,
	       attributes}).

-record(attributes, {id,
		     client_id,
		     speed,
		     light,
		     always_on_top}).

-define(FLAG_SIZE, 4).
-define(ATTR_SIZE, 1).
-define(DATALEN_SIZE, 2).

load() ->
    load("items.otb").

load(Filename) ->
    {ok, Device} = file:open(Filename, [read,raw,binary]),
    {_,Pos} = get_child_node(Device, 0),
    _Flags = pread_int(Device, Pos, ?FLAG_SIZE), 
    1 = pread_int(Device, Pos+?FLAG_SIZE, ?ATTR_SIZE),
    Pos2 = Pos+?FLAG_SIZE+?ATTR_SIZE,
    140 = pread_int(Device, Pos2, ?DATALEN_SIZE),
    Header = parse_header(pread(Device, Pos2+2, 140)),

    ChildNode = get_child_node(Device, Pos2+142),
    parse_items(Device, ChildNode).
	    
%%    io:format("~p\n", [pread(Device,Pos,40)]).



parse_items(Device, ChildNode) ->
    parse_items(Device, ChildNode, []).

parse_items(Device, {Type, Pos}, Items) when Pos < 1000 ->
    Flags = pread_int(Device, Pos, ?FLAG_SIZE), 
    Attr = pread_int(Device, Pos+4, ?ATTR_SIZE),
    Pos2 = Pos+5,
    {Attributes, Pos3} = parse_attributes(Device, Pos2+?FLAG_SIZE,
					  #attributes{}),
    parse_items(Device, Pos3,
		[#item{type = Type,
		       flags = pread_int(Device, Pos2,?FLAG_SIZE),
		       attributes = Attributes}|Items]);
parse_items(_,_,Items) ->
    Items.

parse_attributes(Device, Pos, A) ->
    Attr = pread_int(Device, Pos, ?ATTR_SIZE),
    DataLen = pread_int(Device, Pos+?ATTR_SIZE, ?DATALEN_SIZE),
    Pos2 = Pos+?ATTR_SIZE+?DATALEN_SIZE,
    case Attr of
	255 -> io:format("~p\n", [pread(Device, Pos2-1, 50)]),{A, get_child_node(Device, Pos)};
	?ITEM_ATTR_SERVERID ->
	    ServerID = pread_int(Device, Pos2, 2),
	    A2 = if ServerID > 20000,
		    ServerID < 20100 ->
			 A#attributes{id = ServerID - 20000};
		    true -> 
			 A#attributes{id = ServerID}
		 end,
	    parse_attributes(Device, Pos+2, A2);
	?ITEM_ATTR_CLIENTID ->
	    ClientID = pread_int(Device, Pos2, 2),
	    A2 = A#attributes{client_id = ClientID},
	    parse_attributes(Device, Pos2+2, A2);
	?ITEM_ATTR_SPEED ->
	    Speed = pread_int(Device, Pos2, 2),
	    A2 = A#attributes{speed = Speed},
	    parse_attributes(Device, Pos2+2, A2);
	?ITEM_ATTR_LIGHT2 ->
	    <<LightLevel:16/?UINT,
	     LightColor:16/?UINT>> = pread(Device,Pos2, 4),
	    A2 = A#attributes{light = {LightLevel, LightColor}},
	    parse_attributes(Device, Pos2+4, A2);
	?ITEM_ATTR_TOPORDER ->
	    V = pread_int(Device, Pos2, 1),
	    A2 = A#attributes{always_on_top = V},
	    parse_attributes(Device, Pos2+4, A2);
	_ ->
	    io:format("~p\n", [Attr]),
	    parse_attributes(Device, Pos2+DataLen, A)
    end.

    
    
parse_header(<<MajorVersion:32/?UINT,MinorVersion:32/?UINT,
	      BuildNumber:32/?UINT,CSDVersion:128/binary>>) ->
    #header{major_version = MajorVersion,
	    minor_version = MinorVersion,
	    build_number = BuildNumber,
	    csd_version = CSDVersion}.

get_child_node(Device, Pos) ->
    Pos2 = get_child_node_pos(Device, Pos),
    {pread_int(Device, Pos2, 1), Pos2+1}.

get_child_node_pos(Device, Pos) ->
    case pread_int(Device, Pos, 1) of
	254 ->
	    Pos+1;
	_ ->
	    get_child_node_pos(Device, Pos+1)
    end.

pread_int(Device, Pos, NumBytes) ->
    Bytes = NumBytes*8,
    {ok, <<Data:Bytes/?UINT>>} = file:pread(Device, Pos, NumBytes),
    Data.
pread(Device, Pos, NumBytes) ->
    {ok, Data} = file:pread(Device, Pos, NumBytes),
    Data.
