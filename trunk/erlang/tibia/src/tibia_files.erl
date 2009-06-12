%%%-------------------------------------------------------------------
%%% File    : tibia_files.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  7 Jun 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_files).

-compile(export_all).

-include("tibia.hrl").

-define(NODE_START, 254).
-define(NODE_END, 255).
-define(ESCAPE_CHAR, 253).


parse_file(Data) ->
    parse_file(Data, [],0).


parse_file(<<>>, Nodes,C) ->
    io:format("Count: ~p\n", [C]),
    lists:reverse(Nodes);
parse_file(<<Byte:8/?UINT,Rest/binary>>, Nodes,C) ->
    case Byte of
	?NODE_START ->
	    {Node, Rest2} = get_node(Rest),
	    parse_file(Rest2, [Node|Nodes],C);
	?NODE_END ->
	    parse_file(Rest, Nodes,C+1);
	?ESCAPE_CHAR ->
	    parse_file(Rest, Nodes, C);
	_ ->
	    parse_file(Rest, Nodes,C)
    
    end.

get_node(<<Type:8/?UINT,Rest/binary>>) ->
    get_node(#node{type = Type}, Rest, <<>>, []).

get_node(Node, <<>>, Acc, Nodes) ->
    {[Node#node{data = Acc}|Nodes], <<>>};
get_node(Node, <<Byte:8/?UINT,Rest/binary>>, Acc, Nodes) ->
    case Byte of
	?NODE_END ->
	    {Node#node{children = lists:reverse(Nodes),
		       data = Acc}, Rest};
	?NODE_START ->
	    {Node2, Rest3} = get_node(Rest),
	    get_node(Node,Rest3, Acc, [Node2|Nodes]);
	_ ->
	    get_node(Node,Rest,<<Acc/binary,Byte:8/?UINT>>, Nodes)
    end.

