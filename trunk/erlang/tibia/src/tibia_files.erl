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


p() ->
    {ok, Data} = file:read_file("test.otbm"),
    parse(Data).

p(Filename) ->
    {ok, Data} = file:read_file(Filename),
    parse(Data).

parse(Data) ->
    try parse_file(Data)
    catch
	throw:Reason ->
	    {error, Reason}
    end.


parse_file(<<Version:32/?UINT,?NODE_START:8/?UINT,Type:1/binary,Rest/binary>>) ->
    if Version > 0 ->
	    throw(wrong_version);
       true ->
	    parse_nodes(Rest, #node{type=Type})
	    
    end.
    




parse_nodes(Data, Node) ->
    {<<>>, Nodes} = parse_nodes(Data, Node, []),
    Nodes.

parse_nodes(<<>>, Node, Nodes) ->
    {<<>>, Node#node{children = lists:reverse(Nodes)}};
parse_nodes(<<?ESCAPE_CHAR:8/?UINT,_,Rest/binary>>, Node, Nodes) ->
    parse_nodes(Rest, Node,Nodes);
parse_nodes(<<?NODE_END:8/?UINT,Rest/binary>>, Node, Nodes) ->
    {Rest, Node#node{children = lists:reverse(Nodes)}};
parse_nodes(<<?NODE_START:8/?UINT,Type:1/binary, Rest/binary>>, Node, Nodes) ->
    {Rest2, Node2} = parse_nodes(Rest, #node{type=Type}, []),
    parse_nodes(Rest2, Node, [Node2|Nodes]);
parse_nodes(<<Byte, Rest/binary>>, Node=#node{data=Data}, Nodes) ->
    parse_nodes(Rest, Node#node{data = <<Data/binary,Byte>>}, Nodes).


count_nodes(File) when is_list(File) ->
    {ok, Data} = file:read_file(File),
    count_nodes(Data);
count_nodes(Data) when is_binary(Data) ->
    count_nodes(Data, 0).

count_nodes(<<>>, NumNodes) ->
    NumNodes;
count_nodes(<<?NODE_START:8/?UINT, Rest/binary>>, NumNodes) ->
    count_nodes(Rest, NumNodes+1);
count_nodes(<<_,Rest/binary>>, NumNodes) ->
    count_nodes(Rest, NumNodes).


gen_file(Data) ->
    <<0:32/?UINT,(gen_file(Data,<<>>,0))/binary>>.

gen_file([], Index, Counter) ->
    Index;
gen_file([#node{type = Type,
		data = Data,
		children = Children}|Nodes], Index, Counter) ->
    Children2 = gen_file(Children, <<>>, 0),
    gen_file(Nodes,
	     <<Index/binary,?NODE_START:8/?UINT,Type/binary,
	      Data/binary,Children2/binary,?NODE_END:8/?UINT>>,
	     Counter+1).
