%%%-------------------------------------------------------------------
%%% File    : pong_logics.erl
%%% Author  : Olle Mattsson <olle@mudkipz>
%%% Description : 
%%%
%%% Created :  5 Jan 2009 by Olle Mattsson <olle@mudkipz>
%%%-------------------------------------------------------------------
-module(pong_logics).

-compile(export_all).

-include("pong.hrl").

%%%-------------------------------------------------------------------
%%% get_new_pos(Pos, Angle) -> {NewPos, Angle}
%%% Pos = NewPos = {integer(), integer()}
%%% Angle = integer()
%%%-------------------------------------------------------------------
get_new_pos({{X, PrevOpX}, {Y, PrevOpY}}) ->
    NewX =
	if X =< ?RECT_POS_X+?PEN_WIDTH ->
		{X+7, $+};
	   X > ?RECT_WIDTH+?RECT_POS_X-?PEN_WIDTH;
	   PrevOpX =:= $- ->
		{X-7, $-};
	   true ->
		{X+7, $+}
	end,
    NewY =
	if Y =< ?RECT_POS_Y+?PEN_WIDTH ->
		{Y+7, $+};
	   Y > ?RECT_HEIGHT+?RECT_POS_Y-?PEN_WIDTH;
	   PrevOpY =:= $- ->
		{Y-7, $-};
	   true ->
		{Y+7, $+}
	end,
    {NewX, NewY}.



check_player({Bx,By}, PlayerPos) ->
    if By < PlayerPos+25,
       By > PlayerPos-25 ->
	    hit;
       Bx > ?RECT_POS_X+?PEN_WIDTH+20 ->
	    not_there;
       true ->
	    miss
    end.
