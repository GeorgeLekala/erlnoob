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
get_new_pos(Court) ->
    new_pos(Court).



new_pos(#court{rect_width = RectWidth,
		   rect_height = RectHeight,
		   rect_x = RectX,
		   rect_y = RectY,
		   x_dir = XDir,
		   y_dir = YDir,
		   pen_width = PenWidth},
	    {{X, PrevOpX}, {Y, PrevOpY}}) ->
    NewX =
	if X =< RectX+PenWidth ->
		{X+XDir, $+};
	   X > RectWidth+RectX-PenWidth;
	   PrevOpX =:= $- ->
		{X-XDir, $-};
	   true ->
		{X+XDir, $+}
	end,
    NewY =
	if Y =< RectY+PenWidth ->
		{Y+YDir, $+};
	   Y > RectHeight+RectY-PenWidth;
	   PrevOpY =:= $- ->
		{Y-YDir, $-};
	   true ->
		{Y+YDir, $+}
	end,
    {NewX, NewY}.



