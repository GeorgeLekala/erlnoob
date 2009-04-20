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

get_new_pos(Court, OldPos) ->
    new_pos(Court, OldPos).



new_pos(#court{rect_width = RectWidth,
	       rect_height = RectHeight,
	       rect_x = RectX,
	       rect_y = RectY,
	       jump = Jump,
	       pen_width = PenWidth},
	Pos=#position{x = {X, PrevOpX},
		      y = {Y, PrevOpY}}) ->
    NewX =
	if X =< RectX+PenWidth+23 ->
		Me = self(),
		Me ! player_reached,
		{X+Jump, $+};
	   X > RectWidth+RectX-PenWidth-23;
	   PrevOpX =:= $- ->
		{X-Jump, $-};
	   true ->
		{X+Jump, $+}
	end,
    NewY =
	if Y =< RectY+PenWidth ->
		{Y+Jump, $+};
	   Y > RectHeight+RectY-PenWidth;
	   PrevOpY =:= $- ->
		{Y-Jump, $-};
	   true ->
		{Y+Jump, $+}
	end,
    Pos#position{x = NewX,
		 y = NewY}.



get_new_pos2(#court{jump = Jump},
	     Pos = #position{x = X, y = Y,
			     angle = Angle}) ->
    Radian = Angle*(math:pi()/180),
    NewAngle = if Angle =< 0 ->
		       360 - Angle;
		  true ->
		       Angle
	       end,
    NewX = round(X + Jump * math:cos(Radian)),
    NewY = round(Y + Jump * math:sin(Radian)),
    Pos#position{x = NewX, y = NewY,
		 angle = NewAngle}.

