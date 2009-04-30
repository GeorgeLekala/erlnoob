%%%-------------------------------------------------------------------
%%% File    : pong.hrl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 18 Apr 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------


-define(RECT_WIDTH, 400).
-define(RECT_HEIGHT, 300).

-define(RECT_POS_Y, 150).
-define(RECT_POS_X, 150).

-define(PEN_WIDTH, 10).

-define(PLAYER, 50).



-record(court, {rect_width	= ?RECT_WIDTH,
		rect_height	= ?RECT_HEIGHT,
		rect_x		= ?RECT_POS_X,
		rect_y		= ?RECT_POS_Y,
		pen_width	= ?PEN_WIDTH,
		player		= ?PLAYER,
		jump		= 9}).


-record(position, {x,
		   y,
		   angle = 45}).

