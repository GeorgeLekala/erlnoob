%%%-------------------------------------------------------------------
%%% File    : pong_logics.erl
%%% Author  : Olle Mattsson <olle@mudkipz>
%%% Description : 
%%%
%%% Created :  5 Jan 2009 by Olle Mattsson <olle@mudkipz>
%%%-------------------------------------------------------------------
-module(pong_logics).

-compile(export_all).


%%%-------------------------------------------------------------------
%%% get_new_pos(Pos, Angle) -> {NewPos, Angle}
%%% Pos = NewPos = {integer(), integer()}
%%% Angle = integer()
%%%-------------------------------------------------------------------
get_new_pos({X, Y}, Angle, Direction) ->
    Radian = Angle*(math:pi()/180),
    Ok =if
	X>400; X<10 ->
	    NewPos =
		if
		    X<10  -> {X+10, Y};
		    X>400 -> {X-10, Y}
		end,
	    {NewAngle, NewDirection} = new_angle(Angle, Direction),
	    io:format("X\nOld: ~p\nNew: ~p\n", [Angle,NewAngle]),
	    get_new_pos(NewPos, NewAngle, NewDirection);
	Y>200; Y<10 ->
	    NewPos =
		if
		    Y>200 -> {X, Y-10};
		    Y<10  -> {X, Y+10}
		end,
	    {NewAngle, NewDirection} = new_angle(Angle, Direction),
	    io:format("Y\nOld: ~p\nNew: ~p\n", [Angle,NewAngle]),
	    get_new_pos(NewPos, NewAngle, NewDirection);
	X<200, X>10 ->
	    {{round(X2 = X + 5 * math:cos(Radian)),
	      round(Y2 = Y + 5 * math:sin(Radian))}, Angle, Direction};
	Y<400,Y>10 -> 
	    {{round(X2 = X + 5 * math:cos(Radian)),
	      round(Y2 = Y + 5 * math:sin(Radian))}, Angle, Direction}
    end,
%%    io:format("~p\n", [Ok]),
    Ok.


new_angle(Angle, {Direction, Wall}) ->
    case Direction of
	nw ->
	    case Wall of
		up ->   {Angle-90,  {sw, left}};
		left -> {Angle-540, {ne, up}}
	    end;
	sw ->
	    case Wall of
		left -> {180-Angle, {se, down}};
		down -> {Angle-360, {nw, left}}
	    end;
	se ->
	    case Wall of
		right -> {Angle-180, {sw, down}};
		down ->  {360-Angle, {ne, right}}
	    end;
	ne ->
	    case Wall of
		right -> {Angle-540, {nw, up}};
		up ->    {Angle-360, {se, right}}
	    end;
	true ->
	    io:format("Error", []),
	    exit(error)
    end.
	
%%     Max = 360,
%%     NewAngle = Max - Angle,
%%     NewAngle.



%% calculate_bounce({BallX, BallY}, Angle) ->
%%     K = (BallY - DirY) / (BallX - DirX),
%%     %%X/(BallX-DirX),
%%     io:format("~p\n", [K]),
%%     {round((K*1)+BallX), round((K*1)+BallY)}.

