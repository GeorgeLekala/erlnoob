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
get_new_pos({X, Y}, Angle) ->
    Radian = Angle*(math:pi()/180),
    if
	X>200; X<10 ->
	    X2 =
		if X > 200 -> X-20;
		   X < 10 ->  X+20
		end,
	    get_new_pos({X,Y},new_angle(Angle));
	Y>400; Y<10 ->
	    Y2 =
		if Y > 200 -> Y-20;
		   Y < 10 ->  Y+20
		end,
	    get_new_pos({X,Y},new_angle(Angle));
	X<200, X>10 ->
	    {{round(X2 = X + 5 * math:cos(Radian)),
	      round(Y2 = Y + 5 * math:sin(Radian))}, Angle};
	Y<400,Y>10 -> 
	    {{round(X2 = X + 5 * math:cos(Radian)),
	      round(Y2 = Y + 5 * math:sin(Radian))}, Angle}
    end.


new_angle(Angle) ->
%%     if
%% 	Angle =< 45 ->
%% 	    180-Angle;
%% 	Angle=<90,Angle>45 ->
    Max = 360,
    NewAngle = Max - Angle,
    NewAngle.



%% calculate_bounce({BallX, BallY}, Angle) ->
%%     K = (BallY - DirY) / (BallX - DirX),
%%     %%X/(BallX-DirX),
%%     io:format("~p\n", [K]),
%%     {round((K*1)+BallX), round((K*1)+BallY)}.

