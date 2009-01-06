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
%%% get_new_pos(Pos, Direction) -> {NewPos, NewDirection}
%%% Pos = NewPos = {integer(), integer()}
%%% Direction = NewDirection = {integer(), integer()}
%%%-------------------------------------------------------------------
get_new_pos({X, Y}, Angle) ->
%%     Ok = {BallX, BallY} =
%% 	calculate_bounce({X, Y}, Direction),
%%     if Direction =:= {X,Y} ->
    Radian = Angle*(math:pi()/180),
    {{round(X2 = X + 5 * math:cos(Radian)),
      round(Y2 = Y + 5 * math:sin(Radian))}, Angle}.


%% calculate_bounce({BallX, BallY}, Angle) ->
%%     K = (BallY - DirY) / (BallX - DirX),
%%     %%X/(BallX-DirX),
%%     io:format("~p\n", [K]),
%%     {round((K*1)+BallX), round((K*1)+BallY)}.

