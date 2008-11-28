%%%-------------------------------------------------------------------
%%% File    : snake_logics.erl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created :  9 Nov 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------
-module(snake_logics).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include("snake.hrl").



get_random_apple(Nrows, Ncols) ->
    {random:uniform(Nrows -2),
     random:uniform(Ncols -2)}.

pause_game(State) ->
    case State#state.timer of
	undefined ->
	    {ok, Timer} = timer:send_interval(State#state.speed,
					      State#state.main_window_pid, update),
	    State#state{timer = Timer};
	_Timer ->
	    timer:cancel(State#state.timer),
	    State#state{timer = undefined}
    end.
