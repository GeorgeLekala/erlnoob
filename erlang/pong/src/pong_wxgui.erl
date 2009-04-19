%%%-------------------------------------------------------------------
%%% File    : pong_wxgui.erl
%%% Author  : Olle Mattsson <olle@mudkipz>
%%% Description : 
%%%
%%% Created : 31 Dec 2008 by Olle Mattsson <olle@mudkipz>
%%%-------------------------------------------------------------------
-module(pong_wxgui).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include("pong.hrl").

-record(state, {frame,		% The wxFrame() object
		canvas,		% The canvas to draw the scene on
		pen,		% The wxPen() object
		brush,		% The wxBrush() object
		player,		% The player's position
		ball_pos,	% The ball's bosition
		timer}).	% The timer reference



create_main_window() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlPong", [{size, {800,600}}]),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),


    Panel = wxPanel:new(Frame, []),
    wxPanel:connect(Panel, paint, []),
    wxFrame:connect(Panel, motion, []),
    
    %% Menu bar
    MenuBar	= wxMenuBar:new(),
    File	= wxMenu:new([]),
    Opt		= wxMenu:new([]),
    Help	= wxMenu:new([]),

    wxMenu:append(File, ?wxID_NEW,  "New Game"),
    wxMenu:appendSeparator(File),    
    wxMenu:append(File, ?wxID_EXIT, "Exit Game"),

    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Opt, "Options"),
    wxMenuBar:append(MenuBar, Help, "Help"),

    wxFrame:setMenuBar(Frame, MenuBar),

    State = #state{frame = Frame,
		   canvas = Panel,
		   ball_pos = {{200, $-},{100, $+}},
		   player = 200},
    wxFrame:show(Frame),
    State2 = create_canvas(State),
    spawn_link(pong_options, create_options_window, [self()]),
    loop(State2).



create_canvas(State) ->
    Brush = wxBrush:new({200,200,200}, []),	       
    Pen   = wxPen:new(?wxBLACK, [{width, ?PEN_WIDTH}]),
    State2 = State#state{pen = Pen, brush = Brush},
    redraw(State2),
    State2.


loop(State) ->
    receive
	{start, Interval} ->
	    {ok, Timer} = timer:send_interval(Interval, update),
	    loop(State#state{timer = Timer});
	update ->
	    NewPos = pong_logics:get_new_pos(State#state.ball_pos),
	    State2 = State#state{ball_pos = NewPos},
	    io:format("~p\n~p\n", [pong_logics:check_player(State2#state.ball_pos,
							    State2#state.player),
				   State2#state.player]),
	    redraw(State2),
	    loop(State2);
	#wx{event = #wxMouse{type = motion,y = Y}} ->
	    State2 = State#state{player = Y-25},
	    redraw(State2),
	    loop(State2);
	#wx{event = #wxPaint{}} ->
	    loop(State);
	#wx{event = #wxClose{}} ->
	    timer:cancel(State#state.timer),
	    io:format("~p\n",[process_info(self(),[message_queue_len])]),
	    close;
	Wx = #wx{} ->
	    io:format("Got: ~p\n", [Wx]),
	    loop(State)
    end.
    

redraw(State=#state{ball_pos = OldBallPos,
		    player = PlayerPos}) ->
    CDC = wxClientDC:new(State#state.canvas),
    DC  = wxBufferedDC:new(CDC),
    wxDC:clear(DC),
    wxDC:setBrush(DC, State#state.brush),
    wxDC:setPen(DC, State#state.pen),
    borders(DC, OldBallPos, PlayerPos),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(CDC),
    ok.

borders(DC, BallPos, MousePos) ->
    wxDC:drawRectangle(DC, {?RECT_POS_X,?RECT_POS_Y},
		       {?RECT_WIDTH, ?RECT_HEIGHT}),
    draw_ball(DC, BallPos),
    draw_player(DC, MousePos).

draw_ball(DC, {{X,_}, {Y,_}}) ->
    wxDC:drawCircle(DC, {X,Y}, 6).

draw_player(DC, Y) ->
    wxDC:drawRectangle(DC, {?RECT_POS_X+?PEN_WIDTH+5, Y}, {5, 50}).



refresh_sizer(Frame, Panel, Sizer) ->
    wxSizer:layout(Sizer),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxWindow:refresh(Frame),
    wxWindow:update(Frame).
