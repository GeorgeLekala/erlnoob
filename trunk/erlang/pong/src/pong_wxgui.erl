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

-define(OPTIONS, 100).

-record(state, {frame,		% The wxFrame() object
		canvas,		% The canvas to draw the scene on
		pen,		% The wxPen() object
		brush,		% The wxBrush() object
		player,		% The player's position
		ball_pos,	% The ball's bosition
		timer,		% The timer reference
		court,
		options_pid}).


create_main_window() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlPong", [{size, {800,600}}]),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxFrame:connect(Frame, command_menu_selected, []),
    wxFrame:connect(Frame, size, [{skip, true}]),


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

    wxMenu:append(Opt, ?OPTIONS, "Options"),

    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Opt, "Options"),
    wxMenuBar:append(MenuBar, Help, "Help"),

    wxFrame:setMenuBar(Frame, MenuBar),

    State = #state{frame = Frame,
		   canvas = Panel,
		   ball_pos = #position{x = {200, $-},
					y = {100, $+}},
		   player = 200,
		   court = #court{}},
    wxFrame:show(Frame),
    State2 = create_canvas(State),
    OptionsPid = spawn_link(pong_options, create_options_window, [self()]),
    loop(State2#state{options_pid = OptionsPid}).



create_canvas(State) ->
    Brush = wxBrush:new({200,200,200}, []),	       
    Pen   = wxPen:new(?wxBLACK, [{width, ?PEN_WIDTH}]),
    State2 = State#state{pen = Pen, brush = Brush},
    redraw(State2),
    State2.


loop(State) ->
    receive
	player_reached ->
	    %%Court = State#state.court,
	    State2 = State#state{},
	    loop(State2);
	{start, Interval, _Speed} ->
	    timer:cancel(State#state.timer),
	    {ok, Timer} = timer:send_interval(Interval, update),
	    loop(State#state{timer = Timer,
			     options_pid = undefined});
	update ->
	    NewPos = pong_logics:get_new_pos(State#state.court,
					     State#state.ball_pos),
	    State2 = State#state{ball_pos = NewPos},
	    redraw(State2),
	    loop(State2);
	#wx{event = #wxMouse{type = motion,y = Y}} ->
	    State2 = State#state{player = Y-25},
	    redraw(State2),
	    loop(State2);
	#wx{event = #wxSize{type = size,size = {X,Y}}} ->
	    Court = State#state.court,
	    Court2 = Court#court{rect_x = X div 4,
				 rect_y = Y div 4},
	    loop(State#state{court = Court2});
	#wx{id = ?OPTIONS,
	    event = #wxCommand{type = command_menu_selected}} ->
	    OptionsPid =
		case State#state.options_pid of
		    undefined ->
			spawn_link(pong_options,
				   create_options_window,
				   [self()]);
		    Pid ->
			Pid ! set_focus,
			Pid
		end,
	    loop(State#state{options_pid = OptionsPid});
	#wx{event = #wxPaint{}} ->
	    redraw(State),
	    loop(State);
	#wx{event = #wxClose{}} ->
	    timer:cancel(State#state.timer),
	    case State#state.options_pid of
		undefined ->
		    ignore;
		Pid ->
		    Pid ! close
	    end,
	    close;
	Any  ->
	    io:format("Got: ~p\n", [Any]),
	    loop(State)
    end.
    

redraw(State) ->
    CDC = wxClientDC:new(State#state.canvas),
    DC  = wxBufferedDC:new(CDC),
    wxDC:clear(DC),
    wxDC:setBrush(DC, State#state.brush),
    wxDC:setPen(DC, State#state.pen),
    borders(DC,State),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(CDC),
    ok.

borders(DC, State=#state{court = #court{rect_width = RectWidth,
					rect_height = RectHeight,
					rect_x = X,
					rect_y = Y}}) ->
    wxDC:drawRectangle(DC, {X,Y},
		       {RectWidth, RectHeight}),
    draw_ball(DC, State),
    draw_player(DC, State).

draw_ball(DC, #state{ball_pos = #position{x = {X,_},
					  y = {Y,_}}}) ->
    Pos = {X,Y},
    wxDC:drawCircle(DC, Pos, 5).

draw_player(DC, #state{player = PlayerY,
		       court = #court{rect_x = RectX,
				      rect_y = RectY,
				      rect_height = RectHeight,
				      pen_width = PenWidth,
				      player = Player}}) ->
    PlayerX = RectX+PenWidth+5,
    Pos =
	if PlayerY < RectY+PenWidth ->
		{PlayerX, RectY+PenWidth};
	   PlayerY > RectY+RectHeight-PenWidth-Player ->
		{PlayerX, RectY+RectHeight-PenWidth-Player};
	   true ->
		{PlayerX,PlayerY}
	end,
    wxDC:drawRectangle(DC, Pos, {5, Player}).



refresh_sizer(Frame, Panel, Sizer) ->
    wxSizer:layout(Sizer),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxWindow:refresh(Frame),
    wxWindow:update(Frame).
