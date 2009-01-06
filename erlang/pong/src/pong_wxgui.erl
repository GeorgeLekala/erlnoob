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

-record(state, {frame,	% The frame object
		panel,
		canvas,
		borders,
		pen,
		brush,
		ball_pos = {60,60},
		direction = 300,
		timer
	       }).

create_window() ->
    Wx = wx:new(),
    %%wx:debug(2),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlPong", [{size, {500,400}}]),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),


    Panel = wxPanel:new(Frame, []),
    wxPanel:connect(Panel, paint, []),
    
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
		   canvas = Panel},
    wxFrame:show(Frame),
    State2 = create_canvas(State),
    Me = self(), 
    {ok, Timer} = timer:send_interval(70, Me, update),

    loop(State2#state{timer = Timer}).


create_canvas(State = #state{canvas = Panel}) ->
    Brush = wxBrush:new({200,200,200}, []),	       
    Pen   = wxPen:new(?wxBLACK, [{width, 10}]),
    CDC = wxClientDC:new(Panel),
    DC  = wxBufferedDC:new(CDC),
    borders(DC, Brush, Pen, {State#state.ball_pos, State#state.direction}),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(CDC),
    State#state{pen = Pen, brush = Brush}.


loop(State) ->
    receive
	#wx{event = #wxPaint{}} ->
	    {NewPos, Direction} = redraw(State),
	    loop(State#state{ball_pos = NewPos,
			     direction = Direction});
	#wx{event = #wxClose{}} ->
	    timer:cancel(State#state.timer),
	    io:format("~p\n",[process_info(self(),[message_queue_len])]),
	    wx:destroy();
	Wx = #wx{} ->
	    io:format("Got: ~p\n", [Wx]),
	    loop(State);
	update ->
	    {NewPos, Direction} = redraw(State),
	    loop(State#state{ball_pos = NewPos,
			     direction = Direction})
    end.
    

redraw(State) ->
    CDC = wxClientDC:new(State#state.canvas),
    DC  = wxBufferedDC:new(CDC),
    wxDC:clear(DC),
    PosNDir = borders(DC, State#state.brush,
		     State#state.pen,
		     {State#state.ball_pos,
		      State#state.direction}),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(CDC),
    PosNDir.

borders(DC, Brush, Pen, {OldPos, Direction}) ->
    wxDC:setBrush(DC, Brush),
    wxDC:setPen(DC, Pen),
    wxDC:drawRectangle(DC, {5,5}, {405, 205}),
    {NewPos, NewDirection} = pong_logics:get_new_pos(OldPos, Direction),
    draw_ball(DC, NewPos),
    {NewPos, NewDirection}.

draw_ball(DC, Coord) ->
    wxDC:drawCircle(DC, Coord, 2),
    Coord.

refresh_sizer(Frame, Panel, Sizer) ->
    wxSizer:layout(Sizer),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxWindow:refresh(Frame),
    wxWindow:update(Frame).
