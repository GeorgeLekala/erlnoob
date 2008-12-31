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
		direction = left
	       }).

create_window() ->
    Wx = wx:new(),
    %%wx:debug(2),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlPong", []),
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
		   panel = Panel},
    wxFrame:show(Frame),
    State2 = create_canvas(State),

    loop(State2).


create_canvas(State = #state{panel = Panel}) ->
    CDC = wxClientDC:new(Panel),
    Brush = wxBrush:new({200,200,200}, []),	       
    Pen   = wxPen:new(?wxBLACK, [{width, 10}]),
    _State2 = borders(State#state{pen = Pen, brush = Brush,
				  canvas = CDC}).


loop(State) ->
    receive
	#wx{event = #wxPaint{}} ->
	    loop(borders(State));
	#wx{event = #wxClose{}} ->
	    wx:destroy();
	Wx = #wx{} ->
	    io:format("Got: ~p\n", [Wx]),
	    loop(State)
    after 
	100 ->
	    loop(borders(State))
    end.
    
borders(State = #state{panel = Panel,
		       brush = Brush,
		       pen = Pen,
		       canvas = CDC,
		       ball_pos = OldPos,
		       direction = Direction}) ->
    DC = wxBufferedDC:new(CDC),
    wxDC:setBrush(DC, Brush),
    wxDC:setPen(DC, Pen),
    wxDC:drawRectangle(DC, {5,5}, {200, 100}),
    {NewPos, NewDirection} = get_new_pos(OldPos, Direction),
    draw_ball(DC, NewPos),
    wxBufferedDC:destroy(DC),
    State#state{ball_pos = NewPos,
		direction = NewDirection}.

get_new_pos({X, Y}, Direction) ->
    case Direction of
	left ->
	    if
		X < 20  -> {{X+5, Y}, right};
		X > 185 -> {{X-5, Y}, left};
		true -> {{X-5, Y}, Direction}
	    end;
	right ->
	    if
		X < 15  -> {{X+5, Y}, right};
		X > 185 -> {{X-5, Y}, left};
		true -> {{X+5, Y}, Direction}
	    end
    end.

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
