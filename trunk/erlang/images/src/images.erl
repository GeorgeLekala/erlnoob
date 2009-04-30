%%%-------------------------------------------------------------------
%%% File    : images.erl
%%% Author  : Olle Mattsson <olle@snorlax>
%%% Description : 
%%%
%%% Created : 16 Apr 2009 by Olle Mattsson <olle@snorlax>
%%%-------------------------------------------------------------------
-module(images).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-define(H, 600).
-define(W, 800).
-define(NEW_IMAGE, 1001).

-compile(export_all).

-record(state, {frame,
		image,
		mouse,
		brush,
		pen,
		balls,
		timer}).

-record(image, {image,
		bmp,
		pos,
		width,
		height}).



start() ->
    State = init(),
    loop(State).

init() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Images", [{size, {?W, ?H}}]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),

    %% Menu bar
    MenuBar = wxMenuBar:new(),
    FileMenu    = wxMenu:new([]),

    wxMenu:append(FileMenu, ?wxID_NEW,  "New Image"),
    wxMenu:appendSeparator(FileMenu),    
    wxMenu:append(FileMenu, ?wxID_EXIT, "Exit Game"),

    wxMenuBar:append(MenuBar, FileMenu, "File"),

    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, motion),
    wxFrame:connect(Frame, left_down),
    wxFrame:connect(Frame, left_up),

    SB = wxFrame:createStatusBar(Frame,[{number,2}]),
    wxStatusBar:setStatusWidths(SB, [-1, 150]),

    
    wxFrame:show(Frame),

    {ok, Timer} = timer:send_interval(40, update),

    File = "../image/erlang.gif",
    Image = wxImage:new(File, [{type, ?wxBITMAP_TYPE_GIF}]),

    Width = wxImage:getWidth(Image),
    Height = wxImage:getHeight(Image),
    Brush = wxBrush:new(?wxBLACK, []),
    Pen = wxPen:new(?wxRED, [{width, 5}]),

    #state{frame = Frame,
	   timer = Timer,
	   mouse = {0,500},
	   brush = Brush,
	   pen   = Pen,
	   balls = [{50,169},{150,400}],
	   image = #image{image  = Image,
			  pos    = {{40+Width, $+}, {0, $+}},
			  bmp    = wxBitmap:new(Image),
			  width  = Width,
			  height = Height}}.

draw(State=#state{frame = Frame,
		  mouse = {X, _}}) ->
    CDC  = wxClientDC:new(Frame),
    DC = wxBufferedDC:new(CDC),
    wxDC:clear(DC),
    wxDC:setBrush(DC, State#state.brush),
    wxDC:setPen(DC, State#state.pen),
    wxDC:drawRectangle(DC, {X, 500}, {50, 10}),
    State2 = draw_image(DC, State),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(CDC),
    State2.
    
draw_balls(DC, Balls, Im) ->
    draw_balls(DC, Balls,Im, [], false).
draw_balls(DC, [{X, Y}|Balls], Im={ImX,ImW,ImH}, Acc, Result) ->
    if X >= ImX,
       X =< ImX+ImW,
       Y =< ImH ->
	    draw_balls(DC, Balls, Im, Acc, true);
	Y > 0 ->
	    wxDC:drawCircle(DC, {X,Y}, 5),
	    draw_balls(DC, Balls, Im, [{X,Y-40}|Acc], Result);
       true ->
	    draw_balls(DC, Balls, Im, Acc, Result)
    end;
draw_balls(_, [], _, Acc, Result) ->
    {Acc, Result}.

draw_image(DC, State = #state{image =Image,
			      balls = Balls}) ->
    {Image2, Balls2} = draw_image(DC, Image, Balls),
    State#state{image = Image2,
		balls = Balls2}.

draw_image(DC, Image= #image{pos = {{X, PrevOpX}, {Y, PrevOpY}},
			     width = W,height = H}, Balls) ->
    wxDC:drawBitmap(DC,Image#image.bmp, {X,Y}),
    NewX =
	if X =< 0 ->
		{X+4, $+};
	   X+Image#image.width > ?W-1;
	   PrevOpX =:= $- ->
		{X-4, $-};
	   X >= 0 ->
		{X+4, $+}
	end,
    case draw_balls(DC, Balls, {X,W,H}) of
	{Balls2, true} ->
	    {Image,Balls2};
	{Balls2, false} ->
	    {Image#image{pos = {NewX, {Y, PrevOpY}}},
	     Balls2}
    end.

loop(State=#state{}) ->
    receive
	update ->
	    State2 = draw(State),
	    loop(State2);
	#wx{id = ?wxID_NEW,
	    event = #wxCommand{}} ->
	    loop(State);
	#wx{event = #wxMouse{type = motion, x = X, y = Y}} ->
	    loop(State#state{mouse = {X, Y}});
	#wx{event = #wxMouse{type = left_up, x = X}} ->
	    loop(State#state{balls = [{X+20,500}|State#state.balls]});
	#wx{event = #wxMouse{type = left_down, x = X}} ->
	    loop(State#state{balls = [{X+20,500}|State#state.balls]});
	#wx{event = #wxPaint{}} ->
	    loop(State);
	#wx{event = #wxClose{}} ->
	    timer:cancel(State#state.timer),
	    close;
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    after 30 ->
	    {X,_Y} = State#state.mouse,
	    loop(State#state{balls = [{X+20,500}|State#state.balls]})
    end.

