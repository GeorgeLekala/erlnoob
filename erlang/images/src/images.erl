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
		timer}).

-record(image, {image,
		bmp,
		pos = {0,0},
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

    SB = wxFrame:createStatusBar(Frame,[{number,2}]),
    wxStatusBar:setStatusWidths(SB, [-1, 150]),

    
    wxFrame:show(Frame),

    {ok, Timer} = timer:send_interval(40, update),

    File = "../image/erlang.gif",
    Image = wxImage:new(File, [{type, ?wxBITMAP_TYPE_GIF}]),

    Width = wxImage:getWidth(Image),
    Height = wxImage:getHeight(Image),

    #state{frame = Frame,
	   timer = Timer,
	   image = #image{image  = Image,
			   pos    = {{40+Width, $+}, {510, $+}},
			   width  = Width,
			   height = Height}}.

draw(State=#state{frame = Frame,
		  image = Image}) ->
    CDC  = wxClientDC:new(Frame),
    DC = wxBufferedDC:new(CDC),
    wxDC:clear(DC),
    Image2 = draw_image(DC, Image),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(CDC),
    State#state{image=Image2}.
    

draw_image(DC, Img=#image{image = Image,
			  bmp = undefined}) ->
    Bmp = wxBitmap:new(Image),
    draw_image(DC, Img#image{bmp = Bmp});
draw_image(DC, Image= #image{pos = {{X, PrevOpX}, {Y, PrevOpY}}}) ->
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
    NewY =
	if Y =< 0 ->
		{Y+4, $+};
	   Y+Image#image.height > ?H-1;
	   PrevOpY =:= $- ->
		{Y-4, $-};
	   Y >= 0 ->
		{Y+4, $+}
	end,
    Image#image{pos = {NewX, NewY}}.

loop(State=#state{}) ->
    receive
	update ->
	    State2 = draw(State),
	    loop(State2);
	#wx{id = ?wxID_NEW,
	    event = #wxCommand{}} ->
	    loop(State);
	#wx{event = #wxMouse{}} ->
	    loop(State);
	#wx{event = #wxPaint{}} ->
	    loop(State);
	#wx{event = #wxClose{}} ->
	    timer:cancel(State#state.timer),
	    close;
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    end.

