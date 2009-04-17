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

-record(state, {canvas,
		image}).

-record(image, {data,
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

    
    Attrs = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    GLCanvas = wxGLCanvas:new(Frame, Attrs),
    wxGLCanvas:connect(GLCanvas, paint),
    
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(GLCanvas),


    gl:viewport(0,0,?W,?H),
    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_CULL_FACE),
    
    gl:enable(?GL_LIGHTING),
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(1,0.3,0.3,0.5),

    File = "../image/erlang.gif",
    Image = wxImage:new(File, [{type, ?wxBITMAP_TYPE_GIF}]),

    #state{canvas = GLCanvas,
	   image = #image{data   = wxImage:getData(Image),
			  width  = wxImage:getWidth(Image),
			  height = wxImage:getHeight(Image)}}.

draw(#state{canvas = Canvas,
	    image = #image{data   = Data,
			   width  = W,
			   height = H}}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    gl:texImage2D(?GL_TEXTURE_2D, 0, 3, W, H, 5, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Data),
    wxGLCanvas:swapBuffers(Canvas).
    

loop(State=#state{image=Image}) ->
    receive
	#wx{id = ?wxID_NEW,
	    event = #wxCommand{}} ->
	    draw(State),
	    io:format("Width: ~p\nHeight: ~p\n", [Image#image.width,Image#image.height]),
	    loop(State);
	#wx{event = #wxPaint{}} ->
	    draw(State),
	    loop(State);
	#wx{event = #wxClose{}} ->
	    exit(close);
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    after 100 ->
	    draw(State),
	    loop(State)
    end.

