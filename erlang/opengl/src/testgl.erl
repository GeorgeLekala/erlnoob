%%%-------------------------------------------------------------------
%%% File    : image.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Test images (and read/write binaries)
%%%
%%% Created : 12 Jun 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(testgl).

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 

-export([start/0]).

-record(gl, {win,
	     deg}).

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "GLTest", [{size, {800, 600}}]),
    
    MenuBar = wxMenuBar:new(),
    File = wxMenu:new(),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    wxMenuBar:append(MenuBar, File, "File"),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected, []),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxFrame:connect(Frame, paint, []),

    GLCanvas = wxGLCanvas:new(Frame, []),
    wxFrame:connect(GLCanvas, left_up, []),


    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(GLCanvas),
    draw_scene(GLCanvas),

    loop(#gl{win = GLCanvas}).

loop(State) ->
    receive
	#wx{event = #wxPaint{}} ->
	    draw_scene(State#gl.win),
	    loop(State);
	#wx{event = #wxClose{}} ->
	    exit(shutdown);
	#wx{event = #wxMouse{}} ->
	    draw_scene(State#gl.win),
	    loop(State);
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    after 10 ->
	    gl:rotatef(0.2, 1.0, 1.0, 1.0),
	    draw_scene(State#gl.win), 
	    loop(State)
    end.
    

draw_scene(Win) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:'begin'(?GL_QUADS),
    gl:color3ub(255, 255, 255),
    gl:vertex3fv({ 0.5, -0.5, 0.0}),
    gl:vertex3fv({-0.5, -0.5, 0.0}),
    gl:vertex3fv({-0.5,  0.5, 0.0}),
    gl:vertex3fv({ 0.5,  0.5, 0.0}),

    gl:color3ub(255, 0, 255),
    gl:vertex3fv({ 0.0, 0.5, 0.0}),
    gl:vertex3fv({ 0.0, 0.5, 0.5}),
    gl:vertex3fv({ 0.5, 0.5, 0.5}),
    gl:vertex3fv({ 0.5, 0.5, 0.0}),
    gl:texCoord2f(0.0, 0.0),
    gl:'end'(),
    wxGLCanvas:swapBuffers(Win).

