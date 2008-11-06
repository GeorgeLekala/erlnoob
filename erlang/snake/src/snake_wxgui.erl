%%%-------------------------------------------------------------------
%%% File    : snake_wxgui.erl
%%% Author  :  <Olle@MUDKIPZ>
%%% Description : 
%%%
%%% Created : 19 Sep 2008 by  <Olle@MUDKIPZ>
%%%-------------------------------------------------------------------
-module(snake_wxgui).

%%-export([init/0]).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-record(state, {frame,
		canvas,
		deg,
		speed = 2.0}).

-define(NEW, 1).
-define(QUIT, 2 ).

-define(TRIVIAL, 10).
-define(EASY, 11).
-define(NORMAL, 12).
-define(HARD, 13).
-define(HARDEST, 14).

-define(RULES, 20).
-define(ABOUT, 21).


-define(VS, {{ 0.5,  0.5, -0.5},  %1
	     { 0.5, -0.5, -0.5},  %2
	     {-0.5, -0.5, -0.5},   
	     {-0.5,  0.5, -0.5},  %4
	     {-0.5,  0.5,  0.5},
	     { 0.5,  0.5,  0.5},  %6
	     { 0.5, -0.5,  0.5}, 
	     {-0.5, -0.5,  0.5}}).%8

-define(FACES, 
	%% Faces      Normal     U-axis     V-axis 
	[{{1,2,3,4},  { 0, 0,-1},  {-1,0, 0},  {0,1,0}},  % 
	 {{3,8,5,4},  {-1, 0, 0},  { 0,0, 1},  {0,1,0}},  %
	 {{1,6,7,2},  { 1, 0, 0},  { 0,0,-1},  {0,1,0}},  %
	 {{6,5,8,7},  { 0, 0, 1},  { 1,0, 0},  {0,1,0}},  %
	 {{6,1,4,5},  { 0, 1, 0},  {-1,0, 0},  {0,0,1}},  %
	 {{7,8,3,2},  { 0,-1, 0},  { 1,0, 0},  {0,0,1}}]).



init() ->
    Wx = wx:new(),
    {Frame, Canvas} = wx:batch(fun() -> create_window(Wx) end),
    loop(#state{frame = Frame,
		canvas = Canvas,
		deg = 1.0}).


create_window(Wx) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlSnake", []),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),

%%    Panel = wxPanel:new(Frame, []),
    
    %% Menu bar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Opt     = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?NEW,  "New Game"),
    wxMenu:appendSeparator(File),    
    wxMenu:append(File, ?QUIT, "&Quit Game"),

    wxMenu:append(Help, ?RULES, "Rules"),
    wxMenu:append(Help, ?ABOUT, "About"), 


    wxMenu:appendRadioItem(Opt, ?TRIVIAL, "Level: Trivial"),
    LItem = wxMenu:appendRadioItem(Opt, ?EASY, "Level: Easy"),
    wxMenuItem:check(LItem),
    wxMenu:appendRadioItem(Opt, ?NORMAL, "Level: Normal"),
    wxMenu:appendRadioItem(Opt, ?HARD, "Level: Hard"),
    wxMenu:appendRadioItem(Opt, ?HARDEST, "Level: Hardest"),

    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Opt, "Options"),
    wxMenuBar:append(MenuBar, Help, "Help"),

    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    MainSz = wxBoxSizer:new(?wxVERTICAL),

    Canvas = wxGLCanvas:new(Frame, [{size, {300, 200}},
				    {attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}]),

    wxSizer:add(MainSz, Canvas, []), 
    
    wxWindow:setSizer(Frame,MainSz),
    wxSizer:setSizeHints(MainSz,Frame),
    wxWindow:show(Frame),

    wxGLCanvas:setCurrent(Canvas),

    initGL(Frame, Canvas),
    {Frame, Canvas}.


loop(State) ->
    receive
	#wx{event = #wxClose{}} ->
	    wx:destroy();
	#wx{id = ?TRIVIAL} ->
	    loop(State#state{speed = 1.0});
	#wx{id = ?EASY} ->
	    loop(State#state{speed = 2.0});
	#wx{id = ?NORMAL} ->
	    loop(State#state{speed = 3.0});
	#wx{id = ?HARD} ->
	    loop(State#state{speed = 4.0});
	#wx{id = ?HARDEST} ->
	    loop(State#state{speed = 5.0});
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    after 20 ->
	    draw2(State#state.deg, {?FACES,?VS}),
	    wxGLCanvas:swapBuffers(State#state.canvas),
	    loop(State#state{deg = State#state.deg - State#state.speed})
    end.



initGL(Win,GLWin) ->
    {W,H} = wxWindow:getClientSize(GLWin),
    io:format("GL Window Size = ~p~n",[{W, H}]), 
    io:format("Window Size = ~p~n",[wxWindow:getClientSize(Win)]),
    gl:viewport(0, 0, W, H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    %%gl:frustum( -2.0, 2.0, -2.0, 2.0, 5.0, 25.0 ),
    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    {R,G,B,_} = wxWindow:getBackgroundColour(Win),
    gl:clearColor(R/255,B/255,G/255,1.0).


draw() ->
    gl:drawPixels(20, 20, ?GL_RGB, ?GL_BYTE, 0).


draw2(Deg,{Fs,Vs}) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:rotatef(Deg, -1.0, -1.0, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:'begin'(?GL_QUADS),    
    wx:foreach(fun(Face) -> drawFace(Face,Vs) end, Fs),
    gl:'end'().


drawFace({{V1,V2,V3,V4},N={N1,N2,N3},_Ut,_Vt}, Cube) ->
    gl:normal3fv(N),
    gl:color3f(abs(N1),abs(N2),abs(N3)),
    gl:texCoord2f(0.0, 1.0), gl:vertex3fv(element(V1, Cube)),
    gl:texCoord2f(0.0, 0.0), gl:vertex3fv(element(V2, Cube)),
    gl:texCoord2f(1.0, 0.0), gl:vertex3fv(element(V3, Cube)),
    gl:texCoord2f(1.0, 1.0), gl:vertex3fv(element(V4, Cube)).
