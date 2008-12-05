%%%-------------------------------------------------------------------
%%% File    : image.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Test images (and read/write binaries)
%%%
%%% Created : 12 Jun 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(image).

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 

-export([start/0]).

-record(s,  {f,w1,w2,w3}).
-record(img,{win, image, bmp}).
-record(gl, {win, data, deg}).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx,1, "WxImage Test"),
    %% Menues
    Menu = wxMenu:new(),
    wxMenu:append(Menu, ?wxID_OPEN,  "Load Image"),
    wxMenu:append(Menu, ?wxID_ABOUT, "About"),
    wxMenu:append(Menu, ?wxID_EXIT,  "Quit"),
    MB = wxMenuBar:new(),
    wxMenuBar:append(MB,Menu,"File"),
    wxFrame:setMenuBar(Frame, MB),
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window, [{skip,true}]),
    wxFrame:createStatusBar(Frame,[]),
    %% Sub-Windows
    Opts = [{size, {300,300}}, {style, ?wxSUNKEN_BORDER}],
    W1 = wxWindow:new(Frame, ?wxID_ANY, Opts),
    wxWindow:connect(W1, paint, [{skip, true}]),
    Image   = wxImage:new("../images/s1.png", [{type, ?wxBITMAP_TYPE_PNG}]),
    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    W2 = wxGLCanvas:new(Frame,Opts ++ GLAttrib),
    W3 = wxWindow:new(Frame, ?wxID_ANY, Opts),
    wxWindow:connect(W3, paint, [{skip, true}]),
    %% Setup sizer
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
    wxSizer:add(Sz, W1, SF), 
    wxWindow:setSizer(F,Sz),
    wxSizer:setSizeHints(Sz,F),
    %% Show
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(W2),
    GL = setup_gl(W2,Image),
%%    drawBox(GL),
    io:format("Native Handles ~p ~p ~p~n", 
	      [wxWindow:getHandle(Frame),
	       wxWindow:getHandle(W1),
	       wxWindow:getHandle(W2)]),
    State = screenshot(#s{f=Frame,w1=#img{win=W1,image=Image},w2=GL,w3=#img{win=W3}}),
    loop(State, 0).
		
loop(S,C) ->
    receive 
	E=#wx{obj=Win,event=#wxPaint{}} ->
	    io:format("Got ~p~n", [E]),
	    S1 = redraw(Win,S),
	    loop(S1,C);
	_E=#wx{id=?wxID_ABOUT} ->
	    D = wxMessageDialog:new(wx:null(),"Image and binary testing", 
				    [{style,?wxICON_INFORMATION bor ?wxOK}]),
	    wxMessageDialog:showModal(D),
	    loop(S,C);
	E=#wx{id=EXIT, event=EV} when EXIT =:= ?wxID_EXIT; is_record(EV, wxClose) ->
	    io:format("Got ~p~n", [E]),
	    catch wxWindow:'Destroy'(S#s.f),
	    ok;
	Foo ->
	    io:format("Got ~p~n", [Foo]),
	    loop(S,C)
    end.

redraw_all(S0 = #s{w1=#img{win=W1},w3=#img{win=W3}}) ->
    S1 = redraw(W1, S0),
    redraw(W3, S1).

redraw(Win, S=#s{w1=Img0=#img{win=Win}}) ->
    Img = wx:batch(fun() -> redraw2(Img0, "") end),
    S#s{w1=Img};
redraw(_,S) ->S.

redraw2(Img = #img{image=undefined,bmp=undefined},_) -> Img;
redraw2(Img = #img{bmp=undefined,image=Image},Txt) -> 
    Bmp = wxBitmap:new(Image),
    redraw2(Img#img{bmp=Bmp},Txt);
redraw2(Img = #img{win=Win,bmp=Bmp},Txt) ->
    DC0  = wxClientDC:new(Win),
    DC = wxBufferedDC:new(DC0),
    wxDC:clear(DC),
    wxDC:drawText(DC, Txt, {10,10}),
    wxDC:drawBitmap(DC,Bmp, {10,30}),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    Img.

-define(VS, {{-0.5, -0.5, -0.5},  %1
	     { 0.5, -0.5, -0.5},  %2
	     { 0.5,  0.5, -0.5},   
	     {-0.5,  0.5, -0.5},  %4
	     {-0.5,  0.5,  0.5},
	     { 0.5,  0.5,  0.5},  %6
	     { 0.5, -0.5,  0.5}, 
	     {-0.5, -0.5,  0.5}}).%8

-define(FACES, 
	%% Faces    Normal     U-axis   V-axis 
	[{{1,2,3,4},{0,0,-1},{-1,0,0}, {0,1,0}},  % 
	 {{8,1,4,5},{-1,0,0},{0,0,1},  {0,1,0}},  %
	 {{2,7,6,3},{1,0,0}, {0,0,-1}, {0,1,0}},  %
	 {{7,8,5,6},{0,0,1}, {1,0,0},  {0,1,0}},  %
	 {{4,3,6,5},{0,1,0}, {-1,0,0}, {0,0,1}},  %
	 {{1,2,7,8},{0,-1,0},{1,0,0},  {0,0,1}}]).

update_rotation(S=#s{w2=GL=#gl{deg=Rot}}) ->
    S#s{w2=GL#gl{deg = Rot + 1.0}}.

screenshot(S=#s{w2=#gl{win=Win}, w3=W3=#img{image=OldImg,bmp=OldBmp}}) ->
    {W,H} = wxWindow:getClientSize(Win),
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    gl:readBuffer(?GL_FRONT),
    Mem = wx:create_memory(W*H*3),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin0 = wx:get_memory_bin(Mem),
    WSz = W*3,
    Im = [Row || <<Row:WSz/binary>> <= ImageBin0],
    ImageBin = list_to_binary(lists:reverse(Im)),
    NewImage = wxImage:new(W,H,ImageBin),
    case OldImg of 
	undefined -> ignore;
	_ -> wxImage:destroy(OldImg)
    end,
    case OldBmp of 
	undefined -> ignore;
	_ -> wxBitmap:destroy(OldBmp)
    end,
    S#s{w3=W3#img{image=NewImage,bmp=undefined}}.

%% Needs to setup opengl after window is shown...
%% GL context is created when shown first time.
setup_gl(Win, Image) ->
    [Tid] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, Tid),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    IW = wxImage:getWidth(Image),
    IH = wxImage:getHeight(Image),
    Data = wxImage:getData(Image),
    gl:texImage2D(?GL_TEXTURE_2D, 0, 3, IW, IH, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Data),
    gl:enable(?GL_TEXTURE_2D),
    #gl{win=Win,data={?FACES,?VS},deg=0.0}.