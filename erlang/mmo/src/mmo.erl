%%%-------------------------------------------------------------------
%%% File    : mmo.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created :  4 Jun 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(mmo).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

%% API
-export([start/0,start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).


-record(state, {frame,
		canvas,
		timer,
		image,
		pos,
		scale,
		size}).


start() ->
    wx_object:start(?MODULE, [], []).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Test", [{size, {800,648}}]),
    Attrs = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER]}],
    Canvas = wxGLCanvas:new(Frame, Attrs),
    init_frame(Frame),
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    
    {W,H} = wxGLCanvas:getSize(Canvas),
    gl:clearColor(0.2, 0.2, 0.2, 0.5),
    gl:enable(?GL_TEXTURE_2D),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:enable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA,?GL_ONE_MINUS_SRC_ALPHA),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    
    glu:ortho2D(0, W,H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),


    timer:send_interval(50,self(), repaint),
    wxGLCanvas:connect(Canvas, key_down),
    wxGLCanvas:connect(Canvas, left_down),
    wxGLCanvas:connect(Canvas, left_up),
    wxGLCanvas:connect(Canvas, size),
    wxWindow:setFocus(Canvas),
    self() ! repaint,
    {Frame, #state{canvas = Canvas, frame = Frame,
		  pos= {}, size = {70*8,20*6},scale = true}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(repaint, State) ->
    repaint(State#state.canvas, {200,200}),
    {noreply, State#state{}}.

handle_event(#wx{id = Id,event = #wxCommand{type = command_menu_selected}},
	     State) ->
    case Id of
	?wxID_EXIT ->
	    {stop, normal, State};
	_ ->
	    {noreply, State}
    end;
handle_event(#wx{event = #wxSize{size = {W,H}}}, State) ->
    gl:loadIdentity(),
    gl:viewport(0,0,W,H),
    glu:perspective(45,1.0*W/H,1,100),
    repaint(State#state.canvas, {200,200}),
    {noreply, State#state{}};
handle_event(#wx{event = #wxMouse{type = left_down}},
	     State = #state{}) ->
    wxWindow:connect(State#state.canvas, motion),
    {noreply, State#state{}};
handle_event(#wx{event = #wxMouse{type = left_up}}, State) ->
    wxWindow:disconnect(State#state.canvas, motion),
    {noreply, State#state{scale = false}};
handle_event(#wx{event = #wxMouse{type = motion}},
	     State = #state{scale = true}) ->
    {noreply, State#state{}};
handle_event(#wx{event = #wxKey{keyCode = Key}}, State) ->
    case Key of
	27 ->
	    {stop, normal, State};
	_ ->
	    {noreply, State}
    end;
handle_event(#wx{}, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    timer:cancel(State#state.timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

repaint(Canvas, {W,H}) ->
    TexId = load_image("../images/test.png", [{wrap_s, ?GL_REPEAT}]),
    IdMatrix = {0.08,1.0,1.0,1.0,
		1.0,1.0,1.0,1.0, 
		1.0,1.0,1.0,1.0,
		1.0,1.0,1.0,1.0},
    gl:clear(?GL_COLOR_BUFFER_BIT),
    Fun =
	fun({X,Y}) ->
		%%gl:pushMatrix(),
		gl:loadIdentity(),
		gl:enable(?GL_TEXTURE_2D),
		gl:bindTexture(?GL_TEXTURE_2D, TexId),		   
		gl:matrixMode(?GL_TEXTURE),
		gl:loadMatrixf(IdMatrix),
		gl:translatef(X*H,Y*H,0),
		gl:'begin'(?GL_QUADS),
		%%gl:color3f(0.0,1.0,0.0),
		gl:texCoord2f(0,0),
		gl:vertex2i(0,0),
		%%gl:color3f(0.0,0.0,1.0),
		gl:texCoord2f(W,0),
		gl:vertex2i(W,0),
		%%gl:color3f(1.0,0.0,0.0),
		gl:texCoord2f(W,H),
		gl:vertex2i(W,H),
		%%gl:color3f(1.0,1.0,1.0),
		gl:texCoord2f(0,H),
		gl:vertex2i(0,H),
		gl:'end'()
		%%gl:popMatrix()
	end,
    Fun({0.0,0.0}),
%%     wx:foreach(Fun, [{0,0},{1,0},{2,0},{3,0},{4,0},{5,0},{6,0},{7,0},
%% 		     {0,1},{1,1},{2,1},{3,1},{4,1},{5,1},{6,1},{7,1},
%%  		     {0,2},{1,2},{2,2},{3,2},{4,2},{5,2},{6,2},{7,2},
%%  		     {0,3},{1,3},{2,3},{3,3},{4,3},{5,3},{6,3},{7,3},
%%  		     {0,4},{1,4},{2,4},{3,4},{4,4},{5,4},{6,4},{7,4},
%% 		     {0,5},{1,5},{2,5},{3,5},{4,5},{5,5},{6,5},{7,5}]),

    wxGLCanvas:swapBuffers(Canvas).

    

init_frame(Frame) ->
    MB = wxMenuBar:new(),
    wxFrame:setMenuBar(Frame,MB),
    File = wxMenu:new([]),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    Edit = wxMenu:new([]),
    wxMenu:append(Edit, ?wxID_ANY, "Options"), 
    Help = wxMenu:new([]),
    wxMenu:append(Help, ?wxID_HELP, "Help"), 
    wxMenu:append(Help, ?wxID_ABOUT, "About"), 
    wxMenuBar:append(MB, File, "&File"),
    wxMenuBar:append(MB, Edit, "&Preferences"),
    wxMenuBar:append(MB, Help, "&Help"),
    wxFrame:connect(Frame, command_menu_selected),
    SB = wxFrame:createStatusBar(Frame,[]),
    wxStatusBar:setFieldsCount(SB,2,[]).


load_image(File) -> 
    load_image(File, []).
load_image(File, Options) ->
    Image = wxImage:new(File),
    true = wxImage:ok(Image),
    IW = wxImage:getWidth(Image),
    IH = wxImage:getHeight(Image),
    Data0 = wxImage:getData(Image),
    %% wxImage is upside/down for OpenGL
    RS = IW*3,
    Split = [Row || <<Row:RS/binary>> <= Data0],
    Data = << <<Row:RS/binary>> || Row <- lists:reverse(Split) >>,

    [Tid] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, Tid),
    Mag = proplists:get_value(mag_filter, Options, ?GL_LINEAR),
    Min = proplists:get_value(min_filter, Options, ?GL_LINEAR),
    WS  = proplists:get_value(wrap_s, Options, ?GL_CLAMP),
    WT  = proplists:get_value(wrap_t, Options, ?GL_CLAMP),    
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, Mag),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, Min),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, WS),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, WT),

    gl:texImage2D(?GL_TEXTURE_2D, 0, 3, IW, IH, 0, ?GL_RGB, 
		  ?GL_UNSIGNED_BYTE, Data),
    wxImage:destroy(Image),
    Tid.
