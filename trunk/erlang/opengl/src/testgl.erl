%%%-------------------------------------------------------------------
%%% File    : testgl.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 16 Mar 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(testgl).


-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl").
-include("ex1_parser.hrl").

-export([start/1]).

-record(gl, {win,
	     data,
	     material,
	     obj_ref
	    }).

-record(state, {gl
	       }).

start(Filename) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "GLTest", [{size, {800, 600}}]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxFrame:connect(Frame, paint, []),

    GLCanvas = wxGLCanvas:new(Frame, []),
    wxFrame:connect(GLCanvas, left_up, []),
    wxFrame:connect(GLCanvas, right_up, []),


    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(GLCanvas),

    {Data,Material,ObjectRef} = ex1_parser:parse(Filename),
    loop(#state{gl = #gl{win = GLCanvas,
			 data = Data,
			 material = Material,
			 obj_ref = ObjectRef
			}}).

loop(State) ->
    receive
	#wx{event = #wxPaint{}} ->
	    draw_scene(State#state.gl),
	    loop(State);
	#wx{event = #wxClose{}} ->
	    exit;
	#wx{event = #wxMouse{type = right_up}} ->
	    gl:scalef(1.5,1.5,1.5),
	    draw_scene(State#state.gl),
	    loop(State);
	#wx{event = #wxMouse{type = left_up}} ->
	    gl:scalef(0.5,0.5,0.5),
	    draw_scene(State#state.gl),
	    loop(State);
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    after 10 ->
	    draw_scene(State#state.gl), 
	    loop(State)
    end.
    

draw_scene(GL = #gl{}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:'begin'(?GL_LINES),
    gl:color3ub(255, 255, 255),
    gl:rotatef(0.8, 1.0, 1.0, 0.5),

    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(GL#gl.data),
		  GL#gl.data, ?GL_STATIC_DRAW),
    gl:vertexPointer(3, ?GL_FLOAT,  8*4, 0),
    gl:normalPointer(?GL_FLOAT,     8*4, 3*4),
    gl:texCoordPointer(2,?GL_FLOAT, 8*4, 6*4),
   
    %% The acutal Drawing code is
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
    wx:foreach(fun({Mat,First,Sz}) ->
		       %%set_mat(Mat,Mats),
		       gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
		       gl:drawArrays(?GL_TRIANGLES, First, Sz),
		       gl:bindBuffer(?GL_ARRAY_BUFFER,0)
	       end, GL#gl.obj_ref),
    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_NORMAL_ARRAY),
    gl:'end'(),
    wxGLCanvas:swapBuffers(GL#gl.win).

