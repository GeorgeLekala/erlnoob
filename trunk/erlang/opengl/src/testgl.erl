%%%-------------------------------------------------------------------
%%% File    : ex2.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 17 Mar 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(testgl).

-compile(export_all).
%%-include("glf.hrl").
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-compile(inline).

-define(W, 800).
-define(H, 600).

-define(PAN_SPEED, 25).

-record(cam, 
	{origin,
	 old_origin,
	 distance,				% From origo.
	 azimuth,
	 elevation,
	 pan_x,					%Panning in X direction.
	 pan_y,					%Panning in Y direction.
	 fov,					%Field of view.
	 hither,				%Near clipping plane.
	 yon,					%Far clipping plane.
	 %% Not camera but needed
	 aspect,
	 ortho
	}).

%% -record(keys, {left,
%% 	       right,
%% 	       up,
%% 	       down
%% 	      }).

-record(state, {canvas,
		font,
		time,
		cam,
		type=dlo,
		angle={0.0,0.0},
		draw,
		mouse={{0,0},{0,0}},
		keys,
		objects = []
	       }).

-record(time, {fps=0,      % frames per second
	       fc=0,       % frame counter
	       diff=0,     % Time last frame in ms
	       prev=erlang:now(),
	       fcst=erlang:now()}).     % frame counter start time

start(ObjFile) ->
    State = initg(),
    Draw = load(ObjFile),
    loop(State#state{draw = Draw}).

loop(State = #state{cam=Cam}) ->
    NewState =
	receive
	    #wx{event = #wxClose{}} ->
		exit(normal);
	    
	    %% Mouse Events
	    #wx{event = #wxMouse{type=mousewheel, wheelRotation=WheelRot}} ->
		Distance=Cam#cam.distance,
		if
		    WheelRot/120 > 0 ->
			State#state{cam=Cam#cam{distance = Distance-1.5}};
		    true ->
			State#state{cam=Cam#cam{distance = Distance+1.5}}
		end;
	    #wx{event = #wxMouse{type = middle_down, x=X,y=Y}} ->
		wxGLCanvas:connect(State#state.canvas, motion, []),
		{OldMouse,_}=State#state.mouse,
		State#state{mouse = {{X,Y}, OldMouse}};
	    #wx{event = #wxMouse{type = middle_up, x=X,y=Y}} ->
		wxGLCanvas:disconnect(State#state.canvas, motion),
		{OldMouse,_}=State#state.mouse,
		State#state{mouse = {{X,Y}, OldMouse}};
	    #wx{event = #wxMouse{type = motion, x=X,y=Y}} ->
		{OldMouse,_}=State#state.mouse,
		State2 = State#state{mouse={{X,Y},OldMouse}},
		change_camera(State2);
	    #wx{event = #wxMouse{type = left_up}} ->
		State#state{};
	    #wx{event = #wxMouse{type = right_up}} ->
		State#state{};

	    %% Key Events
	    #wx{event = #wxKey{type = key_up,keyCode=Code}} ->
		State#state{};
	    #wx{event = #wxKey{type = key_down,keyCode=Code}} ->
		_OldOrigin = Cam#cam.old_origin,
		{Cam,Angle} = move_object(Cam,State#state.angle,Code),
		State#state{cam= Cam,angle=Angle};
	    Any ->
		io:format("~p\n", [Any]),
		State#state{}
	after 5 -> State#state{}
	end,
    loop(draw(NewState)).

move_object(M=#cam{origin=Origin={X,Y,Z},distance=Distance},Angle,Code) ->
    case Code of
	?WXK_UP ->
	    {M#cam{origin={X,Y,Z-5},
		   old_origin=Origin%,
		   %distance=Distance-5
		  },Angle};
	?WXK_DOWN ->
	    {M#cam{origin={X,Y,Z+5},
		   old_origin=Origin%,
		   %%distance=Distance+5
		  },Angle};
	?WXK_ESCAPE ->
	    exit(normal);
	?WXK_LEFT ->
	    {M,Angle+5};
	?WXK_RIGHT ->
	    {M,Angle-5};
	_ ->
	    {M,Angle}
    end.

change_camera(State=#state{mouse={{NewX,NewY},{OldX,OldY}},
			   angle ={XAngle0,YAngle0}}) ->
    X = (OldX - NewX),
    Y = (OldY - NewY),
    
    Angle = XAngle0+X,
    YAngle = YAngle0+Y,
    NewXAngle =
	if Angle > 360 -> Angle - 360;
	   Angle < 0   -> Angle + 360;
	   true -> Angle
	end,
    NewYAngle =
	if YAngle > 360 -> YAngle - 360;
	   YAngle < 0   -> YAngle + 360;
	   true -> YAngle
	end,
    State2 = State#state{angle = {NewXAngle,NewYAngle}},
    draw(State2),
    State2.


draw(S=#state{canvas=Canvas, cam=Cam,time=T,font=F,draw=Draw,
	      angle={XAngle,YAngle}}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    load_matrices(Cam, fun() -> lights() end),
    gl:rotated(XAngle, 0.0, 1.0, 0.0),
    gl:rotated(YAngle, 1.0, 0.0, 0.0),
    Draw(),
    wxGLCanvas:swapBuffers(Canvas),
    S#state{time=fps(T)}.

lights() ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.1,0.1,0.1,1.0}),
    gl:enable(?GL_LIGHT0),
    gl:enable(?GL_LIGHT1),
    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE,  {0.7,0.7,0.7,0.0}), 
    gl:lightfv(?GL_LIGHT0, ?GL_SPECULAR, {0.5,0.5,0.5,1}),
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.0,0.0,1.0,0.0}),
    gl:lightfv(?GL_LIGHT1, ?GL_DIFFUSE,  {0.5,0.5,0.5,0.5}), 
    gl:lightfv(?GL_LIGHT1, ?GL_SPECULAR, {0.3,0.3,0.3,1}),
    gl:lightfv(?GL_LIGHT1, ?GL_POSITION, {-0.71,-0.71,0.0,0.0}).


initg() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx,1,"TestGL",[{size, {?W,?H}}]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxFrame:center(Frame, []),
    wxFrame:setFocus(Frame),

    Attrs = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    GLCanvas = wxGLCanvas:new(Frame, Attrs),        
    wx:foreach(fun(Event) ->
		       wxEvtHandler:connect(GLCanvas, Event, [])
	       end, [left_up,
		     right_up,
		     mousewheel,
		     middle_down,
		     middle_up,
		     key_down,
		     key_up]),
    wxWindow:show(Frame),
    wxGLCanvas:setCurrent(GLCanvas),

    gl:viewport(0,0,?W,?H),
    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_CULL_FACE),
    
    gl:enable(?GL_LIGHTING),
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.2,0.2,0.9,1.0),
    
    DefFont = undefined,
    #state{canvas=GLCanvas,
	   font=DefFont,
	   time=#time{},
	   cam=init(?W,?H)}.



load(ObjF) ->
    ObjFile = 
	case file:read_file_info(ObjF) of
	    {error, _} ->
		Ext = case filename:extension(ObjF) of
			  [] -> ".wex1";
			  Else -> Else
		      end,
		Base = filename:basename(ObjF, Ext),
		Root = util:get_root_dir(),
		filename:join([Root,objs,Base++Ext]);
	    _File ->
		ObjF
	end,

    {DataChunk, Mats, ObjRefs} = ex1_parser:parse(ObjFile),
    [Buff,Buff2] = gl:genBuffers(2),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(DataChunk), DataChunk, ?GL_STATIC_DRAW),
    gl:vertexPointer(3, ?GL_FLOAT, 8*4, 0),
    gl:normalPointer(?GL_FLOAT, 8*4, 3*4),
    gl:texCoordPointer(2,?GL_FLOAT, 8*4, 6*4),
    Draw = fun() ->
		   gl:enableClientState(?GL_VERTEX_ARRAY),
		   gl:enableClientState(?GL_NORMAL_ARRAY),
		   gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
		   lists:foreach(fun({Mat,First,Sz}) ->
					 set_mat(Mat,Mats),
					 gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
					 gl:drawArrays(?GL_TRIANGLES, First, Sz),
					 gl:bindBuffer(?GL_ARRAY_BUFFER,0)
				 end, ObjRefs),
		   gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
		   gl:disableClientState(?GL_VERTEX_ARRAY),
		   gl:disableClientState(?GL_NORMAL_ARRAY)		   
	   end,
    Draw.
	  
set_mat(Mat,Mats) ->
    case lists:keysearch(Mat,1,Mats) of
	{value, {Mat,Diff,Amb,Spec,Emission,Shine,Maps}} -> 
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_DIFFUSE, Diff),
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_AMBIENT, Amb),
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_SPECULAR,Spec), 
	    gl:materialf(?GL_FRONT_AND_BACK, ?GL_SHININESS, Shine*128),
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, Emission),
	    ok;
	false ->
	    io:format("Couldn't find ~p in ~p~n", [Mat,Mats])
    end.




%%%%%%%%%%%% Camera 

init(W,H) ->
    #cam{origin    = {0.0,-10.0,0.0},
	 old_origin= {0.0,0.0,0.0},
	 azimuth   = -45.0,
	 elevation =  25.0,
	 distance  =  50,
	 pan_x     =  0.0,
	 pan_y     =  0.0,
	 fov       =  45.0,
	 hither    =  0.1,
	 yon       =  10000.0,
	 aspect    =  W/H,
	 ortho     =  false
	}.


load_matrices(Cam) ->
    load_matrices(Cam,false).
load_matrices(Cam,IncludeLights) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    projection(Cam),
    modelview(Cam, IncludeLights).

projection(#cam{distance=D,fov=Fov,hither=Hither,yon=Yon,
		aspect=Aspect,ortho=Ortho}) ->
    case Ortho of
	false ->
	    glu:perspective(Fov, Aspect, Hither, Yon);
	true ->
	    Sz = D*math:tan(Fov*math:pi()/180/2),
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
    end.

modelview(Cam) ->
    modelview(Cam,false).

modelview(#cam{origin={OX,OY,OZ},distance=Dist,azimuth=Az,
	       elevation=El,pan_x=PanX,pan_y=PanY},
	  Lights) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    if
	is_function(Lights) -> 	Lights();
	true -> ok
    end,
    gl:translatef(PanX, PanY, -Dist),
    gl:translatef(OX, OY, OZ),
    ok.





%%%%%%%%%

fps(T) ->   fps(T, 500).
fps(T0 = #time{fcst=Start,fc=FC,prev=Prev},Interval) ->
    Now = erlang:now(),
    Time = tdiff(Now,Start),
    Diff = tdiff(Now,Prev),
    if Time > Interval ->
	    Fps = round(1000*FC / Time),
	    #time{fps=Fps,diff=Diff,fcst=erlang:now(),prev=Now};
       true ->
	    T0#time{fc=FC+1,diff=Diff,prev=Now}
    end.

tdiff({A2,B2,C2},{A1,B1,C1}) ->
    (A2-A1)*1000000+(B2-B1)*1000 + (C2-C1) / 1000.
