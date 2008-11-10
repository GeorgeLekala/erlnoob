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
-include("snake.hrl").


-define(GRID_ID, 1).

-define(TRIVIAL, 10).
-define(EASY, 11).
-define(NORMAL, 12).
-define(HARD, 13).
-define(HARDEST, 14).

-define(RULES, 20).
-define(ABOUT, 21).


init() ->
    Wx = wx:new(),
    State = wx:batch(fun() -> create_window(Wx) end),
%%     Pid = spawn_link(fun() -> init_refresh_grid(State) end),
    {ok, Timer} = timer:send_interval(State#state.speed, State#state.main_window_pid, update),

    loop(State#state{timer = Timer}).


create_window(Wx) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlSnake", []),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),

    Panel = wxPanel:new(Frame, []),
    
    %% Menu bar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Opt     = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?wxID_NEW,  "New Game"),
    wxMenu:appendSeparator(File),    
    wxMenu:append(File, ?wxID_EXIT, "Exit Game"),

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

    State = create_grid(#state{panel = Panel,
			       frame = Frame,
			       main_sizer = MainSz}),

    StatusBar = wxFrame:createStatusBar(Frame,[]),
    wxFrame:setStatusText(State#state.frame, "Score: 0"),

    wxWindow:show(Frame),

    State#state{frame = Frame,
		panel = Panel,
		status_bar = StatusBar}.


create_grid(State = #state{panel = Panel,
		   frame = Frame,
		   main_sizer = MainSz}) ->
    Nrows = 10,
    Ncols = 10,
    Grid = wxGrid:new(Panel, ?GRID_ID, [{size, {-1, -1}}]),
    wxGrid:createGrid(Grid, Nrows, Ncols, []),
    [wxGrid:setColSize(Grid, Row, 17) || Row <- lists:seq(0, Ncols - 1)],
    [wxGrid:setRowSize(Grid, Col, 15) || Col <- lists:seq(0, Nrows - 1)],
    Borders = borders(Nrows, Ncols),
    colour_borders(Grid, Borders),

    wxGrid:setCellBackgroundColour(Grid, Nrows div 2, Ncols - (Ncols div 2), ?wxBLACK),
    wxGrid:setCellBackgroundColour(Grid, Nrows div 2, Ncols - (Ncols div 2) +1, ?wxBLACK),
    wxGrid:setCellBackgroundColour(Grid, Nrows div 2, Ncols - (Ncols div 2) -1, ?wxBLACK),

    {Row, Col} = snake_logics:get_random_apple(Nrows, Ncols),
    wxGrid:setCellBackgroundColour(Grid, Row, Col, ?wxRED),

    Click = fun(Type) -> wxGrid:connect(Grid, Type, [{callback, fun() -> ok end}]) end,
    lists:foreach(Click, [grid_cell_left_click, grid_cell_left_dclick,
 			  grid_cell_right_click, grid_cell_right_dclick,
			  grid_cell_begin_drag]),
    wxGrid:connect(Grid, key_down, []),
    wxGrid:enableEditing(Grid, false),
    wxGrid:setRowLabelSize(Grid, 0),
    wxGrid:setColLabelSize(Grid, 0),
    wxGrid:disableDragColSize(Grid),
    wxGrid:disableDragRowSize(Grid),
    Snake = #snake{head = [{Nrows div 2, Ncols - (Ncols div 2) -1}],
		   tail = [{Nrows div 2, Ncols - (Ncols div 2) +1},
			   {Nrows div 2, Ncols - (Ncols div 2)}]},
    wxSizer:add(MainSz, Grid),
    refresh_sizer(Frame, Panel, MainSz),
    State#state{grid = Grid,
		rows = Nrows,
		cols = Ncols,
		snake = Snake,
		direction = left,
		main_window_pid = self(),
		apple_pos = {Row, Col}}.

borders(Nrows, Ncols) ->
    List1 = [{0, Col} || Col <- lists:seq(0, Ncols -1)],
    List2 = [{Nrows -1, Col} || Col <- lists:seq(0, Ncols -1)],
    List3 = [{Row, 0} || Row <- lists:seq(0, Nrows -1)],
    List4 = [{Row, Ncols -1} || Row <- lists:seq(0, Nrows -1)],
    lists:flatten([List1, List2, List3, List4]).



colour_borders(Grid, Borders) ->
    lists:foreach(fun({Row, Col}) ->
			  wxGrid:setCellBackgroundColour(Grid, Row, Col, ?wxBLACK)
		  end,
		  Borders).

refresh_sizer(Frame, Panel, Sizer) ->
    wxSizer:layout(Sizer),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxWindow:refresh(Frame),
    wxWindow:update(Frame).


refresh_grid(State = #state{snake = Snake = #snake{head = Head = [{Row, Col} | _],
						   tail = [Tail| Tail2]}}) ->
    
    {Head2, WhadDidIEat, ApplePos} =
	case State#state.direction of
	    left ->
		wx:batch(fun() -> move_snake(State, {Row, Col -1}, Tail) end);
	    right ->
		wx:batch(fun() -> move_snake(State, {Row, Col +1}, Tail) end);
	    up ->
		wx:batch(fun() -> move_snake(State, {Row -1, Col}, Tail) end);
	    down ->
		wx:batch(fun() -> move_snake(State, {Row +1, Col}, Tail) end)
	end,
    wxWindow:refresh(State#state.frame),
    case WhadDidIEat of
	true ->
	    NewScore = State#state.score +1,
	    wxFrame:setStatusText(State#state.frame, "Score: " ++ integer_to_list(NewScore)),
	    State#state{snake = Snake#snake{head = [Head2 | Head],
					    tail = [Tail | Tail2]},
			score = NewScore,
			apple_pos = ApplePos};
	false ->
	    State#state{snake = Snake#snake{head = [Head2 | Head],
					    tail = Tail2},
			apple_pos = ApplePos};
	game_over ->
	    State
    end.    


move_snake(State = #state{grid = Grid, red = Red, white = White, black = Black},
	   Head = {RowHead, ColHead}, Tail = {RowTail, ColTail}) ->
    %%io:format("Head: ~p Tail: ~p\n", [Head, Tail]),
    {DidIEat, ApplePos} =
	case wxGrid:getCellBackgroundColour(Grid, RowHead, ColHead) of
	    Red ->
		%% Head
		wxGrid:setCellBackgroundColour(Grid, RowHead, ColHead, Black),
		{Row, Col} = next_apple(State),
		wxGrid:setCellBackgroundColour(Grid, Row, Col, Red),
		{true, {Row, Col}};
	    White ->
		%% Head
		wxGrid:setCellBackgroundColour(Grid, RowHead, ColHead, Black),
		%% Tail
		wxGrid:setCellBackgroundColour(Grid, RowTail, ColTail, White),
		{false, State#state.apple_pos};
	    Black ->
		State#state.main_window_pid ! game_over,
		{game_over, State#state.apple_pos}
	end,
    {Head, DidIEat, ApplePos}.
    
next_apple(State = #state{black = Black, grid = Grid}) ->
    {Row, Col} = snake_logics:get_random_apple(State#state.rows, State#state.cols),
    case wxGrid:getCellBackgroundColour(Grid, Row, Col) of
	Black ->
	    next_apple(State);
	_Any ->
	    {Row, Col}
    end.


loop(State) ->
    receive
	update ->
	    State2 = wx:batch(fun() -> refresh_grid(State) end),
	    loop(check_tail(State2#state{mode = available}));
	game_over ->
	    timer:cancel(State#state.timer),
	    Dialog = wxTextEntryDialog:new(State#state.frame, "Game Over!\n\nYour score:" ++
					   integer_to_list(State#state.score) ++
					   "\n\nEnter your name:"),
	    wxTextEntryDialog:show(Dialog),
	    loop(State#state{score = 0});
	#wx{event = #wxClose{}} ->
	    timer:cancel(State#state.timer),
	    wx:destroy();
	#wx{id = ?TRIVIAL} ->
	    State2 = new_game(State#state{speed = 300}),
	    loop(State2);
	#wx{id = ?EASY} ->
	    State2 = new_game(State#state{speed = 200}),
	    loop(State2);
	#wx{id = ?NORMAL} ->
	    State2 = new_game(State#state{speed = 120}),
	    loop(State2);
	#wx{id = ?HARD} ->
	    State2 = new_game(State#state{speed = 70}),
	    snake_logics:hard_mode(State2),
	    loop(State2);
	#wx{id = ?HARDEST} ->
	    State2 = new_game(State#state{speed = 50}),
	    loop(State2);
	#wx{id = ?wxID_EXIT} ->
	    timer:cancel(State#state.timer),
	    wx:destroy();
	#wx{id = ?wxID_NEW} ->
	    State2 = new_game(State),
	    loop(State2);
	#wx{event = #wxKey{keyCode = KeyCode}} ->
	    OldDirection = State#state.direction,
	    NewDirection2 =
		case State#state.mode of
		    available ->
			    if
				KeyCode =:= ?WXK_LEFT, OldDirection =/= right -> left;
				KeyCode =:= ?WXK_RIGHT, OldDirection =/= left -> right;
				KeyCode =:= ?WXK_UP, OldDirection =/= down -> up;
				KeyCode =:= ?WXK_DOWN, OldDirection =/= up -> down;
				KeyCode =:= 80 ->
				    State2 = snake_logics:pause_game(State),
				    loop(State2);
				KeyCode =:= 78 ->
				    State2 = new_game(State),
				    loop(State2);
				true -> OldDirection
			    end;
		    busy ->
			OldDirection
		end,		    
	    loop(State#state{direction = NewDirection2,
			     mode = busy});
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    end.


new_game(State) ->
    timer:cancel(State#state.timer),
    wxGrid:destroy(State#state.grid),
    State2 = create_grid(State),
    wxGrid:setFocus(State2#state.grid),
    {ok, Timer} = timer:send_interval(State2#state.speed, State2#state.main_window_pid, update),
    State2#state{timer = Timer}.    

check_tail(State = #state{snake = Snake}) ->
    [H | T] = Snake#snake.head,
    Snake2 =
	case Snake#snake.tail of
	    [] ->
		Snake#snake{tail = lists:reverse(T),
			    head = [H]};
	    Any when is_list(Any)->
		Snake
	end,
    State#state{snake = Snake2}.


