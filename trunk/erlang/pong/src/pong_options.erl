%%%-------------------------------------------------------------------
%%% File    : pong_options.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 18 Apr 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(pong_options).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include("pong.hrl").

-record(state, {main_pid,
		radio_box,
		frame}).

create_options_window(MainPid) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlPong - Options",
			[{size, {400,300}}]),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxFrame:connect(Frame, command_button_clicked, []),

    Panel = wxPanel:new(Frame, []),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    RadioBox = wxRadioBox:new(Panel, ?wxID_ANY, "Difficulty",
			      {-1,-1},{100,-1},["Trivial",
						"Easy",
						"Hard",
						"Insane"]),
    Button = wxButton:new(Panel, ?wxID_OK, [{label, "OK"}]),

    wxSizer:add(Sizer, RadioBox),
    wxSizer:add(Sizer, Button),

    wxSizer:add(MainSizer, Sizer),
    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(Frame),
    loop(#state{main_pid = MainPid,
		frame = Frame,
		radio_box = RadioBox}).


loop(State) ->
    receive
	#wx{id = ?wxID_OK,
	    event = #wxCommand{type = command_button_clicked}} ->
	    Selection = wxRadioBox:getSelection(State#state.radio_box),
	    SelString = wxRadioBox:getString(State#state.radio_box,Selection),
	    case SelString of
		"Trivial" ->
		    State#state.main_pid ! {start, 60, 5};
		"Easy" ->
		    State#state.main_pid ! {start, 45, 6};
		"Hard" ->
		    State#state.main_pid ! {start, 30, 7};
		"Insane" ->
		    State#state.main_pid ! {start, 15, 10};
		Any ->
		    io:format("No valid difficulty. ~p\n", [Any]),
		    State#state.main_pid ! {start, 60, 5}
	    end,
	    selected;
	#wx{event = #wxClose{}} ->
	    close;
	close ->
	    close;
	Any ->
	    io:format("~p\n", [Any]),
	    loop(State)
    end.
