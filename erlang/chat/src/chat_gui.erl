%%%-------------------------------------------------------------------
%%% File    : chat_gui.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 24 Apr 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(chat_gui).

-compile(export_all).

-include_lib("wx/include/wx.hrl").


-record(state, {main_pid,
		users,
		in_ctrl,
		out_ctrl,
		connect_pid}).

-define(CONNECT, 1001).



start() ->
    spawn(fun() -> chat_gui:init() end).

init() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlChat", [{size, {600,400}}]),
    wxFrame:connect(Frame, close_window, [{skip,true}]),
    wxFrame:connect(Frame, size, [{skip,true}]),

    create_menu(Frame),

    Panel = wxPanel:new(Frame, []),
    wxPanel:connect(Panel, command_button_clicked, []),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    MsgUserSizer = wxBoxSizer:new(?wxHORIZONTAL),

    IncomingMessages = wxTextCtrl:new(Panel, ?wxID_ANY, [{size, {500,300}},
							 {style, ?wxTE_MULTILINE bor
							  ?wxTE_READONLY}]),
    
    UserCtrl = wxListCtrl:new(Panel, [{size, {100,300}},
				      {style, ?wxLC_REPORT bor ?wxLC_NO_HEADER bor
				       ?wxLC_SINGLE_SEL}]),
    wxListCtrl:insertColumn(UserCtrl, 0, "Username", []),

    SendSizer = wxBoxSizer:new(?wxHORIZONTAL),
    Send = wxButton:new(Panel, ?wxID_ANY, [{label, "&Send"},
					   {size, {100,70}}]),


    OutgoingMessages = wxTextCtrl:new(Panel, ?wxID_ANY, [{size, {500,70}},
							 {style, ?wxTE_MULTILINE}]),
    wxSizer:add(MsgUserSizer, IncomingMessages, [{proportion, 5}]),
    wxSizer:add(MsgUserSizer, UserCtrl, [{proportion, 1}]),

    wxSizer:add(SendSizer, OutgoingMessages, [{proportion, 5}]),
    wxSizer:add(SendSizer, Send, [{proportion, 1}]),

    wxSizer:add(MainSizer, MsgUserSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, SendSizer, [{flag, ?wxEXPAND}]),


    wxPanel:setSizerAndFit(Panel, MainSizer),
    WindowSize = wxFrame:getSize(Frame),
    wxFrame:setSizeHints(Frame, WindowSize),
    wxFrame:show(Frame),

    loop(#state{users = UserCtrl,
		in_ctrl = IncomingMessages,
		out_ctrl = OutgoingMessages,
		main_pid = self()}).

loop(State) ->
    receive
	connect_close ->
	    ?MODULE:loop(State#state{connect_pid = undefined});
	#wx{event = #wxCommand{type = command_button_clicked}} ->
	    Message = wxTextCtrl:getValue(State#state.out_ctrl),
	    State#state.main_pid ! {message, "Pelle", Message},
	    wxTextCtrl:clear(State#state.out_ctrl),
	    ?MODULE:loop(State);
	{message, From, Msg} ->
	    Message = lists:flatten([From, ": ", Msg, "\n"]),
	    wxTextCtrl:appendText(State#state.in_ctrl, Message),
	    ?MODULE:loop(State);
	{connect, {Ip, Port}} ->
	    wxListCtrl:insertItem(State#state.users, 5001,
				  io_lib:format("{~p, ~p}", [Ip, Port])),
	    ?MODULE:loop(State#state{connect_pid = undefined});
	#wx{id = ?CONNECT} when State#state.connect_pid =:= undefined ->
	    Pid = spawn_link(chat_gui_connect, connect_window, [State#state.main_pid]),
	    ?MODULE:loop(State#state{connect_pid = Pid});
	#wx{id = ?CONNECT} ->
	    ?MODULE:loop(State);
	#wx{event = #wxClose{}} ->
	    exit(close);	    
	#wx{id = ?wxID_EXIT} ->
	    exit(close);
	#wx{event = #wxSize{size = {W, _H}}} ->
	    wxListCtrl:setColumnWidth(State#state.users, 0, W div 6),
	    %%wxListCtrl:deleteAllItems(State#state.users),
	    loop(State);
	Any ->
	    io:format("Any: ~p\n", [Any]),
	    ?MODULE:loop(State)
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


create_menu(Frame) ->
    wxFrame:connect(Frame, command_menu_selected, []),
    MenuBar	= wxMenuBar:new(),
    File	= wxMenu:new([]),
    Opt		= wxMenu:new([]),
    Help	= wxMenu:new([]),

    wxMenu:append(File, ?CONNECT, "Connect"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?wxID_EXIT, "Exit"),
    
    wxMenu:append(Opt, ?wxID_ANY, "Options"),
    wxMenu:append(Help, ?wxID_ABOUT, "About"),
	    
    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Opt, "Options"),
    wxMenuBar:append(MenuBar, Help, "Help"),

    wxFrame:setMenuBar(Frame, MenuBar),
    MenuBar.
