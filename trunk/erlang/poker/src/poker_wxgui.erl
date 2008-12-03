%%%-------------------------------------------------------------------
%%% File    : poker_wxgui.erl
%%% Author  :  <NTI@KLEPTO>
%%% Description : 
%%%
%%% Created : 26 Nov 2008 by  <NTI@KLEPTO>
%%%-------------------------------------------------------------------
-module(poker_wxgui).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include("../include/poker.hrl").
-define(RULES, 20).
-define(ABOUT, 21).

init() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "erlPoker"),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),

    %%Panel = wxPanel:new(Frame, []),
    
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

    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Opt, "Options"),
    wxMenuBar:append(MenuBar, Help, "Help"),

    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    MainSz = wxBoxSizer:new(?wxVERTICAL),

    W1 = wxWindow:new(Frame, ?wxID_ANY, []),
    wxWindow:connect(W1, paint, [{skip, true}]),

    DC = wxClientDC:new(Frame),

    Image = wxImage:new("../images/s1.png", [{type, ?wxBITMAP_TYPE_PNG}]),
    %%screenshot(#s{f=F,w1=#img{win=W1,image=Image},w2=GL,w3=#img{win=W3}}),

    wxWindow:show(Frame).
