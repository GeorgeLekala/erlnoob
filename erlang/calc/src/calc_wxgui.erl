%%%-------------------------------------------------------------------
%%% File    : calc_wxgui.erl
%%% Author  : Olle Mattsson <olle@mudkipz>
%%% Description : 
%%%
%%% Created : 11 Sep 2008 by Olle Mattsson <olle@mudkipz>
%%%-------------------------------------------------------------------
-module(calc_wxgui).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

start() ->
    Wx = wx:new(),
    create_window(Wx),
    ok.

create_window(Wx) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Calculatior", []),

    Panel = wxPanel:new(Frame, []),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    Buttons = create_buttons(Panel),

    lists:foreach(fun(Button) ->
			  wxSizer:add(MainSz, Button)
		  end,
		  Buttons),

    wxFrame:show(Frame).





create_buttons(Panel) ->
    List1 = [{?wxID_ANY, [{label, "1"}]},
	     {?wxID_ANY, [{label, "2"}]},
	     {?wxID_ANY, [{label, "3"}]}],
    
    List2 = [{?wxID_ANY, [{label, "4"}]},
	     {?wxID_ANY, [{label, "5"}]},
	     {?wxID_ANY, [{label, "6"}]}],
    
    List3 = [{?wxID_ANY, [{label, "7"}]},
	     {?wxID_ANY, [{label, "8"}]},
	     {?wxID_ANY, [{label, "9"}]}],

    Fun = 
	fun({Id, Options}, Acc) ->
		Button = wxButton:new(Panel, Id, Options),
		[Button | Acc]
	end,

    Row1 = wxBoxSizer:new(?wxHORIZONTAL),
    Buttons1 = lists:foldr(Fun, [], List1),
    lists:foreach(fun(Button) ->
			  wxSizer:add(Row1, Button)
		  end, Buttons1),
    
    Row2 = wxBoxSizer:new(?wxHORIZONTAL),
    Buttons2 = lists:foldr(Fun, [], List2),
    lists:foreach(fun(Button) ->
			  wxSizer:add(Row2, Button)
		  end, Buttons2),
    
    Row3 = wxBoxSizer:new(?wxHORIZONTAL),
    Buttons3 = lists:foldr(Fun, [], List3),
    lists:foreach(fun(Button) ->
			  wxSizer:add(Row3, Button)
		  end, Buttons3),
        
%%    lists:flatten([Buttons1, Buttons2, Buttons3])
    [Row1, Row2, Row3].
