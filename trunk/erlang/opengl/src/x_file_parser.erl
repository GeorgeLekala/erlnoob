%%%-------------------------------------------------------------------
%%% File    : x_file_parser.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 12 Mar 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(x_file_parser).

-compile(export_all).

-define(DWORD, 32/unsigned-little-integer).
-define(LONG,  32/unsigned-little-integer).
-define(WORD,  16/unsigned-little-integer).
-define(BYTE,  8/unsigned-little-integer).


parse(File) ->
    {ok, Bin} = file:read_file(File),
    Text = binary_to_list(Bin),

    do_parse(Text).




do_parse(Text) ->
    Str = "Mesh {",
    case string:str(Text, Str) of
	0 ->
	    io:format("\"Mesh {\" not found in file.\n", []);
	Pos ->
	    string:substr(Text, Pos + length(Str))
    end.
    
	


remove_whitespace(Text) ->
    remove_whitespace(Text, []).

remove_whitespace([H | T], Acc) ->
    case H of
	"\n" -> remove_whitespace(T, Acc);
	" "  -> remove_whitespace(T, Acc);
	"\r" -> remove_whitespace(T, Acc);
	"\t" -> remove_whitespace(T, Acc);
	"#"  -> remove_comment(T),
		remove_whitespace(T, Acc);
	_    -> remove_whitespace(T, [H | Acc])
    end;
remove_whitespace([], Acc) ->
    lists:reverse(Acc).

remove_comment([H | T]) ->
    case H of
	"\n" ->
	    T;
	"\r" ->
	    remove_comment(T);
	_ ->
	    remove_comment(T)
    end.
