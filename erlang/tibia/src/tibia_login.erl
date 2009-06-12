%%%-------------------------------------------------------------------
%%% File    : tibia_login.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 30 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(tibia_login).

-compile(export_all).

-include("tibia.hrl").

parse_login_package(State, <<_:12/binary,Msg:128/binary>>) ->
    Decrypted = crypto:rsa_private_decrypt(Msg, [?e, ?n, ?d], rsa_no_padding),

    <<0:8/?UINT,K1:32/?UINT,K2:32/?UINT,
     K3:32/?UINT,K4:32/?UINT,
     AccSize:16/?UINT,Acc:AccSize/binary,
     PassSize:16/?UINT,Pass:PassSize/binary,_/binary>> = Decrypted,
    Key = #key{k1 = K1,
	       k2 = K2,
	       k3 = K3,
	       k4 = K4},
    check_account(State#state{key = Key}, Acc, Pass).


parse_first_game_packet(State, Msg) ->
    Decrypted = crypto:rsa_private_decrypt(Msg, [?e, ?n, ?d], rsa_no_padding),
    <<0:8/?UINT,K1:32/?UINT,K2:32/?UINT,
     K3:32/?UINT,K4:32/?UINT,_GameMasterLogin:8/?UINT,
     AccSize:16/?UINT,Acc:AccSize/binary,
     NameSize:16/?UINT, Name:NameSize/binary,
     PassSize:16/?UINT, Pass:PassSize/binary,_/binary>> = Decrypted,
    Key = #key{k1 = K1,
	       k2 = K2,
	       k3 = K3,
	       k4 = K4},
    io:format("Acc: ~p Name: ~p\n", [Acc,Name]),
    check_account(State#state{key = Key}, Acc, Pass).


check_account(State, Acc, Pass) ->
    {ok, {IP, _Port}} = inet:peername(State#state.client_socket),
    Disabled = is_disabled(IP),
    if size(Acc) =:= 0 ->
	    tibia_proxy:disconnect(State,
				   <<"You need to enter an account number.">>);
       Disabled ->
	    timer:apply_after(30*1000,?MODULE,delete_tries,[IP]),
	    tibia_proxy:disconnect(State,
				   <<"too many connection attempts, wait 30 seconds">>);

       true ->
	    ignore
    end,
    
    F = fun() ->
		Pattern = #account{name = binary_to_list(Acc),
				   password = binary_to_list(Pass),
				   _ = '_'},
		mnesia:match_object(Pattern)
	end,
    case mnesia:transaction(F) of
	{atomic, [Account]} ->
	    State#state{account = Account};
	{atomic, []} ->
	    login_add_count(IP),
	    tibia_proxy:disconnect(State,
				   <<"Account name or password is incorrect.">>)
    end.





login_add_count(IP) ->
    F = fun() ->
		case mnesia:match_object(#tries{ip = IP,
						_ = '_'}) of
		    [Rec] ->
			Tries =
			    #tries{ip = IP,
				   tries = Rec#tries.tries + 1},
			mnesia:write(Tries);
		    [] ->
			mnesia:write(#tries{ip = IP,
					    tries = 1});
		    Other ->
			error_logger:format("login_add_count returned ~p\n",
					    [Other])
		end
	end,
    mnesia:transaction(F),
    ok.

is_disabled(IP) ->
    F = fun() -> case mnesia:match_object(#tries{ip = IP,
						 _ = '_'}) of
		     [Rec] ->
			 Rec#tries.tries;
		     [] ->
			 0;
		     Other ->
			 error_logger:format("is_disabled returned ~p\n",
					     [Other])
		 end
	end,
    {atomic, Tries} = mnesia:transaction(F),
    if Tries >= 5 -> true;
       true -> false
    end.

delete_tries(IP) ->
    F = fun() ->
		mnesia:delete({tries, IP})
	end,
    mnesia:transaction(F).


