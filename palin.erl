-module(palin).
-export([palin/1,nopunct/1,palindrome/1]).
-export([server/1]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

%% A palindrome checking server

server(Pid) ->
    receive
	{check,Msg} ->
	    Pid ! {result,
		   lists:flatten(
		     io_lib:format(
		       "\"~s\"~sa palindrome.",
		       [Msg,
			case(palindrome(Msg)) of
			    true -> " is ";
			    false -> " is not "
			end]))},
	    server(Pid);
	_ ->
	    ok
    end.
