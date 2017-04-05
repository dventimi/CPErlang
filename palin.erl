-module(palin).
-export([palin/1,nopunct/1,palindrome/1]).
-export([
	 server/0,
	 server/1,
	 proxy/1,
	 proxy/2
	]).

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

%% The basic solution creates a server process for a particular client
%% Pid, and then waits for messages having the proper format,
%% returning the appropriate response.

server(Pid) ->
    receive
	{check,Msg} ->				%Check for a palindrome.
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
	_ ->					%Stop for ANY other message.
	    ok
    end.

%% The modified solution creates a server not tied to a particular
%% client, and then waits for messages having the proper format (which
%% now includes the client Pid), returning the appropriate response to
%% the client.  Note also that now a diagnostic message is written to
%% standard out indicating the client Pid.

server() ->
    receive
	{check,Msg,Pid} ->			%Check for a palindrome.
	    io:format("Server:  ~w~n",[self()]),
	    Pid ! {result,
		   lists:flatten(
		     io_lib:format(
		       "\"~s\"~sa palindrome.",
		       [Msg,
			case(palindrome(Msg)) of
			    true -> " is ";
			    false -> " is not "
			end]))},
	    server();
	_ ->					%Stop for ANY other message.
	    ok
    end.

%% The replicating server solution creates a server that proxies
%% messages to a list of servers in a "round-robin" fashion.  Note
%% that the proxy server is agnostic about the server protocol.  I.e.,
%% it just forwards messages to the servers, who must handle
%% communication back to the client themselves.  Note also that a
%% proxy server does not control the life-cycle of its proxied
%% servers.  I.e., they must be started before calling proxy/1, with
%% their Pids passed in in a list.  Likewise, the proxied servers must
%% be stopped independent of the proxying server.  However, given that
%% the proxy server is protocol-agnostic, if the proxied servers
%% support stopping themselves in their own protocol, then the stop
%% message CAN be sent through the proxy server, though whatever that
%% message is, it must be sent multiple times once for ever server.
%% In practice, it may be safer to stop the proxied servers directly.

proxy(Servers) ->
    proxy(Servers,Servers).

proxy([],Servers) ->
    proxy(Servers,Servers);
proxy([X|Xs],Servers) ->
    receive 
	stop ->
	    ok;
	Msg ->
	    X ! Msg,
	    proxy(Xs,Servers)
    end.
