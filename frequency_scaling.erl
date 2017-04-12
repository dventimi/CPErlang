%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_scaling).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([
	 proxy/1,
	 proxy/2
	]).
-include_lib("eunit/include/eunit.hrl").

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
    receive
	{request, Pid, allocate} ->
	    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
	    Pid ! {reply, Reply},
	    loop(NewFrequencies);
	{request, Pid , {deallocate, Freq}} ->
	    NewFrequencies = deallocate(Frequencies, Freq),
	    Pid ! {reply, ok},
	    loop(NewFrequencies);
	{request, Pid, stop} ->
	    Pid ! {reply, stopped}
    end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	{reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	{reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	{reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free],  NewAllocated}.

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
