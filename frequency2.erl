%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).				%REVIEWER:  Set module to frequency2 (see below)
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([
	 clear/0,
	 clearserver/0,
	 clearclient/0,
	 spam/1
	]).
-include_lib("eunit/include/eunit.hrl").	%REVIEWER:  EUnit support

%% These are the start functions used to create and
%% initialize the server.

%% REVIEWER: I find the instructions are a little confusing.  We're do
%% submit frequency.erl?  But frequency2.erl is the support file?
%% Except, the original module name was just 'frequency', which is an
%% error?  I decided that they must want us just to modify this file,
%% independent of the work we did in the previous exercise.  The
%% simplest way for me to do that was just to put all modifications in
%% this file, but change its module to 'frequency2'.  Consequently,
%% the register expression below has to change a bit.

start() ->
    register(frequency,
	     spawn(frequency2, init, [])).

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
	    Pid ! {reply, stopped};
	{request, Pid, clear} ->		%REVIEWER: New protocol handler for 'clear'
	    clear(),				%Use help function.  Remind me why we're not just using 'flush'?
	    Pid ! {reply, ok},			%Say 'ok' back to client.  All clear!
	    loop(Frequencies);			%Loop again, with state unchanged.
	{request, Pid, {echo,Msg}} ->		%REVIEWER: New protocol handler for 'echo'
	    Pid ! echo(Msg),			%Use help function.  (we're being a bit pedantic here)
	    loop(Frequencies)			%Loop again, with state unchanged.
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

%% REVIEWER: Added a spam functional API call to send arbitrary
%% messages.  The point is that these won't be handled and so will
%% just sit in the mailbox, which gives us something to clear.  No
%% reason the client should be the only one to experience all the
%% "clearing" fun, right?

spam(Msg) ->
    frequency ! {request, self(), Msg},
    ok.

%% REVIEWER: Added a clear function to the functional API.

clearserver() ->
    frequency ! {request, self(), clear},
    receive
	{reply, Reply} ->
	    Reply
    end.

%% REVIEWER: Heck, we might as well support clearing the client
%% process as well.

clearclient() ->
    clear().

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free],  NewAllocated}.

%% REVIEWER: Added an internal help function used to clear messages.
%% Note that this can be used by "anybody" (client, server, whomever).

clear() ->
    receive
	Msg ->					%If ANY message found,
	    ?debugFmt("~p~n",[Msg]),		% %No fun if we never see what we're clearing. But, only during debugging.
	    clear()				%discard and check again.
    after 0 ->					%If NO message found, time out immediately
	    ok					%then exit.
    end.

echo(Msg) ->
    Msg.

%% REVIEWER:  Adding EUnit tests.  See below.

%% Uses fixtures.  See:  http://learnyousomeerlang.com/eunit
frequency_test_() ->
    {setup,					
     fun setup/0,				%Setup function
     fun teardown/1,				%Teardown function
     fun test_functions/0}.			%Test function

setup() ->
    ?debugMsg("Starting the frequency server."),
    start(),					%Just wraps functional API call
    timer:sleep(1000),
    ?debugMsg("Frequency server started.").

teardown(_) ->					%EUnit teardown function has to take 1 parameter
    ?debugMsg("Starting the frequency server."),
    stop(),					%Just wraps the functional API call
    timer:sleep(1000),
    ?debugMsg("Frequency server started.").

test_functions() ->
    ?debugMsg("Start off with a little server spam"),
    spam("Server Spam 0"),

    ?debugMsg("Allocate a freq, wait for the reply, print it out."),
    {ok,F1} = allocate(),
    ?debugFmt("allocated:  ~w~n",[F1]),

    ?debugMsg("Deallocate the freq.  Should succeed."),
    deallocate(F1),

    ?debugMsg("Allocate a freq, wait for the reply, print it out."),
    {ok,F2} = allocate(),
    ?debugFmt("allocated:  ~w~n",[F2]),

    ?debugMsg("Because we allocated,deallocated,allocated, the two frequencies should be the same."),
    ?assert(F1==F2),

    ?debugMsg("Allocate a freq.  Because we haven't deallocated, it should be a new freq."),
    {ok,F3} = allocate(),
    ?debugFmt("allocated:  ~w~n",[F3]),
    ?assertNot(F1==F3),
    ?assertNot(F2==F3),

    ?debugMsg("Deallocate the freq that we have.  This should succeed the first time."),
    deallocate(F3),
    
    ?debugMsg("Send a bunch of spam to fill up the server's mailbox."),
    spam("Server Spam 1"),
    spam("Server Spam 2"),
    spam("Server Spam 3"),
    
    ?debugMsg("Send a bunch of spam that'll bounce back into the client's mailbox."),
    spam({echo, "Client Spam 1"}),
    spam({echo, "Client Spam 2"}),
    spam({echo, "Client Spam 3"}),

    ?debugMsg("Clear out the server's mailbox."),
    clearserver(),

    ?debugMsg("Clear out the client's mailbox."),
    clearclient().
