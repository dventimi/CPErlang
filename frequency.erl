%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0]).
-include_lib("eunit/include/eunit.hrl").

%% These are the start functions used to create and
%% initialize the server.

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
	    {NewFrequencies, Reply} = deallocate(Frequencies, {Freq, Pid}),
	    Pid ! {reply, Reply},
	    loop(NewFrequencies);
	{request, Pid, stop} ->
	    Pid ! {reply, stopped}
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    case lists:keyfind(Pid, 2, Allocated) of	%Check if client already has allocation.
	false -> 				%If not, allocate.
	    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}} ; 
	_ -> 					%If so, leave state unchanged and error.
	    {{[Freq|Free], Allocated}, {error, denied}}
    end.

deallocate({Free, Allocated}, {Freq, Pid}) ->
    case lists:keyfind(Pid, 2, Allocated) of	%Check if client already has allocation.
	{Freq, Pid} ->				%If {Freq,Pid} match exactly, it's the client's (one and only) lease, so go ahead.
	    NewAllocated=lists:keydelete(Freq, 1, Allocated),
	    {{[Freq|Free], NewAllocated}, ok} ;
	_ ->					%Otherwise, leave state alone, and error.
	    {{Free, Allocated}, {error, denied}}
    end.


%% Test functions

%% This is the basic functionality exhibited by Simon in the video
%% lecture.
frequency_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun test_functions/0}.

start() ->
    ?debugMsg("Starting the frequency server."),
    register(?MODULE,spawn(frequency,init,[])),
    timer:sleep(1000),
    ?debugMsg("Frequency server started.").

stop(_) ->
    ?MODULE ! {request,self(),stop}.    

test_functions() ->
    %% Allocate a freq, wait for the reply, print it out.
    ?MODULE ! {request,self(),allocate},
    {ok,F1} = receive {reply,Msg1} -> Msg1 end,
    ?debugFmt("allocated:  ~w~n",[F1]),

    %% Deallocate the freq.  Should succeed.
    ?MODULE ! {request,self(),{deallocate,F1}},
    ok = receive {reply,Msg2} -> Msg2 end,

    %% Allocate a freq, wait for the reply, print it out.
    ?MODULE ! {request,self(),allocate},
    {ok,F3} = receive {reply,Msg3} -> Msg3 end,
    ?debugFmt("allocated:  ~w~n",[F3]),

    %% Because we allocated,deallocated,allocated, the two frequencies
    %% should be the same.
    ?assert(F1==F3),

    %% Allocate a freq.  Because we already have an allocation, this
    %% should fail.
    ?MODULE ! {request,self(),allocate},
    {error,denied} = receive {reply,Msg5} -> Msg5 end,

    %% Deallocate the freq that we have.  This should succeed the
    %% first time.
    ?MODULE ! {request,self(),{deallocate,10}},
    ok = receive {reply,Msg6} -> Msg6 end,

    %% Now, deallocate a freq for which we no longer have a lease.
    %% This should fail.
    ?MODULE ! {request,self(),{deallocate,10}},
    {error,denied} = receive {reply,Msg7} -> Msg7 end.


			     
