%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_exceptions).
-export([init/0]).
-include_lib("eunit/include/eunit.hrl").

%% ################################################################################
%% REVIEWER: Once again, I had a hard time interpreting exactly what
%% is being asked of us.  We're to add exceptions--and exception
%% handling--to deal with:
%%
%% * the case where an unallocated frequency is the subject of a
%% deallocation request
%%
%% * the receipt of an unknown message
%%
%% But, near the top of the instructions it also says, "look at how to
%% use exceptions to deal with unexpected cases IN THE
%% SERVER [emphasis added]."
%%
%% This latter passage is consistent with an interpretation of the
%% lecture material in which exceptions don't cross process
%% boundaries.  I.e., things like try...catch constructs are useful
%% for handling predictable exeptional circumstances WITHIN a process.
%% Given that, who exactly will be generating the exception (whether
%% it's an error, an exit, or a throw) and who will be catching it?
%% Not the client!  Not if we don't want exceptions crossing process
%% boundaries, that is.
%%
%% Consequently, I made the game-time decision to confine exception
%% generation and handling to the server.
%%
%% First, I modified the deallocate/2 help function so that instead of
%% returning an error value when asked to deallocate a non-allocated
%% frequency, it generates an error with the Error 'not_allocate'.  
%%
%% Second, in the case of an unexpected message there's no help
%% function in the first place to modify!  So, I just added an
%% additional receive clause in the loop/1 function, with the
%% catch-all pattern 'Msg'.  It's worth noting that this doesn't even
%% use error/1, exit/1, or throw/1 at all.
%%
%% Third, and in any case, the "handling within the server" is
%% satisfied by wrapping the whole loop in a try...catch construct
%% whose 'catch' portion just logs the error to standard out, and then
%% soldiers on.
%% 
%% After all, how do you "handle" a deallocation of a frequency that
%% hasn't been allocated?  You could take down the offending
%% client--if it was linked--but is that really necessary?
%%
%% Likewise, how do you "handle" an unexpected message?  There are
%% lots of ways, but absent any other requirements, who can say that
%% just logging the bad message and moving on is the wrong thing to
%% do?
%% ################################################################################

%% These are the start functions used to create and
%% initialize the server.

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
    try						%REVIEWER:  Added try
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
		Pid ! {reply, stopped};
	    Msg ->				%REVIEWER:  Added catch-all clause
		?debugFmt("Unknown Message: ~p~n",[Msg]),
		loop(Frequencies)
	end
    catch					%REVIEWER:  Added catch
	error:Error ->
	    ?debugFmt("Error: ~p~n",[Error]),
	    loop(Frequencies)
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    case lists:keyfind(Pid, 2, Allocated) of	%Check if client already has allocation.
	false -> 				%If not, allocate.
	    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}} ; 
	_ ->
	    error(already_allocated)		%If so, error
    end.

deallocate({Free, Allocated}, {Freq, Pid}) ->
    case lists:keyfind(Pid, 2, Allocated) of	%Check if client already has allocation.
	{Freq, Pid} ->				%If {Freq,Pid} match exactly, it's the client's (one and only) lease, so go ahead.
	    NewAllocated=lists:keydelete(Freq, 1, Allocated),
	    {{[Freq|Free], NewAllocated}, ok} ;
	_ ->
	    error(not_allocated)		%REVIEWER:  Added error/1 if not allocated
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REVIEWER:  Test functions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

frequency_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun test_functions/0}.

start() ->
    ?debugMsg("Starting the frequency server."),
    register(?MODULE,spawn(?MODULE,init,[])),
    timer:sleep(1000),
    ?debugMsg("Frequency server started.").

stop(_) ->
    ?MODULE ! {request,self(),stop}.    

test_functions() ->
    ?MODULE ! {request,self(),{deallocate,10}},
    ?MODULE ! {foo,self(),bar}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REVIEWER:  Sample transcript from running ?MODULE:test()
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% frequency_exceptions.erl:132:<0.250.0>: Starting the frequency server.
%% frequency_exceptions.erl:135:<0.250.0>: Frequency server started.
%% frequency_exceptions.erl:95:<0.251.0>: Error: not_allocated

%% frequency_exceptions.erl:90:<0.251.0>: Unknown Message: {foo,<0.253.0>,bar}

