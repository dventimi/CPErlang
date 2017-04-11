%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_hot).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([					%REVIEWER:  Export new functions.
	 inject/1,
	 dump/0
	]).
-include_lib("eunit/include/eunit.hrl").	%REVIEWER:  Include EUnit.

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?MODULE,
	     spawn(?MODULE, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
    receive
	{request, _Pid, dump} ->		%REVIEWER:  Add 'dump' to protocol, for diagnostic purposes.  Note that it doesn't use a help fn.
	    io:format("Frequencies:~p~n",[Frequencies]),
	    loop(Frequencies);
	{request, Pid, {inject, Freqs}}	->	%REVIEWER:  Add 'inject' to protocol. See help function.
	    {NewFrequencies, Reply} = inject(Frequencies, Freqs),
	    Pid ! {reply, Reply},
	    loop(NewFrequencies);
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

dump() ->					%REVIEWER:  Add functional API call for 'dump' message.
    ?MODULE ! {request, self(), dump}.

inject(Freqs) ->				%REVIEWER:  Add functional API call for 'inject' message.
    ?MODULE ! {request, self(), {inject, Freqs}},
    receive
	{reply, Reply} ->
	    Reply
    end.

allocate() -> 
    ?MODULE ! {request, self(), allocate},
    receive 
	{reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    ?MODULE ! {request, self(), {deallocate, Freq}},
    receive 
	{reply, Reply} -> Reply
    end.

stop() -> 
    ?MODULE ! {request, self(), stop},
    receive 
	{reply, Reply} -> Reply
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

inject({Free,Allocated},Freqs) ->		%REVIEWER:  Add help function for 'inject' message.
    {{Free++Freqs,Allocated}, ok}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free],  NewAllocated}.

%% This is the basic functionality exhibited by Simon in the video
%% lecture.
frequency_test_() ->
    {setup,
     fun server_start/0,
     fun server_stop/1,
     fun test_functions/0}.

server_start() ->
    ?debugMsg("Starting the frequency server."),
    start(),
    timer:sleep(1000),
    ?debugMsg("Frequency server started.").

server_stop(_) ->
    stop().

%% ################################################################################
%% REVIEWER:  Test function that just runs a script
%% ################################################################################

test_functions() ->
    ?debugFmt("~p", [inject([30,31,32])]),
    ?debugFmt("~p", [dump()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]),
    ?debugFmt("~p", [allocate()]).

%% ################################################################################
%% REVIEWER:  Sample output from test script
%% ################################################################################

%% frequency_hot.erl:110:<0.311.0>: Starting the frequency server.
%% frequency_hot.erl:113:<0.311.0>: Frequency server started.
%% frequency_hot.erl:119:<0.314.0>: ok
%% frequency_hot.erl:120:<0.314.0>: {request,<0.314.0>,dump}
%% frequency_hot.erl:121:<0.314.0>: {ok,10}
%% frequency_hot.erl:122:<0.314.0>: {ok,11}
%% frequency_hot.erl:123:<0.314.0>: {ok,12}
%% frequency_hot.erl:124:<0.314.0>: {ok,13}
%% frequency_hot.erl:125:<0.314.0>: {ok,14}
%% frequency_hot.erl:126:<0.314.0>: {ok,15}
%% frequency_hot.erl:127:<0.314.0>: {ok,30}
%% frequency_hot.erl:128:<0.314.0>: {ok,31}
%% frequency_hot.erl:129:<0.314.0>: {ok,32}
%% frequency_hot.erl:130:<0.314.0>: {error,no_frequency}
%% frequency_hot.erl:131:<0.314.0>: {error,no_frequency}

