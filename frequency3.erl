%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency3).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([
	 clear/0
	]).

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
	    timer:sleep(10000),
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
    clear(), %% clear messages received after the timeout 
    frequency ! {request, self(), allocate}, 
    receive 
	{reply, Reply} -> Reply 
    after 1000 -> 
	    {error, timeout} 
    end.

deallocate(Freq) -> 
    clear(), %% clear messages received after the timeout 
    frequency ! {request, self(), {deallocate, Freq}}, 
    receive 
	{reply, Reply} -> Reply 
    after 1000 -> 
	    {error, timeout} 
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Valentin's code
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear() -> 
    receive 
	Msg -> 
	    io:format("Clearing message: ~w~n", [Msg]), 
	    clear() 
    after 0 -> 
	    ok 
    end.
