%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_hardened).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([
	 client/0
	]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?MODULE,
	     spawn(?MODULE, init, [])).

init() ->
    process_flag(trap_exit, true),    %%% ADDED
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
	{'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
	    NewFrequencies = exited(Frequencies, Pid), 
	    loop(NewFrequencies)
    end.

%% Functional interface

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

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),                                               %%% ADDED
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
    unlink(Pid),                                             %%% ADDED
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
	{value,{Freq,Pid}} ->
	    NewAllocated = lists:keydelete(Freq,1,Allocated),
	    {[Freq|Free],NewAllocated}; 
	false ->
	    {Free,Allocated} 
    end.

%% DONE Implement a client function that, when spawned as a process,
%% can be used to model a client.

client() ->
    client([]).

client(Frequencies) ->
    receive
	allocate ->				%Get new F from server
	    {ok,NewF} = allocate(),
	    client([NewF|Frequencies]);
	deallocate ->				%Return most recent F to server
	    case Frequencies of
		[F|Fs] ->			%Actually have something to return
		    deallocate(F),
		    client(Fs);
		[] ->				%Nothing to return
		    client(Frequencies)
	    end;
	dump ->					%Diagnostic
	    io:format("~p: ~w~n",[self(),Frequencies]),
	    client(Frequencies);
	stop ->					%Stop client process
	    ok;
	Msg ->					%"Catch all"
	    io:format("~p~n",[Msg]),		%Log Msg
	    client(Frequencies)			%Then loop again
    end.
