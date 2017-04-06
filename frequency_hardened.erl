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
	 mock_init/0
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

%% TODO Implement a client function that, when spawned as a process,
%% can be used to model a client.

mock_init() ->
    process_flag(trap_exit, true),
    mock_loop(1000).

mock_loop(Wait) ->
    mock_script(),
    timer:sleep(Wait),
    io:format("~p: Tick ~ps~n",[self(),Wait/1000]),
    receive
	slower ->
	    mock_loop(round(Wait*1.10));
	faster ->
	    mock_loop(round(Wait/1.10));
	stop ->
	    ok;
	Msg ->
	    io:format("~p~n",[Msg]),
	    mock_loop(Wait)
    after 0 ->
	    mock_loop(Wait)
    end.

mock_script() ->
    {ok,F1} = allocate(),
    io:format("~p: Allocated ~p~n",[self(),F1]),
    {ok,F2} = allocate(),
    io:format("~p: Allocated ~p~n",[self(),F2]),
    deallocate(F1),
    io:format("~p: Deallocated ~p~n",[self(),F1]),
    deallocate(F2),
    io:format("~p: Deallocated ~p~n",[self(),F2]).

