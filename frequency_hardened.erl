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
    mock_loop(1000,false).

mock_loop(Wait,Toggle) ->
    case Toggle of
	true -> mock_script();
	false -> ok
    end,
    timer:sleep(Wait),
    receive
	slower ->
	    mock_loop(round(Wait*1.10),Toggle);
	faster ->
	    mock_loop(round(Wait/1.10),Toggle);
	stop ->
	    ok;
	toggle ->
	    mock_loop(Wait,not(Toggle));
	Msg ->
	    io:format("~p~n",[Msg]),
	    mock_loop(Wait,Toggle)
    after 0 ->
	    mock_loop(Wait,Toggle)
    end.

mock_script() ->
    Msg = allocate(),
    case Msg of
	{ok,F1} ->
	    io:format("~p: Allocated ~p~n",[self(),F1]),
	    deallocate(F1),
	    io:format("~p: Deallocated ~p~n",[self(),F1]);
	_ ->
	    error
    end.
