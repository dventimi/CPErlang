%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_supervised).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([
	 start_link/2,
	 stop/1,
	 init/1
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

%% ################################################################################
%% TODO Program a supervisor process that can be used to start the
%% supervisor, and restart it whenever it has terminated unexpectedly.
%% ################################################################################

%% REVIEWER: This instruction from FutureLearn is difficult to parse.
%% "Program a supervisor process that can be used to start the
%% supervisor"???  Huh???  Don't you mean, "Program a supervisor
%% process that can be used to start the worker"?  Anyway, that's what
%% I'm doing, with the worker process being the frequency server.

start_link(Name,ChildSpecList) -> 
    register(Name,spawn_link(my_supervisor,init,[ChildSpecList])),
    ok.

init(ChildSpecList) -> 
    process_flag(trap_exit,true),
    supervisor_loop(start_children(ChildSpecList)).

start_children([]) -> []; 
start_children([{M,F,A}|ChildSpecList]) -> 
    case (catch apply(M,F,A)) of 
	{ok,Pid} -> 
	    [{Pid,{M,F,A}}|start_children(ChildSpecList)];
	_ -> 
	    start_children(ChildSpecList)
    end.

restart_child(Pid,ChildList) -> 
    {value,{Pid,{M,F,A}}} = lists:keysearch(Pid, 1, ChildList), 
    {ok,NewPid} = apply(M,F,A),
    [{NewPid,{M,F,A}}|lists:keydelete(Pid,1,ChildList)].

supervisor_loop(ChildList) -> 
    receive 
	{'EXIT',Pid,_Reason} -> 
	    NewChildList = restart_child(Pid,ChildList),
	    supervisor_loop(NewChildList);
	{stop,From} -> 
	    From ! {reply,terminate(ChildList)}
    end.

stop(Name) -> 
    Name ! {stop,self()},
    receive 
	{reply,Reply} -> 
	    Reply
    end.

terminate([{Pid,_}|ChildList]) -> 
    exit(Pid,kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.
