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
	 supervisor_start/0,
	 supervisor_stop/0,
	 supervisor_init/0,
	 worker_init/0
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

%% The supervisor follows the generic process skeleton:
%% 1. Entry-point is an initialize function (supervisor_init/0).
%% 2. Jump into the Receive/Evaluate loop (supervisor_loop/1).
%% 3. Receive/Evaluate loop handles various messages.
%% 4. Terminate if you get a stop message.

%% The specific parts of the supervisor are these.
%% 1. The supervisor_init/0 function sets up trap exits to convert signals to messages.
%% 2. It starts one worker process via the frequency server's own init/0 function (see above).
%% 3. It establishes a bi-directional link using spawn_link (could also be uni-directional with a monitor).
%% 4. The state managed by supervisor_loop/1 is just the Pid of the supervised worker.
%% 5. Handle 'EXIT' messages from dying workers by just respawning the worker.
%% 6. Note that there's just one worker.  Could be a list, but I just want to keep it simple for now.

%% NOTE: Communication is an open question. The lecture videos talked
%% about supervision purely in terms of life-cycle management, but
%% didn't say anything about communication patterns.  Do supervisors
%% proxy messages to their worker processes?  I'm sure they could, but
%% I don't know if that's typical.  I just chose not to address that
%% issue, for the sake of simplicity.  The one and only one worker
%% process gets registered into the process registry.  If it dies, a
%% new process is registered under the same name (the module name).
%% That preserves the functional API methods that we originally
%% defined above.

supervisor_init() -> 
    process_flag(trap_exit,true),		%Trap exits
    supervisor_loop().				%Jump into the loop

worker_init() ->				%Just a wrapper to the function above
    init().

supervisor_loop() ->
    Pid = spawn_link(?MODULE,worker_init,[]),
    register(?MODULE,Pid),			%We could register under anything, say, "worker"
    supervisor_loop(Pid).
supervisor_loop(Pid) -> 
    receive 
	{'EXIT',SomePid,Reason} -> 		%Oh nos!  Worker died!  Respawn!
	    io:format("~w,~p~n",[SomePid,Reason]),
	    NewPid = spawn_link(?MODULE,worker_init,[]),
	    register(?MODULE,NewPid),		%But, we want to preserve the allocate/deallocate functions way above.
	    supervisor_loop(Pid);
	{stop,From} -> 				%Our client wants us to stop.
	    exit(whereis(?MODULE),kill),		%Exterminate worker w/ extreme prejudice
	    From ! {reply,ok}			%Hey, Client, everything's A-OK!
    end.

supervisor_start() -> 				%Functional API helper
    register(supervisor,
	     spawn(?MODULE,supervisor_init,[])),
    ok.

supervisor_stop() -> 				%Functional API helper
    supervisor ! {stop,self()},			%Hey, Supervisor, Stop!
    receive 
	{reply,Reply} -> 			%You stopped!  Great!
	    Reply
    end.
