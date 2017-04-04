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
	    NewFrequencies = deallocate(Frequencies, Freq),
	    Pid ! {reply, ok},
	    loop(NewFrequencies);
	{request, Pid, stop} ->
	    Pid ! {reply, stopped}
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

%% Test functions

frequency_test() ->
    Freq = spawn(frequency,init,[]),
    timer:sleep(1000),
    Freq ! {request,self(),allocate},
    timer:sleep(1000),
    {ok,F1} = receive {reply,Msg1} -> Msg1 end,
    ?debugFmt("allocated:  ~w~n",[F1]),
    Freq ! {request,self(),{deallocate,F1}},
    timer:sleep(1000),
    ok = receive {reply,Msg2} -> Msg2 end,
    Freq ! {request,self(),allocate},
    timer:sleep(1000),
    {ok,F2} = receive {reply,Msg3} -> Msg3 end,
    ?debugFmt("allocated:  ~w~n",[F2]),
    ?assert(F1==F2).
    
