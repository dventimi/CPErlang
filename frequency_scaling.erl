%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_scaling).
-export([
	 allocate/0,
	 deallocate/1,
	 dump/0,
	 backend_init/1,
	 frontend_loop/1,
	 frontend_loop/2,
	 start/0,
	 stop/0
	]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    register(frontend,
	     spawn(?MODULE,
		   frontend_loop,
		   [[
		     spawn(?MODULE,backend_init,[lists:seq(10,15)]),
		     spawn(?MODULE,backend_init,[lists:seq(20,25)])
		    ]])).

stop() ->
    frontend ! {frontend,self(),stop}.

%% These are the start functions used to create and
%% initialize the server.

frontend_loop(Backends) ->
    frontend_loop(Backends,Backends).

frontend_loop([],Backends) ->
    frontend_loop(Backends,Backends);
frontend_loop([X|Xs],Backends) ->
    receive 
	{frontend,Pid,stop} ->
	    lists:map(fun(Backend) ->
    			      Backend ! {request,self(),stop}
    		      end,
    		      Backends),	    
	    Pid ! {reply,ok};
	{frontend,Pid,dump} ->
	    Pid ! {reply,Backends},
	    frontend_loop([X|Xs],Backends);
	Msg ->
	    X ! Msg,
	    frontend_loop(Xs,Backends)
    end.

%% The Main Loop

backend_init(Free) ->
    backend_loop({Free,[]}).

backend_loop(Frequencies) ->
    receive
	{request,Pid,dump} ->
	    Pid ! {reply,Frequencies},
	    backend_loop(Frequencies);
	{request,Pid,ping} ->
	    Pid ! {reply,{pong,self()}},
	    backend_loop(Frequencies);
	{request,Pid,allocate} ->
	    {NewFrequencies,Reply} = allocate(Frequencies,Pid),
	    Pid ! {reply,Reply},
	    backend_loop(NewFrequencies);
	{request,Pid ,{deallocate,Freq}} ->
	    NewFrequencies = deallocate(Frequencies,Freq),
	    Pid ! {reply,ok},
	    backend_loop(NewFrequencies);
	{request,Pid,stop} ->
	    Pid ! {reply,stopped}
    end.

%% Functional interface

dump() ->
    frontend ! {frontend,self(),dump},
    receive
	{reply,Backends} ->
	    io:format("~p~n",[Backends])
    end.

allocate() -> 
    frontend ! {request,self(),allocate},
    receive 
	{reply,Reply} -> Reply
    end.

deallocate(Freq) -> 
    frontend ! {request,self(),{deallocate,Freq}},
    receive 
	{reply,Reply} -> Reply
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[],Allocated},_Pid) ->
    {{[],Allocated},{error,no_frequency}};
allocate({[Freq|Free],Allocated},Pid) ->
    {{Free,[{Freq,Pid}|Allocated]},{ok,Freq}}.

deallocate({Free,Allocated},Freq) ->
    NewAllocated=lists:keydelete(Freq,1,Allocated),
    {[Freq|Free], NewAllocated}.
