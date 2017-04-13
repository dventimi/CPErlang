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
    frontend_loop(Backends,Backends,1).

frontend_loop([],Backends,_N) ->
    frontend_loop(Backends,Backends,1);
frontend_loop([X|Xs],Backends,N) ->
    receive 
	{frontend,Pid,stop} ->
	    lists:map(fun(Backend) ->
    	    		      Backend ! {{tag,N},{message,{request,self(),stop}}}
    	    	      end,
    	    	      Backends),	    
	    Pid ! {reply,ok};
	{frontend,Pid,dump} ->
	    Pid ! {reply,Backends},
	    frontend_loop([X|Xs],Backends,N);
	{request,Pid,{deallocate,Freq,{tag,M}}} ->
	    lists:nth(M,Backends) ! {{tag,M},{message,{request,Pid,{deallocate,Freq}}}},
	    frontend_loop([X|Xs],Backends,N);
	Msg ->
	    X ! {{tag,N},{message,Msg}},
	    frontend_loop(Xs,Backends,N+1)
    end.

%% The Main Loop

backend_init(Free) ->
    backend_loop({Free,[]}).

backend_loop(Frequencies) ->
    receive
	{{tag,N},{message,{request,Pid,dump}}} ->
	    Pid ! {reply,{tag,N},Frequencies},
	    backend_loop(Frequencies);
	{{tag,N},{message,{{request,Pid,ping}}}} ->
	    Pid ! {reply,{tag,N},{pong,self()}},
	    backend_loop(Frequencies);
	{{tag,N},{message,{request,Pid,allocate}}} ->
	    {NewFrequencies,Reply} = allocate(Frequencies,Pid),
	    Pid ! {reply,{tag,N},Reply},
	    backend_loop(NewFrequencies);
	{{tag,N},{message,{request,Pid,{deallocate,Freq}}}} ->
	    NewFrequencies = deallocate(Frequencies,Freq),
	    Pid ! {reply,{tag,N},ok},
	    backend_loop(NewFrequencies);
	{{tag,N},{message,{request,Pid,stop}}} ->
	    Pid ! {reply,{tag,N},stopped};
	Msg ->
	    io:format("Unexpected message: ~p~n",[Msg])
    end.

%% Functional interface

dump() ->
    frontend ! {frontend,self(),dump},
    receive
	{reply,Backends} ->
	    io:format("~p~n",[Backends]);
	Msg ->
	    io:format("Unexpected message: ~p~n",[Msg])
    end.

allocate() -> 
    frontend ! {request,self(),allocate},
    receive 
	{reply,{tag,N},Reply} -> 
	    io:format("Tag: ~p~n",[N]),
	    Reply;
	Msg ->
	    io:format("Unexpected message: ~p~n",[Msg])
    end.

deallocate({Freq,N}) -> 
    frontend ! {request,self(),{deallocate,Freq,{tag,N}}},
    receive 
	{reply,{tag,N},Reply} -> 
	    io:format("Tag: ~p~n",[N]),
	    Reply;
	Msg ->
	    io:format("Unexpected message: ~p~n",[Msg])
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

%% ################################################################################
%% REVIEWER:  Test function that just runs a script
%% ################################################################################

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

test_functions() ->
    ?debugFmt("~p~n",[frequency_scaling:allocate()]),
    ?debugFmt("~p~n",[frequency_scaling:allocate()]),
    ?debugFmt("~p~n",[frequency_scaling:deallocate({10,1})]),
    ?debugFmt("~p~n",[frequency_scaling:allocate()]),
    ?debugFmt("~p~n",[frequency_scaling:allocate()]).

