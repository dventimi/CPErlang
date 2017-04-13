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
-include_lib("eunit/include/eunit.hrl").	%Add Eunit.

%% ################################################################################
%% Functional interface.
%% ################################################################################

start() ->
    register(frontend,				%Front-end is put in the process registry.
	     spawn(?MODULE,			%Back-ends are not.
		   frontend_loop,		%Start the frontend loop.
		   [[				%Pass in two fresh backend processes, each with a rang of freqs.
		     spawn(?MODULE,backend_init,[lists:seq(10,15)]),
		     spawn(?MODULE,backend_init,[lists:seq(20,25)])
		    ]])).

stop() ->
    frontend ! {frontend,self(),stop}.		%Ask frontend to stop (in turn, it will ask backends to stop).

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

%% ################################################################################
%% Frontend server
%% ################################################################################

frontend_loop(Backends) ->
    frontend_loop(Backends,Backends,1).		%Jump into the frontend loop with a 'tag' of 1.

frontend_loop([],Backends,_N) ->		%Finished a round of round-robin?
    frontend_loop(Backends,Backends,1);		%Then start a new round, and reset tag to 1.
frontend_loop([X|Xs],Backends,N) ->		%Within a round of round-robin?
    receive 					%Then check for messages.
	{frontend,Pid,stop} ->			%Got a stop message?
	    lists:map(fun(Backend) ->		%Then send stop messages to backends...
    	    		      Backend ! {{tag,N},{message,{request,self(),stop}}}
    	    	      end,
    	    	      Backends),	    
	    Pid ! {reply,ok};			%...and bail.
	{frontend,Pid,dump} ->			%Got a dump diagnostic message?
	    Pid ! {reply,Backends},		%Then report back to client Pid with list of Backends,
	    frontend_loop([X|Xs],Backends,N);	%and loop, leaving loop state unchanged.
	{request,Pid,{deallocate,Freq,{tag,M}}} -> %Got a deallocate message, identified by a tag?
	    lists:nth(M,Backends) ! {{tag,M},{message,{request,Pid,{deallocate,Freq}}}}, %Then direct the deallocate message to that specific backend.
	    frontend_loop([X|Xs],Backends,N);	%and loop, leaving loop state unchanged.
	Msg ->					%Got any other kind of message?
	    X ! {{tag,N},{message,Msg}},	%Then just forward the message to the next backend, but tag it with N.
	    frontend_loop(Xs,Backends,N+1)	%and loop, discarding this backend and incrementing the tag.
    end.

%% ################################################################################
%% Backend server
%%
%% The protocol is basically: {{tag,N},{message,{request,Pid,Msg}}},
%% where Msg varies.  The {tag,N} element gets sent in to the backend,
%% and then the backend includes that tag packaged up somewhere in its
%% reply, where clients can find it.
%%
%% The key idea is this.  We can allocate in a dumb round-robin
%% fashion blindly.  But, deallocate is a little more complicated.
%% The frontend has to divert deallocate messages to a particular
%% backend (presumably, the backend from which the freq was
%% allocated).  So, who will know how to do this?  The frontend (which
%% is a proxy)?  No way, because then the frontend has to build up
%% state, mapping clients to the frequencies they've allocated.
%% That's basically session management, which I want to stay a
%% thousand miles away from, and I REALLY want a (mostly) stateless
%% frontend.  That only leaves the clients.  The clients have to
%% somehow own the state.  They have to know from which backend they
%% got a Freq, so they can ask the frontend to do the appropriate
%% switching of deallocate messages.
%%
%% Well, in that case the clients might as well know the Pid of the
%% backend from whom they got a Freq, but the trouble with that is,
%% how does the frontend (who's the traffic director) map the Pid to a
%% backend?  Yes, that could be done with TupleList and the keyfind/1
%% and related functions, but I didn't want to refactor the list-based
%% round-robin frontend proxy I'd already written.  Consequently,
%% clients are handed a "tag", in addition to the freq, when they're
%% allocating.  The tag is just the integer number of the backend,
%% starting with 1.  I.e., if there are two backends, N is in [1,2].
%% 
%% But, there's a subtle wrinkle.  The "tag"--which is really just the
%% current round-robin index--originates in the frontend.  After all,
%% it's the frontend proxy that's cycling the round-robin index.  So,
%% from whom does the client get it?  Not the frontend, because the
%% frontend just forwards messages from a client to a backend, and
%% then gets out of the way.  I.e., the frontend doesn't proxy return
%% messages from the backend back to the client (i.e., the message
%% with the allocated freq).  It COULD do that, but that's not the way
%% I wrote it and that's not the way I wanted to write it.  Instead,
%% backends message directly back to the client.
%%
%% Therefore, it becomes a little convoluted.  :)
%%
%% The frontend sends the tag (which, again, is just the round-robin
%% index) to the backend, and the backend sends the tag to the client
%% (along with the freq).  Now armed with a tag (along with a freq), a
%% client is now in a position to send a properly-formed deallocate
%% message, whose tag tells the frontend how to direct the deallocate
%% message and to whom.
%%
%% In practice, only the allocate message flow needs to cause a tag to
%% flow back to the client.  But, I wanted a degree of uniformity
%% across the protocol.  Consequently, all the messages are tagged,
%% whether they use the tag or not.
%%
%% Now, this is hopelessly complicated, and the protocol is a bit of a
%% bespoke nightmare.  Plus, there's basically no security: a client
%% could easily forge a deallocate message to deallocate a freq it
%% doesn't possess (though, it'd have to guess right about the
%% Freq-Tag mapping).  Put another way, this is not my best work.
%% But, this has gone on long enough, so I'm throwing in the towel.
%% ################################################################################

backend_init(Free) ->
    backend_loop({Free,[]}).			%Jump into the backend loop with a list of Free freqs, and empty Allocated.

backend_loop(Frequencies) ->
    receive
	{{tag,N},{message,{request,Pid,dump}}} -> %Got a dump message?
	    Pid ! {reply,{tag,N},Frequencies},	  %Then report back with my Free and Allocated freqs.
	    backend_loop(Frequencies);
	{{tag,N},{message,{{request,Pid,ping}}}} -> %Got a ping message?
	    Pid ! {reply,{tag,N},{pong,self()}},    %Then just reply with a pong and my Pid.
	    backend_loop(Frequencies);
	{{tag,N},{message,{request,Pid,allocate}}} -> %Got an allocate message?
	    {NewFrequencies,Reply} = allocate(Frequencies,Pid), %Then allocate a new freq.
	    Pid ! {reply,{tag,N},Reply},			%And report back, along with the tag.
	    backend_loop(NewFrequencies);
	{{tag,N},{message,{request,Pid,{deallocate,Freq}}}} -> %Got a deallocate message?
	    NewFrequencies = deallocate(Frequencies,Freq),     %Then deallocate
	    Pid ! {reply,{tag,N},ok},			       %And report back ok.
	    backend_loop(NewFrequencies);
	{{tag,N},{message,{request,Pid,stop}}} -> %Got a stop message?
	    Pid ! {reply,{tag,N},stopped};	  %Then report back, and then die.
	Msg ->					  %Got any other message?
	    io:format("Unexpected message: ~p~n",[Msg]) %Then log it as bogus.
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
%% Test function that just runs a script
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
    ?debugFmt("~p",[frequency_scaling:allocate()]),
    ?debugFmt("~p",[frequency_scaling:allocate()]),
    ?debugFmt("~p",[frequency_scaling:deallocate({10,1})]),
    ?debugFmt("~p",[frequency_scaling:allocate()]),
    ?debugFmt("~p",[frequency_scaling:allocate()]).

%% ################################################################################
%% Test output
%% ################################################################################

%% frequency_scaling.erl:207:<0.8865.2>: Starting the frequency server.
%% frequency_scaling.erl:210:<0.8865.2>: Frequency server started.
%% frequency_scaling.erl:216:<0.8870.2>: {ok,10}
%% frequency_scaling.erl:217:<0.8870.2>: {ok,20}
%% frequency_scaling.erl:218:<0.8870.2>: ok
%% frequency_scaling.erl:219:<0.8870.2>: {ok,10}
%% frequency_scaling.erl:220:<0.8870.2>: {ok,21}
