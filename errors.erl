-module(errors).
-export([
	 bail/3,
	 recurse/2,
	 try_recurse/1,
	 try_return/1
	]).
-include_lib("eunit/include/eunit.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Main test function
%% 
%% This is the heart of the action.  The point of this is to try to
%% tease out the actual semantic differences between throw/1, error/1, and exit/1.  
%% 
%% As you can see, aside from the success clause, the three failure
%% clauses are nearly identical.
%% 
%% 1. They all invoke one of the failure BIFs (throw/1, error/1, or
%% exit/1).
%% 
%% 2. Next, they try to emit a debug message.
%% 
%% 3. Finally, they each try to return a tuple of {ok,<atom>}, where
%% <atom> is in [throw,error,exit] to indicate which occurred.
%% 
%% Here's the REAL point of this.  We know that throw/1 is a
%% "non-local return", so we expect never to see that clause's debug
%% message and never to encounter its return value.  That's fine.
%% 
%% But, in that it's a "non-local return," how is throw/1 different
%% from error/1 or exit/1, if at all?  In their behavior, those two
%% BIFs ALSO seem to be "non-local returns."  So, why in books is the
%% distinction attached to throw/1, and not to the other two BIFs?
%%
%% I find this very puzzling.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_error(0) ->
    ?debugMsg("Normal-case message"),		%This message, seen
    {ok,normal};				%This pair, returned
return_error(1) ->
    throw(foo),					%Definitely non-local exit
    ?debugMsg("Post-throw message"),		%Consequently, never seen
    {ok,throw};					%And, this pair never returned
return_error(2) ->
    error(foo),					%But, also a non-local exit?
    ?debugMsg("Post-error message"),		%This message, also never seen!
    {ok,error};					%This pair, never returned
return_error(3) ->
    exit(foo),					%Another non-local exit!
    ?debugMsg("Post-exit message."),		%Again, message not seen
    {ok,'EXIT'}.				%And pair, not returned

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Driving function with a try...catch construct, with exception
%% patterns matching on each of the three major error classes: throw,
%% error, and exit.
%%
%% 1. Note the symmetry across the three error classes.
%% 
%% 2. They each have an atom for the error class (throw, error, or
%% exit), followed by a colon (:), followed by a variable (V).
%% 
%% 3. The same variable, V, was chosen to emphasize that technically,
%% it can be anything (it doesn't have to be Throw, or Error, or
%% Reason, as it often is in books).
%% 
%% 4. Likewise, the same return value, {foo, V}, was chosen to
%% emphasize that it can be anything.  It doesn't have to be
%% {throw,Throw}, {error,Error}, or {'EXIT',Reason}, as it often is in
%% books.
%% 
%% 5. So, the clauses in the catch branch are functionally equivalent.
%% They match on three different error classes, but the behavior is
%% the same.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_return(X) ->
    try return_error(X) of
	Val ->
	    Val
    catch
	throw:V ->				%error class = throw
	    {foo, V};
	error:V ->				%error class = error
	    {foo, V};
	exit:V ->				%error class = exit
	    {foo, V}
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test function that just runs through a script
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error_test() ->
    ?debugMsg("Step1:  success path"),
    ?debugFmt("try_return(0):  ~p~n",[try_return(0)]),

    ?debugMsg("Step2:  throw path"),
    ?debugFmt("try_return(1):  ~p~n",[try_return(1)]),

    ?debugMsg("Step3:  error path"),
    ?debugFmt("try_return(2):  ~p~n",[try_return(2)]),

    ?debugMsg("Step4:  exit path"),
    ?debugFmt("try_return(3):  ~p~n",[try_return(3)]).
    
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Transcript of a sample run of errors:test().
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% errors.erl:37:<0.212.0>: Step1:  success path
%% errors.erl:8:<0.212.0>: Normal-case message
%% errors.erl:38:<0.212.0>: try_return(0):  {ok,normal}

%% errors.erl:40:<0.212.0>: Step2:  throw path
%% errors.erl:41:<0.212.0>: try_return(1):  {throw,foo}

%% errors.erl:43:<0.212.0>: Step3:  error path
%% errors.erl:44:<0.212.0>: try_return(2):  {error,foo}

%% errors.erl:46:<0.212.0>: Step4:  exit path
%% errors.erl:47:<0.212.0>: try_return(3):  {exit,foo}

%%   Test passed.
%% ok

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Take II.
%%
%% 
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Driving function.  'Choice' parameter is in [0,1,2,3].
%%
%% 0 causes a local return. Execution continues in bail/3.
%%
%% 1 causes a non-local return with throw.  Execution continues in
%% catch block of try_recurse/1.
%%
%% 2 causes an error.  Is it non-local?  What is returned?
%%
%% 3 causes an exit.  Is it non-local?  What is returned?

try_recurse(Choice) ->    
    try recurse(10,				%Max 10 recursions
		fun(X)->
			bail(Choice,X,5) 	%Bail with 'Choice' when X decrements from 10 to 5
		end)
    catch
	throw:V ->
	    io:format("Non-local return of ~p caused by 'throw'.~n",[V]),
	    V;
	error:V ->
	    io:format("Non-local return of ~p caused by 'error'.~n",[V]),
	    V;
	exit:V ->
	    io:format("Non-local return of ~p caused by 'exit'.~n",[V]),
	    V
    end.

%% Non-tail recursively apply the function F, maximum N times.  The
%% number of recursions may be less, in particular if F performs a
%% non-local return.

recurse(N,_F) when N =< 0 ->
    ok;
recurse(N,F) ->
    F(N),
    recurse(N-1,F).

%% Function we can use in recurse/2.  Depending on Choice (0,1,2,3),
%% it may or may not perform a non-local return.  0 definitely
%% doesn't.  1 definitely does, because calls throw/1.  2 & 3 call
%% error/1 and exit/1, respectively.  Are they considered non-local
%% returns?

bail(Choice,N,M) when N<M ->
    case Choice of
	0 ->
	    ok;
	1 ->
	    throw(non_local_throw);
	2 ->
	    error(non_local_error);
	3 ->
	    exit(non_local_exit)
    end,
    io:format("Local return, N:~p~n",[N]),
    ok;
bail(_Choice,_N,_M) ->
    ok.

