-module(errors).
-export([
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

try_return(X) when is_integer(X) ->
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
