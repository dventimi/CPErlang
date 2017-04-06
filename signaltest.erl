-module(signaltest).
-export([init/0,loop/0]).

init() ->
    register(?MODULE,spawn(?MODULE,loop,[])).

loop() ->
    process_flag(trap_exit,true),
    receive
	{'EXIT',Pid,Reason} ->
	    io:format("~p,~p~n",[Pid,Reason]),
	    loop();
	Msg ->
	    io:format("~p~n",[Msg]),
	    loop()
    end.
