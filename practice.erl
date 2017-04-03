-module(practice).
-export([
	 receiver1/0,
	 receiver2/0,
	 receiver3/0,
	 test2/0,
	 test3/0,
	 process/0,
	 test4/0,
	 loop/1
	]).

%% Design a simple test bed to show how messages are processed in
%% mailbox order.

receiver1() ->
    receive
	Msg ->
	    io:format("message:~w:~s~n",[Msg,erlang:timestamp()]),
	    receiver1()
    end.

receiver2() ->
    receive
	Msg ->
	    case Msg of
		stop ->
		    ok;
		wait ->
		    timer:sleep(1000),
		    receiver2();
		pen ->
		    io:format("~w:~s~n",[erlang:timestamp(),"I have a pen."]),
		    receiver2();
		apple ->
		    io:format("~w:~s~n",[erlang:timestamp(),"I have a apple."]),
		    receiver2();
		pineapple ->
		    io:format("~w:~s~n",[erlang:timestamp(),"Unh! Pineapple pen!."]),
		    receiver2();
		_Msg ->
		    receiver2()
	    end
    end.

receiver3() ->
    receive
	stop ->
	    ok;
	wait ->
	    timer:sleep(1000),
	    receiver3();
	pen ->
	    io:format("~w:~s~n",[erlang:timestamp(),"I have a pen."]),
	    receiver3();
	apple ->
	    io:format("~w:~s~n",[erlang:timestamp(),"I have a apple."]),
	    receiver3();
	pineapple ->
	    io:format("~w:~s~n",[erlang:timestamp(),"Unh! Pineapple pen!."]),
	    receiver3();
	_Msg ->
	    receiver3()
    end.

test2() ->
    S2 = spawn(practice,receiver2,[]),
    S2 ! pineapple,
    S2 ! apple,
    S2 ! pen,
    ok.

test3() ->
    S3 = spawn(practice,receiver3,[]),
    S3 ! pineapple,
    S3 ! apple,
    S3 ! pen,
    ok.

%% Processing messages in sequence

process() ->
    process_first().

process_first() ->
    receive
	{first,FirstString} ->
	    io:format("~s~n",[FirstString]),
	    process_second()
    end.

process_second() ->
    receive
	{second,SecondString} ->
	    io:format("~s~n",[SecondString]),
	    ok
    end.
    
test4() ->
    S4 = spawn(practice,process,[]),
    S4 ! {second,"Message 2"},
    S4 ! {first,"Message 1"}.


loop(N) ->
    receive 
	stop -> io:format("~w~n",[N]);
	_Msg -> loop(N)
    after 1000 ->
	    loop(N+1)
    end.
