%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(gf).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

% an implementation of this is included.
-export([start_link/0]).

% you need to implement these functions.
-export([init/1, handle_call/3, handle_cast/2]).

% these are implemented for you.
-export([handle_info/2, terminate/2, code_change/3]).

% you will need to implement these.
-export([allocate/0,deallocate/1,stop/0,report/0,inject/1]).

%% These are the start functions used to create and
%% initialize the server.

start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, 
      ?MODULE, [], []).

init([]) ->
    {ok,{get_frequencies(),[]}}.

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% Functional interface

allocate() -> 
    gen_server:call(?MODULE,allocate).

deallocate(Freq) -> 
    gen_server:cast(?MODULE,{deallocate,Freq}).

stop() ->  
    gen_server:stop(?MODULE).

report() ->
    gen_server:call(?MODULE,report).

inject(Freq) ->
    gen_server:cast(?MODULE,{inject,Freq}).

%% Callback functions

handle_call(allocate,From,Frequencies) ->
    {NewFrequencies,Reply} = allocate(Frequencies,From),
    {reply,Reply,NewFrequencies};
handle_call(report,_From,Frequencies) ->
    {reply,Frequencies,Frequencies}.

handle_cast({deallocate,Freq},Frequencies) ->
    NewFrequencies = deallocate(Frequencies,Freq),
    {noreply,NewFrequencies};
handle_cast({inject,Freq},Frequencies) ->
    NewFrequencies = inject(Frequencies,Freq),
    {noreply,NewFrequencies}.

%% handle_cast(stop,State) ->
%%     'for you to do'.    

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

inject({Free, Allocated}, Freq) ->
    {[Freq|Free], Allocated}.
    

% default implementations

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ################################################################################
%% Test function
%% ################################################################################

paces_test() ->
    ?debugMsg("Start the server."),
    ?debugFmt("~p",[gf:start_link()]),

    ?debugMsg("Allocate frequency 10 and report."),
    ?debugFmt("~p",[gf:allocate()]),
    ?debugFmt("~p",[gf:report()]),

    ?debugMsg("Inject frequency 20 and report."),
    ?debugFmt("~p",[gf:inject(20)]),
    ?debugFmt("~p",[gf:report()]),

    ?debugMsg("Allocate frequency 20 and report."),
    ?debugFmt("~p",[gf:allocate()]),
    ?debugFmt("~p",[gf:report()]),

    ?debugMsg("Deallocate frequencies 10 & 10 and report."),
    ?debugFmt("~p",[gf:deallocate(10)]),
    ?debugFmt("~p",[gf:deallocate(20)]),
    ?debugFmt("~p",[gf:report()]),

    ?debugMsg("Stop the server."),
    ?debugFmt("~p",[gf:stop()]).

%% ################################################################################
%% Test output
%% ################################################################################

%% gf.erl:104:<0.118.0>: Start the server.
%% gf.erl:105:<0.118.0>: {ok,<0.120.0>}
%% gf.erl:107:<0.118.0>: Allocate frequency 10 and report.
%% gf.erl:108:<0.118.0>: {ok,10}
%% gf.erl:109:<0.118.0>: {[11,12,13,14,15],[{10,{<0.118.0>,#Ref<0.0.4.9>}}]}
%% gf.erl:111:<0.118.0>: Inject frequency 20 and report.
%% gf.erl:112:<0.118.0>: ok
%% gf.erl:113:<0.118.0>: {[20,11,12,13,14,15],[{10,{<0.118.0>,#Ref<0.0.4.9>}}]}
%% gf.erl:115:<0.118.0>: Allocate frequency 20 and report.
%% gf.erl:116:<0.118.0>: {ok,20}
%% gf.erl:117:<0.118.0>: {[11,12,13,14,15],
%%  [{20,{<0.118.0>,#Ref<0.0.2.784>}},{10,{<0.118.0>,#Ref<0.0.4.9>}}]}
%% gf.erl:119:<0.118.0>: Deallocate frequencies 10 & 10 and report.
%% gf.erl:120:<0.118.0>: ok
%% gf.erl:121:<0.118.0>: ok
%% gf.erl:122:<0.118.0>: {[20,10,11,12,13,14,15],[]}
%% gf.erl:124:<0.118.0>: Stop the server.
%% gf.erl:125:<0.118.0>: ok
