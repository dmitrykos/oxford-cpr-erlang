-module(process_ring).
-export([start/3, start/4, start_first/3]).

start(N, M, Msg) ->
    spawn(process_ring, start_first, [N, M, Msg]),
    ok.

start_first(N, M, Msg) ->
    io:format("Started ~p...~n", [N]),
    Root = self(),
    Pid = spawn(process_ring, start, [N-1, M, Root, Msg]),
    listen(Pid, M).

start(1, M, Root, Msg) ->
    io:format("Started 1, sending first message to root ~p now...~n", [Root]),
    Root ! {msg, Msg},
    listen(Root, M);
start(N, M, Root, Msg) ->
    io:format("Started ~p...~n", [N]),
    Pid = spawn(process_ring, start, [N-1, M, Root, Msg]),
    listen(Pid, M).

listen(Next_pid, 0) ->
    io:format("Parent of ~p terminating...~n", [Next_pid]),
    ok;

listen(Next_pid, M) ->
    receive
	{msg, Info} ->
	    io:format("[~p] Received msg ~p: ~p, fowarding to ~p...~n", [self(), M, Info, Next_pid]),
	    Next_pid ! {msg, Info},
	    listen(Next_pid, M-1);
	{quit, Sender} ->
	    Sender ! ack,
	    Next_pid ! {quit, self()},
	    true
    end.
