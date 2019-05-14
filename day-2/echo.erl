-module(echo).
-export([start/0, stop/0, print/1, listen/0]).

start() ->
    Pid = spawn(echo, listen, []),
    register(server, Pid),
    ok.

stop() ->
    server ! stop,
    ok.

print(Term) ->
    server ! {print, Term},
    ok.

listen() ->
    receive
	stop -> 
	    true;
	{print, Term} -> 
	    io:format("Server says: ~p~n", [Term]),
	    listen()
    end.
