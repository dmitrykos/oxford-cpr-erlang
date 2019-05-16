-module(mutex).
-export([start/0, wait/0, signal/0, init/0, 
	 test_lock/0, test_bad_lock/0, test_bad_lock_two/0,
	 spawn_tester/0, spawn_bad_tester/0]).

start() ->
    register(mutex, spawn(mutex, init, [])).

wait() ->   
    mutex ! {self(), acquire},
    receive
	{reply, ok} ->
	    ok
    end.

signal() ->
    mutex ! {self(), release},
    receive
	{reply, ok} ->
	    ok
    end.

%% FSM

init() ->
    free().

free() ->
    receive
	{Pid, acquire} ->
	    Pid ! {reply, ok},
	    io:format("[MUTEX] ~p acquired lock~n", [Pid]),
	    busy(Pid)
    end.

busy(LockHolder) ->
    receive
	{LockHolder, release} ->
	    LockHolder ! {reply, ok},
	    io:format("[MUTEX] ~p released lock~n", [LockHolder]),
	    free()
    end.


% Test helper
spawn_tester() ->
    spawn(mutex, test_lock, []).

spawn_bad_tester() ->
    spawn(mutex, test_bad_lock, []),
    spawn(mutex, test_bad_lock_two, []).

test_bad_lock() ->
    mutex:wait(), 
    io:format("[~p] Acquire & terminate!~n", [self()]),
    exit(forced_exit).

test_bad_lock_two() ->
    mutex:wait(), 
    io:format("[~p] Acquire & terminate!~n", [self()]).

test_lock() -> 
    mutex:wait(), 
    io:format("[~p] Acquired!~n", [self()]), 
    mutex:signal(), 
    io:format("[~p] Release & terminate!~n", [self()]),
    ok. 

