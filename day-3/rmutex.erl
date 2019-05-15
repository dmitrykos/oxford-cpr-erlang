-module(rmutex).
-export([start/0, wait/0, signal/0, init/0, 
	 test_lock/0, test_bad_lock/0, test_bad_lock_two/0,
	 spawn_tester/0, spawn_bad_tester/0]).

start() ->
    register(rmutex, spawn(rmutex, init, [])).

wait() ->   
    rmutex ! {self(), acquire},
    receive
	{reply, ok} ->
	    ok
    end.

signal() ->
    rmutex ! {self(), release},
    receive
	{reply, ok} ->
	    ok
    end.

%% FSM

init() ->
    process_flag(trap_exit, true),
    free().

free() ->
    receive
	{Pid, acquire} ->
	    Pid ! {reply, ok},
	    print_acquire_lock(Pid), %% timer:sleep(2000),
	    try_link(Pid), 
	    busy(Pid)
    end.

busy(LockHolder) ->
    receive
	{LockHolder, release} ->
	    LockHolder ! {reply, ok},
	    print_release_lock(LockHolder),
	    unlink(LockHolder),  
	    free();
	{'EXIT', LockHolder, Reason} ->
	    print_expire_lock(LockHolder, Reason),
	    unlink(LockHolder),  
	    free()
    end.

try_link(Pid) ->
    try link(Pid)
    catch
	_ -> print_expire_lock(Pid, failed_to_link),
	     free()
    end.    

print_acquire_lock(LockHolder) ->
    io:format("[MUTEX] ~p acquired lock~n", [LockHolder]).

print_release_lock(LockHolder) ->
    io:format("[MUTEX] ~p released lock~n", [LockHolder]).

print_expire_lock(LockHolder, Reason) ->
    io:format("[MUTEX] ~p exited: ~p. Lock released.~n", [LockHolder, Reason]).    

% Test helpers
spawn_tester() ->
    spawn(rmutex, test_lock, []).

spawn_bad_tester() ->
    spawn(rmutex, test_bad_lock, []),
    spawn(rmutex, test_bad_lock_two, []).

test_bad_lock() ->
    rmutex:wait(), 
    io:format("[~p] Acquire & terminate!~n", [self()]),
    exit(forced_exit).

test_bad_lock_two() ->
    rmutex:wait(), 
    io:format("[~p] Acquire & terminate!~n", [self()]).

test_lock() -> 
    rmutex:wait(), 
    io:format("[~p] Acquired!~n", [self()]), 
    rmutex:signal(), 
    io:format("[~p] Release & terminate!~n", [self()]),
    ok. 

