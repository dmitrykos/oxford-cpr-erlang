-module(sup).
-export([start/1, start_child/4, stop/1, init_sup/0]).

start(SupName) ->
    Pid = spawn(sup, init_sup, []),
    register(SupName, Pid),
    {ok, Pid}.

start_child(Sup, Mod, Func, Args) ->
    Sup ! {self(), spawn, Mod, Func, Args}, 
    receive
	{reply, Response} ->
	    Response
    end.

stop(Sup) ->
    Sup ! {self(), stop},
    receive
	{reply, Response} ->
	    Response
    end.

%% Supervisor

init_sup() ->
    process_flag(trap_exit, true),
    loop([]).
    
loop(Children) ->
    MaxRestarts = 5,
    io:format("Children ~p~n", [Children]), 
    receive
	{Pid, spawn, Mod, Func, Args} -> 
	    NewPid = spawn_link(Mod, Func, Args),
	    register(Mod, NewPid),
	    Pid ! {reply, {ok, NewPid}},
	    NewChildren = [child(NewPid, Mod, Func, Args, MaxRestarts) | Children],
	    loop(NewChildren);
	
	{Pid, stop} ->
	    Pid ! {reply, ok};
	
	{'EXIT', Pid, Reason} ->
	    io:format("~p crashed due to: ~p~n", [Pid, Reason]),	    
	    Child = find(Pid, Children),
	    NewChildren = restart(Child, Children),
	    loop(NewChildren)
    end.

%% Restarting processes
restart({Pid, _, _, _, 0}, Children) ->
    io:format("Not respawning ~p, max restarts reached~n", [Pid]),
    remove(Pid, Children);
restart(Child, Children) ->
    {Pid, _, _, _, _} = Child, 
    NewPid = respawn(Child),
    NewChild = decrement_counter(update_pid(Child, NewPid)),
    [NewChild | remove(Pid, Children)].

respawn({Pid, Mod, Func, Args, _}) ->
    io:format("Respawned ~p~n", [Pid]),
    NewPid = spawn_link(Mod, Func, Args),
    register(Mod, NewPid),
    NewPid.
    

%% Operations on Child tuple
child(Pid, Mod, Func, Args, Restarts) ->
    {Pid, Mod, Func, Args, Restarts}.

update_pid({_Pid, Mod, Func, Args, Restarts}, NewPid) ->
    {NewPid, Mod, Func, Args, Restarts}.

decrement_counter({Pid, Mod, Func, Args, Restarts}) ->
    {Pid, Mod, Func, Args, Restarts-1}.


%% Operations on Children list
remove(_, []) ->
    [];
remove(Pid, [{Pid, _, _, _, _}|T]) ->
    T;
remove(Pid, [H|T]) ->
    [H|remove(Pid, T)].
	      

find(_, []) ->
    error; % unhandled
find(Pid, [{Pid, Mod, Func, Args, Restarts}|_]) ->    
    {Pid, Mod, Func, Args, Restarts};
find(Pid, [_|T]) -> 
    find(Pid, T).
