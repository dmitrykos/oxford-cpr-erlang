-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1, loop/1, init/0]).


%% Client
start() ->
    register(my_db, spawn(my_db, init, [])),
    ok.

stop() ->
    call({self(), stop}).

write(Key, Element) ->
    call({self(), write, Key, Element}).

delete(Key) ->
    call({self(), delete, Key}).

read(Key) ->
    call({self(), read, Key}).

match(Element) ->
    call({self(), match, Element}).

call(Message) ->
    my_db ! Message,
    receive
	{reply, Response} ->
	    Response
    end.

%% Server
init() ->
    DbRef = db:new(),
    loop(DbRef).

loop(DbRef) ->
    receive
	{Pid, stop} ->
	    db:destroy(DbRef),
	    Pid ! {reply, ok};
	
	{Pid, write, Key, Element} ->
	    NewDbRef = db:write(Key, Element, DbRef),
	    Pid ! {reply, ok},
	    loop(NewDbRef);
	
	{Pid, delete, Key} ->
	    NewDbRef = db:delete(Key, DbRef),
	    Pid ! {reply, ok},
	    loop(NewDbRef);
	
	{Pid, read, Key} ->
	    Result = db:read(Key, DbRef),
	    Pid ! {reply, Result},
	    loop(DbRef);
	
	{Pid, match, Element} ->
	    Result = db:match(Element, DbRef),
	    Pid ! {reply, Result},
	    loop(DbRef)
    end.

