-module(my_db).
-export([start/1]

start() ->
    register(my_db, spawn(my_db, init, [])).

stop() ->
    call({stop}).

write(Key, Element) ->
    call({write, Key, Element}).

delete(Key) ->
    call({delete, Key}).

read(Key) ->
    call({read, Key}).

match(Element) ->
    match({match, Element}).

call(Message) ->
    my_db ! Message,
    receive
	{reply, Response} ->
	    Response
    end.

init() ->
    DbRef = db:new(),
    loop(DbRef).

loop(DbRef) ->
    receive
	{stop} ->
	    db:destroy(DbRef);
	{write, Key, Element} ->
	    NewDbRef = db:write(Key, Element, DbRef),
	    loop(NewDbRef);
	{delete, Key} ->
	    NewDbRef = db:delete(Key, DbRef),
	    loop(NewDbRef);
	{read, Key} ->
	    NewDbRef = db:read(Key)

reply(Pid, Response)
