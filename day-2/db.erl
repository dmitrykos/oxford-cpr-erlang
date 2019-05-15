-module(db).
-export([new/0, write/3, delete/2, read/2, match/2, destroy/1]).

new() ->
    [].

write(Key, Element, DbRef) ->
    [{Key, Element} | DbRef].

% Assumes multiple elements with the same key can exist
delete(_, []) ->
    [];
delete(Key, [{Key, _} | T]) ->  
    delete(Key, T);
delete(Key, [H|T]) ->
    [H|delete(Key, T)].

read(_, []) ->
    {error, instance};
read(Key, [{Key, Element} | _]) ->
    {ok, Element};
read(Key, [_|T]) ->
    read(Key, T).

match(_, []) ->
    [];
match(Element, [{Key, Element} | T]) ->
    [Key | match(Element, T)];
match(Element, [_|T]) ->
    match(Element, T).

destroy(_DbRef) ->
    [].
