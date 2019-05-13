-module(exps).
-export([sum/1, sum_interval/2, create/1, reverse_create/1, print/1, even_print/1]).

sum(0) ->
    0;
sum(N) ->
    N+sum(N-1).

sum_interval(N, M) when N == M -> 
    N;
sum_interval(N, M) when N < M -> 
    N + sum_interval(N+1, M).

reverse_create(0) -> 
    [];
reverse_create(N) ->
    [N | reverse_create(N-1)].

create(N, N) ->
    [N];
create(N, M) ->
    [N | create(N+1, M)].
create(N) ->
    create(0, N).

io_print(N) ->
    io:format("Number: ~p~n", [N]).

print(1) ->
    io_print(1);
print(N) ->
    print(N-1),
    io_print(N).

even_print(1) ->
    true;
even_print(N) when N rem 2 == 0 ->
    even_print(N-1),
    io_print(N);
even_print(N) ->
    even_print(N-1).
