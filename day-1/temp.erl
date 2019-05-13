-module(temp).
-export([f2c/1, c2f/1, convert/1]).

f2c(Fahrenheit) ->
    (5/9)*(Fahrenheit-32).

c2f(Celsius) ->
    ((9/5)*Celsius) + 32.

convert({c, Value}) ->
    {f, c2f(Value)};
convert({f, Value}) ->
    {c, f2c(Value)}.
