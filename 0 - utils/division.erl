
-module(division).
-export([divisible_by/2]).

divisible_by(Value, Number) ->
    Value rem Number == 0.


