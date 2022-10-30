
% Iesus Hominum Salvator <3

-module(app).
-export([ihs/0]).

ihs () -> fiat(), lux().
fiat() -> ok .
lux () ->

    Options = [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 25 ],
    Value   =   2835,

    Elms = fun(Value, X) -> 
        Result = division:divisible_by(Value, X),
        io:format("~w is divisible by ~w: ~w~n", [ Value, X, Result ])
    end,

    access_elements(Elms, Value, Options).


access_elements(Fun, Value, [First | Rest]) ->
    Fun(Value, First),
    access_elements(Fun, Value, Rest);
access_elements(Fun, Value, []) ->
    ok.


