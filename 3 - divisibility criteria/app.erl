
% Iesus Hominum Salvator <3

-module(app).
-export([ihs/0]).

ihs () -> fiat(), lux().
fiat() -> ok .
lux () ->

    Options = [ 2, 3, 4, 5, 6, 7, 8, 9, 10 ],
    Value   =   3450,

    Elms = fun(Number, X) -> 
        Result = division:divisible_by(Number, X),
        io:format("~w is divisible by ~w: ~w~n", [ Number, X, Result ])
    end,

    utils:access_elements_pvalue(Elms, Value, Options).


