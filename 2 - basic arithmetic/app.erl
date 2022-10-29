
% Iesus Honinum Salvator <3

-module(app).
-export([ihs/0]).

ihs () -> fiat(), lux().
fiat() -> ok .
lux () ->

    A = division:divisible_by(5, 3),
    io:format("value: ~w~n", [ A ]).


