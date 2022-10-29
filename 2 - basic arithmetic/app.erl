
% Iesus Hominum Salvator <3

-module(app).
-export([ihs/0]).

ihs () -> fiat(), lux().
fiat() -> ok .
lux () ->

    io:format("Operations:~n~n"),
    operations().

operations() ->
    A = 10,
    B = 20,

    io:format("\t~w ~c ~w = ~w~n", [ A, $+, B, calc(A, B, $+) ]),
    io:format("\t~w ~c ~w = ~w~n", [ A, $-, B, calc(A, B, $-) ]),
    io:format("\t~w ~c ~w = ~w~n", [ A, $*, B, calc(A, B, $*) ]),
    io:format("\t~w ~c ~w = ~w~n", [ A, $/, B, calc(A, B, $/) ]).

%% Define as cláusulas da função.
calc(X, Y, C) when C == $+ ->
    X + Y;
calc(X, Y, C) when C == $- ->
    X - Y;
calc(X, Y, C) when C == $* ->
    X * Y;
calc(X, Y, C) when C == $/ ->
    X / Y.


