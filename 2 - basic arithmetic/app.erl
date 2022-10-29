
% Iesus Hominum Salvator <3

-module(app).
-export([ihs/0]).

ihs () -> fiat(), lux().
fiat() -> ok .
lux () ->

    io:format("Operations:~n~n"),
    operations(),

    io:format("Signals:~n~n"),
    signals(),

    io:format("Addition:~n~n"),
    addition(),

    io:format("Multiplication:~n~n"),
    multiplication(),

    io:format("Division:~n~n"),
    division().

operations() ->
    A = 10,
    B = 20,

    io:format("\t~w ~c ~w = ~w~n", [ A, $+, B, calc(A, B, $+) ]),
    io:format("\t~w ~c ~w = ~w~n", [ A, $-, B, calc(A, B, $-) ]),
    io:format("\t~w ~c ~w = ~w~n", [ A, $*, B, calc(A, B, $*) ]),
    io:format("\t~w ~c ~w = ~w~n", [ A, $/, B, calc(A, B, $/) ]),
    io:format("~n~n").

calc(X, Y, C) when C == $+ ->
    X + Y;
calc(X, Y, C) when C == $- ->
    X - Y;
calc(X, Y, C) when C == $* ->
    X * Y;
calc(X, Y, C) when C == $/ ->
    X / Y.

signals() ->
    Show = fun(A, B) -> io:format("\t~5w = ~5w~n", [ A, B ]) end,
    
    Show(
        (4) + (-2) + (1) - (1) ,
         4     -2  +  1  -  1  
    ),

    Show(
        (-6) - (10) + (-3) ,
         -6  -  10     -3
    ),

    Show(
        (10) + (-5) - (-10) ,
         10     -5  +   10
    ),

    Show(
        (15) + (-10) - (4) + ( 20 - 30) + (-1),
         15     -10  -  4  + (-10     )    -1
    ),

    Show(
        200 + (-23) - (-30) + 78 + (50 / 100) - 10,
        200    -23  +   30  + 78 + (50 / 100) - 10
    ),

    io:format("~n~n").

addition() ->
    io:format("\tCommutative: ~w = ~w = ~w~n", [
        10 + 20 + 30,
        30 + 20 + 10,
        20 + 10 + 30
    ]),

    io:format("\tAssociative: ~w = ~w = ~w = ~w = ~w = ~w~n", [
        (10 +  20) + 30,
        (30 +  20) + 10,
        (20 +  10) + 30,
         10 + (20  + 30),
         30 + (10  + 20),
         20 + (30  + 10)
    ]),

    io:format("~n~n").

multiplication() ->
    io:format("\tCommutative: ~w = ~w = ~w~n", [
        10 * 20 * 30 * 40,
        40 * 30 * 20 * 10,
        30 * 10 * 40 * 20
    ]),

    io:format("\tAssociative: ~w = ~w = ~w~n", [
        10 *  20 * (30  * 40),
        40 *  30 * (20  * 10),
        30 * (20 *  40) * 10
    ]),

    io:format("\tDistributive: ~w = ~w~n", [
         10 * (20 + 30),
        (10 * 20) + (10 * 30)
    ]),

    io:format("\tNeutral element: ~w, ~w, ~w, ~w~n", [
        10 * 1,
        20 * 1,
        1 * 10,
        1 * 20
    ]),

    io:format("\tAnnulment: ~w, ~w, ~w, ~w~n~n", [
        10 * 0,
        20 * 0,
        0 * 10,
        0 * 20
    ]),

    Mult = fun(X, Y) -> 
        io:format("\t~w * ~w = ~w~n", [ X, Y, X * Y ]) end,

    Mult(0.2, 0.3),
    Mult(1.2, 0.4),
    Mult(-12.0, 5.0),
    Mult(5.0, -12.0),
    Mult(15.0, 0.02),
    Mult(15.0, 0.2),
    Mult(0.2, 0.3),
    Mult(2.0 / 10.0, 3.0 / 10.0),
    Mult(1.2, 0.4),
    Mult(12.0 / 10.0, 4.0  / 10.0),
    Mult(29.01, 4.0),
    Mult(2901.0 / 100.0, 40.0   / 10.0),

    io:format("~n~n").

division() ->
    Quotient = 4.0,
    Divider = 2.0,
    Rest = 1.0,
    Dividend = Divider * Quotient + Rest,

    io:format("\t~w = ~w x ~w + ~w~n", [ Dividend, Divider, Quotient, Rest ]),
    io:format("\t~w / ~w = ~w~n", [ Dividend, Divider, Dividend / Divider ]),

    Div = fun(X, Y) -> 
        io:format("\t~w / ~w = ~w~n", [ X, Y, X / Y ]) end,

    Div(433.0, 6.0),
    Div(8.0, 1230.0),
    
    io:format("~n~n").


