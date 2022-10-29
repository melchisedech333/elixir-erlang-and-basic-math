
% IHS <3

-module(app).
-export([app/0]).

app() ->
    A = division:divisible_by(5, 3),
    io:format("value: ~w~n", [ A ]).


