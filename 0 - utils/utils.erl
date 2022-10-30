
-module(utils).
-export([map_elements/2, access_elements_pvalue/3, 
         number_to_list/1, sums_elements/1]).

%% 
%% Access elements of a list, making it possible to change their 
%% values, through an anonymous function. The function returns a 
%% new list with the changed values. Below is an example of usage.
%% 
%%   Mod = fun(X) -> X * 2 end,
%%   N = map_elements(Mod, [ 1, 2, 3 ]),
%%

map_elements(Fun, [First | Rest]) -> 
    [Fun(First) | map_elements(Fun, Rest)];

map_elements(Fun, []) -> 
    [].


%%
%% Access elements of a list, through an anonymous function.
%% Allowing the passing of an external value, for it to be 
%% used within the anonymous function. Below is an example of usage.
%%   
%%   Elms = fun(Value, X) -> 
%%       io:format("Value: ~w, List element: ~w~n", [ Value, X ])
%%   end,
%%   
%%   access_elements_pvalue(Elms, 333, [ 1, 2, 3 ]).
%%

access_elements_pvalue(Fun, Value, [First | Rest]) ->
    Fun(Value, First),
    access_elements_pvalue(Fun, Value, Rest);
access_elements_pvalue(Fun, Value, []) ->
    ok.


%% 
%% Converts a number to a list of numbers. The list returned by 
%% the function is only composed of numbers, not characters from 
%% the ASCII table. Below is an example of usage.
%% 
%%   List = number_to_list(333),
%% 

number_to_list(Value) ->
    Items = integer_to_list(Value),

    Adjust = fun(X) -> 
        X - 48 % ASCII table.
    end,

    map_elements(Adjust, Items).


%% 
%% Perform the sum of all elements of a list. The function 
%% returns only an integer. Below is an example of usage.
%% 
%%   Total = sums_elements(List),
%% 

sums_elements(List) ->
    sums_elements(List, 0).

sums_elements([First | Rest], Number) ->
    sums_elements(Rest, First + Number);

sums_elements([], Number) ->
    Number.


