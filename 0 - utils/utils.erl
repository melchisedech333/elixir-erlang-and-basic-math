
-module(utils).
-export([map_elements/2, access_elements_pvalue/3, number_to_list/1, 
         sums_elements/1, get_list_size/1, get_last_items/2,
         reverse_list/1, list_numbers_to_string/1]).

%% 
%% Access elements of a list, making it possible to change their 
%% values, through an anonymous function. The function returns a 
%% new list with the changed values. 
%% 
%%   Mod = fun(X) -> X * 2 end,
%%   N = map_elements(Mod, [ 1, 2, 3 ])
%%

map_elements(Fun, [First | Rest]) -> 
    [Fun(First) | map_elements(Fun, Rest)];

map_elements(Fun, []) -> 
    [].


%%
%% Access elements of a list, through an anonymous function.
%% Allowing the passing of an external value, for it to be 
%% used within the anonymous function. 
%%   
%%   Elms = fun(Value, X) -> 
%%       io:format("Value: ~w, List element: ~w~n", [ Value, X ])
%%   end,
%%   
%%   access_elements_pvalue(Elms, 333, [ 1, 2, 3 ])
%%

access_elements_pvalue(Fun, Value, [First | Rest]) ->
    Fun(Value, First),
    access_elements_pvalue(Fun, Value, Rest);
access_elements_pvalue(Fun, Value, []) ->
    ok.


%% 
%% Converts a number to a list of numbers. The list returned by 
%% the function is only composed of numbers, not characters from 
%% the ASCII table. 
%% 
%%   List = number_to_list(333)
%% 

number_to_list(Value) ->
    Items = integer_to_list(Value),

    Adjust = fun(X) -> 
        X - 48 % ASCII table.
    end,

    map_elements(Adjust, Items).


%% 
%% Perform the sum of all elements of a list. The function 
%% returns only an integer. 
%% 
%%   Total = sums_elements(List)
%% 

sums_elements(List) ->
    sums_elements(List, 0).

sums_elements([First | Rest], Number) ->
    sums_elements(Rest, First + Number);

sums_elements([], Number) ->
    Number.


%% 
%% Returns the last N elements of a list.
%%
%%   Elements = get_last_items([1, 2, 3], 2)
%%

get_last_items(StartList, StartAmount) ->
    if 
        StartAmount =< 0 ->
            io:format("Invalid amount.~n"),
            false;
        
        true ->
            Size   = get_list_size(StartList),
            Amount = adjust_amount(StartAmount, Size),
            Offset = (Size - Amount) + 1,
            
            get_last_items(StartList, Offset, 1, [])
    end.


get_last_items([Head | Rest], Offset, Index, List) when Index >= Offset ->
    get_last_items(Rest, Offset, Index + 1, [ Head | List ]);

get_last_items([Head | Rest], Offset, Index, List) ->
    get_last_items(Rest, Offset, Index + 1, List);

get_last_items([], Offset, Index, List) ->
    reverse_list(List).


adjust_amount(Amount, Size) when Amount > Size ->
    Size;
adjust_amount(Amount, Size) ->
    Amount.


%%
%% Returns the number of elements in a list.
%%
%%   Size = get_list_size([1, 2, 3])
%%

get_list_size(List) ->
    get_list_size(List, 0).

get_list_size([First | Rest], Count) ->
    get_list_size(Rest, Count + 1);

get_list_size([], Count) ->
    Count.


%%
%% Reverses the order of elements in a list. 
%% 
%%   List = reverse_list([1, 2, 3])
%% 

reverse_list(List) ->
    reverse_list(List, []).

reverse_list([Head | Rest], List) ->
    reverse_list(Rest, [ Head | List]);

reverse_list([], List) ->
    List.


%%
%% Converts a list of integers to the equivalent string 
%% representation according to the ASCII table.
%%
%%   String = list_numbers_to_string([1, 2, 3])
%%

list_numbers_to_string(List) ->

    Convert = fun(X) -> 
        X + 48 % ASCII table.
    end,

    map_elements(Convert, List).


