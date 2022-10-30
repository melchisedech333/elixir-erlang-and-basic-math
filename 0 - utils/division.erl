
-module(division).
-export([divisible_by/2]).

divisible_by(Value, Number) ->
    case Number of
        2 -> Value rem 2 == 0;
        3 -> divisible_by_3(Value);
        4 -> divisible_by_4(Value);
        5 -> divisible_by_5(Value);
        6 -> divisible_by_6(Value);
        7 -> false;
        8 -> false;
        9 -> false;
        10 -> false;
        11 -> false;
        12 -> false;
        15 -> false;
        25 -> false
    end.


divisible_by_3(Value) ->
    List  = utils:number_to_list(Value),
    Total = utils:sums_elements(List),
    Total rem 3 == 0.


divisible_by_4(Value) when Value =< 99 -> 
    Value rem 4 == 0;

divisible_by_4(Value) when Value  > 99 -> 
    List = utils:number_to_list(Value),
    Elem = utils:get_last_items(List, 2),
    Last = utils:list_numbers_to_string(Elem),
    { Num, _ } = string:to_integer(Last),
    Num rem 4 == 0;

divisible_by_4(Value) ->
    io:format("invalid number (divisible by 4).~n"), 
    false.


divisible_by_5(Value) ->
    List = utils:number_to_list(Value),
    Elem = utils:get_last_items(List, 1),
    Last = utils:list_numbers_to_string(Elem),
    { Num, _ } = string:to_integer(Last),

    if
        Num == 5 -> true;
        Num == 0 -> true;
        true -> false
    end.


divisible_by_6(Value) ->
    (Value rem 2 == 0) and (divisible_by_3(Value) == true).


