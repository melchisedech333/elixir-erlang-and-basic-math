
%
% Iesus Hominum Salvator <3
%

-module(division).
-export([divisible_by/2]).

divisible_by(Value, Number) ->
    case Number of
        2  -> Value rem 2 == 0;
        3  -> divisible_by_3(Value);
        4  -> divisible_by_4(Value);
        5  -> divisible_by_5(Value);
        6  -> divisible_by_6(Value);
        7  -> divisible_by_7(Value);
        8  -> false;
        9  -> false;
        10 -> false;
        11 -> false;
        12 -> false;
        15 -> false;
        25 -> false;
        
        _Else -> 
            io:format("Division by ~w not supported.~n", [ Number ]),
            false
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

divisible_by_4(_) ->
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


divisible_by_7(Value) ->
    List = utils:number_to_list(Value),
    Blocks = utils:split_list_in_blocks(List, 3, 0),

    Rest = utils:map_elements(fun(X) -> 
        X rem 7 
    end, Blocks),

    process_rest_calc(Rest, 1, 0, 0, 0, 0) rem 7 == 0.

process_rest_calc([ H | R], Signal, Last, First, Result, FirstCalc) ->
    if 
        % First call.
        First == 0 ->
            if  Signal == 0 -> process_rest_calc(R, 1, H, 1, 0, 0);
                true        -> process_rest_calc(R, 0, H, 1, 0, 0)
            end;

        % process calculations.
        true ->
            if 
                FirstCalc == 0 -> % First calculation.
                    Ret = process_rest_signal(Signal, Last, H, Result, 0),

                    if  Signal == 0 -> process_rest_calc(R, 1, H, 1, Ret, 1);
                        true ->        process_rest_calc(R, 0, H, 1, Ret, 1)
                    end;
                
                true -> % Remaining calculations.
                    Ret = process_rest_signal(Signal, Last, H, Result, 1),

                    if  Signal == 0 -> process_rest_calc(R, 1, H, 1, Ret, 1);
                        true ->        process_rest_calc(R, 0, H, 1, Ret, 1)
                    end
            end
    end;

process_rest_calc([], _, _, _, Result, _) ->
    Result.

process_rest_signal(Signal, Last, Current, Result, First) ->
    if 
        First == 0 ->
            if 
                Signal == 0 -> Result + (Last - Current); % Negative values.
                Signal == 1 -> Result + (Last + Current)  % Positive values.
            end;
        true ->
            if 
                Signal == 0 -> Result - Current; % Negative values.
                Signal == 1 -> Result + Current  % Positive values.
            end
    end.


