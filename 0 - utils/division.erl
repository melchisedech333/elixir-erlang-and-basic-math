
-module(division).
-export([divisible_by/2]).

divisible_by(Value, Number) ->
    case Number of
        2 -> Value rem Number == 0;
        3 -> divisible_by_3(Value);
        4 -> divisible_by_4(Value);
        5 -> false;
        6 -> false;
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
    Last = utils:get_last_items(List, 2),

    io:format("Last elements: ~w~n", [ Last ]),

    true;

divisible_by_4(Value) ->
    io:format("invalid number (divisible by 4).~n"), 
    false.

% fn divisible_by_4(value: i32) -> bool {
%     let number = value.to_string();
%     let offset;

%     if number.len() >= 2 {
%         offset = number.len() - 2;
%     } else {
%         offset = number.len() - 1;
%     }

%     let part = &number[offset..];
%     let number :i32 = part.to_string().trim()
%         .parse().expect("Enter a valid number.");
    
%     number % 4 == 0
% }


