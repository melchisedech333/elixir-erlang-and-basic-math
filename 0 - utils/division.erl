
-module(division).
-export([divisible_by/2]).

divisible_by(Value, Number) ->
    case Number of
        2 -> Value rem Number == 0;
        3 -> divisible_by_3(Value);
        4 -> false;
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

    io:format("Total: ~w~n", [ Total ]),

    false.


% fn divisible_by_3(value: i32) -> bool {
%     let number = value.to_string();
%     let mut total = 0;

%     for item in number.chars() {
%         let num :i32 = 
%             item.to_string().trim()
%                 .parse().expect("Enter a valid number.");
        
%         total += num;
%     }

%     total % 3 == 0
% }


