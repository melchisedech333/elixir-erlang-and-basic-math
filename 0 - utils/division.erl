
-module(division).
-export([divisible_by/2]).

divisible_by(Value, Number) ->
    case Number of
        2 -> Value rem 2 == 0;
        3 -> divisible_by_3(Value);
        4 -> divisible_by_4(Value);
        5 -> divisible_by_5(Value);
        6 -> divisible_by_6(Value);
        7 -> divisible_by_7(Value);
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


divisible_by_7(Value) ->
    List = utils:number_to_list(Value),
    Blocks = utils:split_list_in_blocks(List, 3, 0),

    io:format("Blocks: ~w~n", [ Blocks ]),

    false.


% fn divisible_by_7(value :i32) -> bool {
%     let number = value.to_string();
%     let len = number.len() - 1;
%     let mut s = String::from("");
%     let mut counter1 = 0;
%     let mut counter2 = 0;
%     let mut numbers :Vec<i32> = Vec::new();

%     // Separate numbers into 3-digit blocks and divide by 7.
%     for item in number.chars().rev() {
%         let num = item.to_string();
%         s.push_str(&num[..]);

%         if counter1 == 2 || counter2 == len {
%             let nums :String = s.chars().rev().collect();
%             let num : i32 = nums.to_string().parse()
%                 .expect("Invalid number.");
%             s.clear();
%             counter1 = 0;
%             numbers.push(num % 7);
%         } else {
%             counter1 += 1;
%         }

%         counter2 += 1;
%     }

%     // Processes the value of the remainder of the division and their signs.
%     let mut numbers :Vec<i32> = numbers.into_iter().rev().collect();
%     let mut flag = true;
%     let mut total = 0;

%     for num in &mut numbers {
%         let mut number = String::from("");

%         if flag == true {
%             number.push_str(&format!("{}", num));
%             flag = false;
%         } else {
%             number.push_str(&format!("-{}", num));
%             flag = true;
%         }

%         let number :i32 = number.to_string()
%             .parse().expect("Invalid number.");
%         total += number;
%     }

%     total % 7 == 0
% }


