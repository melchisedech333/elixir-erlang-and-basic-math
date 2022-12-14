
%
% Functions used throughout the application to cooperate in solving 
% a variety of problems.
% 
% Iesus Hominum Salvator <3
%

-module(utils).
-export([map_elements/2, access_elements_pvalue/3, number_to_list/1, 
         sums_elements/1, get_list_size/1, get_first_items/2, get_last_items/2,
         reverse_list/1, list_numbers_to_string/1, split_list_in_blocks/3]).

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

map_elements(_, []) -> 
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
access_elements_pvalue(_, _, []) ->
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
%% Returns the first N elements of a list.
%% 
%%   Elements = get_first_items([1, 2, 3], 2)
%% 

get_first_items(List, Total) ->
    get_first_items(List, Total, 0, []).

get_first_items([ H | R], Total, Count, List) ->
    if
        Count < Total ->
            get_first_items(R, Total, Count + 1, [ H | List ]);
        true ->
            get_first_items(R, Total, Count + 1, List)
    end;

get_first_items([], _, _, List) ->
    reverse_list(List).


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

get_last_items([_ | Rest], Offset, Index, List) ->
    get_last_items(Rest, Offset, Index + 1, List);

get_last_items([], _, _, List) ->
    reverse_list(List).


adjust_amount(Amount, Size) when Amount > Size ->
    Size;
adjust_amount(Amount, _) ->
    Amount.


%%
%% Returns the number of elements in a list.
%%
%%   Size = get_list_size([1, 2, 3])
%%

get_list_size(List) ->
    get_list_size(List, 0).

get_list_size([_ | Rest], Count) ->
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


%% 
%% Divides a list of numbers into groups containing N-elements.
%% You can adjust the alignment of the parts.
%% 
%% BlockSize - integer: 
%%   The size of the digit block.
%% 
%% Adjust - integer: 
%%   Adjusts the alignment of blocks, if necessary.
%%   
%%   0 = Right - Align the blocks to the right -> [ 12,  234, 234 ].
%%   1 = Left  - Align the blocks to the left  -> [ 122, 342, 34  ].
%% 
%% Example of use:
%% 
%%   Blocks = split_list_in_blocks([1, 2, 3, 4, 5, 6, 7, 8], 3, 0),
%%   io:format("Blocks: ~w~n", [ Blocks ]) 
%% 
%% Output:
%% 
%%   Blocks: [12, 345, 678]
%% 

split_list_in_blocks(_, BlockSize, _) when BlockSize =< 0 ->
    io:format("Invalid block size.~n"), [];

split_list_in_blocks(List, BlockSize, Adjust) ->
    Size = get_list_size(List),

    if
        BlockSize > Size ->
            io:format("Invalid block size.~n"), [];
        true ->
            process_split_blocks(List, Size, BlockSize, Adjust)
    end.

process_split_blocks(List, Size, BlockSize, Adjust) ->
    V = (Size rem BlockSize == 0),

    if 
        V == true -> 
            process_stable_blocks(List, BlockSize, 1, [], []);
        true ->
            process_segmented_blocks(List, Size, BlockSize, Adjust)
    end.

process_stable_blocks([ H | R ], BlockSize, Count, Block, Blocks) ->
    Size = get_list_size(Block),

    if 
        Size == BlockSize ->
            BlockItem = prepare_block_number(Block),
            process_stable_blocks(
                R, BlockSize, Count + 1, [ H ], [ BlockItem | Blocks]);
        true ->
            process_stable_blocks(
                R, BlockSize, Count + 1, [ H | Block ], Blocks)
    end;
    
process_stable_blocks([], _, _, Block, Blocks) ->
    BlockItem = prepare_block_number(Block),
    reverse_list([ BlockItem | Blocks ]).

prepare_block_number(List) ->
    A = reverse_list(List),
    B = list_numbers_to_string(A),
    { Number, _ } = string:to_integer(B),
    Number.

process_segmented_blocks(List, Size, BlockSize, Adjust) ->
    Mod    = Size rem BlockSize,
    Offset = trunc(Size / BlockSize),
    Check  = (Offset * BlockSize) + Mod,

    if 
        Check == Size ->

            %% Right adjust.
            if Adjust == 0 ->
                A = (Offset * BlockSize),
                B = get_last_items(List, A),
                C = get_first_items(List, Mod),
                Number = prepare_block_number(reverse_list(C)),
                Blocks = process_stable_blocks(B, BlockSize, 1, [], []),
                [ Number | Blocks ];
                
            %% Left adjust.
            true ->
                A = (Offset * BlockSize),
                B = get_first_items(List, A),
                C = get_last_items(List, Mod),
                Number = prepare_block_number(reverse_list(C)),
                Blocks = process_stable_blocks(B, BlockSize, 1, [], []),
                Blocks ++ [ Number ]
            end;
        true ->
            io:format("invalid number to convert.~n"), []
    end.


