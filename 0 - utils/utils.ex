
defmodule Utils do

    # 
    # Access elements of a list, making it possible to change their 
    # values, through an anonymous function. The function returns a 
    # new list with the changed values. 
    # 
    #   mod = fn(x) -> x * 2 end
    #   n = map_elements(mod, [ 1, 2, 3 ])
    #

    def map_elements(fun, [ first | rest ]) do
        [ fun.(first) | map_elements(fun, rest) ]
    end

    def map_elements(_, []) do
        []
    end


    #
    # Access elements of a list, through an anonymous function.
    # Allowing the passing of an external value, for it to be 
    # used within the anonymous function. 
    #   
    #   elms = fn(value, x) ->
    #       IO.puts("value: #{value}, list element: #{x}")
    #   end
    #   
    #   access_elements_pvalue(elms, 333, [ 1, 2, 3 ])
    #

    def access_elements_pvalue(fun, value, [ first | rest]) do
        fun.(value, first)
        access_elements_pvalue(fun, value, rest)
    end

    def access_elements_pvalue(_, _, []) do 
        :ok
    end


    # 
    # Converts a number to a list of numbers. The list returned by 
    # the function is only composed of numbers, not characters from 
    # the ASCII table. 
    # 
    #   list = number_to_list(333)
    # 

    def number_to_list(value) do
        items = :erlang.integer_to_list(value)

        adjust = fn(x) ->
            x - 48 # ASCII table.
        end

        map_elements(adjust, items)
    end


    # 
    # Perform the sum of all elements of a list. The function 
    # returns only an integer. 
    # 
    #   total = sums_elements(list)
    # 

    def sums_elements(list) do
        sums_elements(list, 0)
    end

    def sums_elements([ first | rest ], number) do
        sums_elements(rest, first + number)
    end

    def sums_elements([], number) do
        number
    end


    # 
    # Returns the first N elements of a list.
    # 
    #   elements = get_first_items([1, 2, 3], 2)
    # 

    def get_first_items(list, total) do
        get_first_items(list, total, 0, [])
    end

    def get_first_items([ h | r], total, count, list) do
        if count < total do
            get_first_items(r, total, count + 1, [ h | list ])
        else
            get_first_items(r, total, count + 1, list)
        end
    end

    def get_first_items([], _, _, list) do
        reverse_list(list)
    end


    # 
    # Returns the last N elements of a list.
    #
    #   elements = get_last_items([1, 2, 3], 2)
    #

    def get_last_items(start_list, start_amount) do
        if start_amount <= 0 do
            IO.puts("Invalid amount.")
            false
        else
            size   = get_list_size(start_list)
            amount = adjust_amount(start_amount, size)
            offset = (size - amount) + 1

            get_last_items(start_list, offset, 1, [])
        end
    end


    def get_last_items([ head | rest ], offset, index, list) when index >= offset do
        get_last_items(rest, offset, index + 1, [ head | list ])
    end

    def get_last_items([ _ | rest ], offset, index, list) do
        get_last_items(rest, offset, index + 1, list)
    end

    def get_last_items([], _, _, list) do
        reverse_list(list)
    end


    def adjust_amount(amount, size) when amount > size do
        size
    end

    def adjust_amount(amount, _) do
        amount
    end


    #
    # Returns the number of elements in a list.
    #
    #   size = get_list_size([1, 2, 3])
    #

    def get_list_size(list) do
        get_list_size(list, 0)
    end

    def get_list_size([ _ | rest ], count) do
        get_list_size(rest, count + 1)
    end

    def get_list_size([], count) do
        count
    end


    #
    # Reverses the order of elements in a list. 
    # 
    #   list = reverse_list([1, 2, 3])
    # 

    def reverse_list(list) do
        reverse_list(list, [])
    end

    def reverse_list([ head | rest ], list) do
        reverse_list(rest, [ head | list])
    end

    def reverse_list([], list) do
        list
    end


    #
    # Converts a list of integers to the equivalent string 
    # representation according to the ASCII table.
    #
    #   string = list_numbers_to_string([1, 2, 3])
    #

    def list_numbers_to_string(list) do

        convert = fn(x) -> 
            x + 48 # ASCII table.
        end

        map_elements(convert, list)
    end


    # 
    # Divides a list of numbers into groups containing N-elements.
    # You can adjust the alignment of the parts.
    # 
    # block_size - integer: 
    #   The size of the digit block.
    # 
    # adjust - integer: 
    #   Adjusts the alignment of blocks, if necessary.
    #   
    #   0 = Right - Align the blocks to the right -> [ 12,  234, 234 ].
    #   1 = Left  - Align the blocks to the left  -> [ 122, 342, 34  ].
    # 
    # Example of use:
    # 
    #   blocks = split_list_in_blocks([1, 2, 3, 4, 5, 6, 7, 8], 3, 0)
    #   IO.puts("Blocks: #{ inspect(blocks) }") 
    # 
    # Output:
    # 
    #   blocks: [12, 345, 678]
    # 

    def split_list_in_blocks(_, block_size, _) when block_size <= 0 do
        IO.parts("Invalid block size.")
        []
    end

    def split_list_in_blocks(list, block_size, adjust) do
        size = get_list_size(list)

        if block_size > size do
            IO.parts("Invalid block size.")
            []
        else
            process_split_blocks(list, size, block_size, adjust)
        end
    end

    def process_split_blocks(list, size, block_size, adjust) do
        v = (rem(size, block_size) == 0)

        if v == true do
            process_stable_blocks(list, block_size, 1, [], [])
        else
            process_segmented_blocks(list, size, block_size, adjust)
        end
    end

    def process_stable_blocks([ h | r ], block_size, count, block, blocks) do
        size = get_list_size(block)

        if size == block_size do
            block_item = prepare_block_number(block)
            process_stable_blocks(
                r, block_size, count + 1, [ h ], [ block_item | blocks])
        else
            process_stable_blocks(
                r, block_size, count + 1, [ h | block ], blocks)
        end
    end
        
    def process_stable_blocks([], _, _, block, blocks) do
        block_item = prepare_block_number(block)
        reverse_list([ block_item | blocks ])
    end

    def prepare_block_number(list) do
        a = reverse_list(list)
        b = list_numbers_to_string(a)
        { number, _ } = :string.to_integer(b)
        number
    end

    def process_segmented_blocks(list, size, block_size, adjust) do
        mod    = rem(size, block_size)
        offset = trunc(size / block_size)
        check  = (offset * block_size) + mod

        if check == size do

            # Right adjust.
            if adjust == 0 do
                a = (offset * block_size)
                b = get_last_items(list, a)
                c = get_first_items(list, mod)
                number = prepare_block_number(reverse_list(c))
                blocks = process_stable_blocks(b, block_size, 1, [], [])
                [ number | blocks ]
                
            # Left adjust.
            else
                a = (offset * block_size)
                b = get_first_items(list, a)
                c = get_last_items(list, mod)
                number = prepare_block_number(reverse_list(c))
                blocks = process_stable_blocks(b, block_size, 1, [], [])
                blocks ++ [ number ]
            end

        else
            IO.puts("invalid number to convert.")
            []
        end
    end

end


