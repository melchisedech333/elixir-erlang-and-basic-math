
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

    def map_elements(fun, []) do
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

    def access_elements_pvalue(fun, value, []) do 
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

    def get_last_items([ head | rest ], offset, index, list) do
        get_last_items(rest, offset, index + 1, list)
    end

    def get_last_items([], offset, index, list) do
        reverse_list(list)
    end


    def adjust_amount(amount, size) when amount > size do
        size
    end

    def adjust_amount(amount, size) do
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

    def get_list_size([ first | rest ], count) do
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

end


