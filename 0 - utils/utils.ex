
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

end


