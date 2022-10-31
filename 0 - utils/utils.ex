
defmodule Utils do

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

end


