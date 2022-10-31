
# Iesus Hominum Salvator <3

import Utils, only: [access_elements_pvalue: 3]
import Division, only: [divisible_by: 2]

defmodule IHS do
    defmodule Fiat do
        def lux() do

            options = [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 25 ]
            value   =   30

            elms = fn(value, x) ->
                result = divisible_by(value, x)
                IO.puts("#{value} is divisible by #{x}: #{result}")
            end

            access_elements_pvalue(elms, value, options)

        end
    end
end

IHS.Fiat.lux()


