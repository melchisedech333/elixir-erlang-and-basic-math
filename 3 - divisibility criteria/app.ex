
# Iesus Hominum Salvator <3

import Utils, only: [access_elements_pvalue: 3]

defmodule IHS do
    defmodule Fiat do
        def lux() do

            options = [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 25 ]
            value   =   10

            elms = fn(value, x) ->
                IO.puts("value: #{value}, x: #{x}")
            end

            access_elements_pvalue(elms, value, options)

        end
    end
end

IHS.Fiat.lux()


