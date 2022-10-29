
# IHS <3

import Division, only: [divisible_by: 2]

defmodule IHS do
    defmodule Fiat do
        def lux() do
            IO.puts divisible_by(5, 3)
        end
    end
end

IHS.Fiat.lux()


