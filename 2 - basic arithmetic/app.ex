
# IHS <3

import DivisibilityCriteria, only: [divisible_by: 2]

defmodule IHS do
    defmodule Fiat do
        def lux() do
            IO.puts divisible_by(1, 2)
        end
    end
end

IHS.Fiat.lux()


