
defmodule Division do
    def divisible_by(value, number) do
        case number do
            2  -> rem(value, number) == 0
            3  -> false
            4  -> false
            5  -> false
            6  -> false
            7  -> false
            8  -> false
            9  -> false
            10 -> false
            11 -> false
            12 -> false
            15 -> false
            25 -> false
            _  -> 
                IO.puts("Division by #{number} not supported.")
                false
        end
    end
end


