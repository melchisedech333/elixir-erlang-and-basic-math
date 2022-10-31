
import Utils, only: [number_to_list: 1, sums_elements: 1, get_last_items: 2,
                     list_numbers_to_string: 1, split_list_in_blocks: 3,
                     map_elements: 2]

defmodule Division do
    def divisible_by(value, number) do
        case number do
            2  -> rem(value, number) == 0
            3  -> divisible_by_3(value)
            4  -> divisible_by_4(value)
            5  -> divisible_by_5(value)
            6  -> divisible_by_6(value)
            7  -> divisible_by_7(value)
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


    def divisible_by_3(value) do
        list  = number_to_list(value)
        total = sums_elements(list)
        rem(total, 3) == 0
    end


    def divisible_by_4(value) when value <= 99 do
        rem(value, 4) == 0
    end

    def divisible_by_4(value) when value > 99 do
        list = number_to_list(value)
        elem = get_last_items(list, 2)
        last = list_numbers_to_string(elem)
        { num, _ } = :string.to_integer(last)
        rem(num, 4) == 0
    end

    def divisible_by_4(_) do
        IO.puts("invalid number (divisible by 4).")
        false
    end


    def divisible_by_5(value) do
        list = number_to_list(value)
        elem = get_last_items(list, 1)
        last = list_numbers_to_string(elem)
        { num, _ } = :string.to_integer(last)

        case num do
            5 -> true
            0 -> true
            _ -> false
        end
    end

    
    def divisible_by_6(value) do
        (rem(value, 2) == 0) and (divisible_by_3(value) == true)
    end


    def divisible_by_7(value) do
        list = number_to_list(value)
        blocks = split_list_in_blocks(list, 3, 0)

        rest = map_elements(fn(x) ->
            rem(x, 7) 
        end, blocks)

        IO.puts("rest: #{ inspect(rest) }")

        # process_rest_calc(Rest, 1, 0, 0, 0, 0) rem 7 == 0.
    end
end


