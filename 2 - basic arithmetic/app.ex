
# Iesus Hominum Salvator <3

defmodule IHS do
    defmodule Fiat do
        def lux() do
            IO.puts("Operations:\n")
            operations()

            IO.puts("Signals:\n")
            signals()

            IO.puts("Addition:\n")
            addition()

            IO.puts("Multiplication:\n")
            multiplication()
        end

        def operations() do
            a = 10
            b = 20

            calc = fn 
                a, b, c when c == '+' -> a + b
                a, b, c when c == '-' -> a - b
                a, b, c when c == '*' -> a * b
                a, b, c when c == '/' -> a / b
            end

            IO.puts("\t#{a} + #{b} = #{ calc.(a, b, '+' ) }")
            IO.puts("\t#{a} - #{b} = #{ calc.(a, b, '-' ) }")
            IO.puts("\t#{a} * #{b} = #{ calc.(a, b, '*' ) }")
            IO.puts("\t#{a} / #{b} = #{ calc.(a, b, '/' ) }")

            separator()
        end

        def signals() do
            show = fn a, b -> IO.puts("\t#{a} = #{b}") end

            show.(
                (4) + (-2) + (1) - (1) ,
                 4     -2  +  1  -  1  
            )

            show.(
                (-6) - (10) + (-3) ,
                 -6  -  10     -3
            )

            show.(
                (10) + (-5) - (-10) ,
                 10     -5  +   10
            )

            show.(
                (15) + (-10) - (4) + ( 20 - 30) + (-1),
                 15     -10  -  4  + (-10     )    -1
            )

            show.(
                200 + (-23) - (-30) + 78 + (50 / 100) - 10,
                200    -23  +   30  + 78 + (50 / 100) - 10
            )

            separator()
        end

        def addition() do    
            IO.puts([ "\tCommutative: ",
                "#{ 10 + 20 + 30 } = ", 
                "#{ 30 + 20 + 10 } = ", 
                "#{ 20 + 10 + 30 }"
            ])

            IO.puts([ "\tAssociative: ", 
                "#{ (10 +  20) + 30  } = ",
                "#{ (30 +  20) + 10  } = ",
                "#{ (20 +  10) + 30  } = ",
                "#{  10 + (20  + 30) } = ",
                "#{  30 + (10  + 20) } = ",
                "#{  20 + (30  + 10) }"
            ])

            separator()
        end

        def multiplication() do
            
        end

        def separator() do IO.puts("\n") end
    end
end

IHS.Fiat.lux()


