
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

            IO.puts("Division:\n")
            division()
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
            IO.puts([
                "\tCommutative: ",
                "#{ 10 * 20 * 30 * 40 } = ",
                "#{ 40 * 30 * 20 * 10 } = ",
                "#{ 30 * 10 * 40 * 20 }"
            ])

            IO.puts([
                "\tAssociative: ",
                "#{ 10 *  20 * (30  * 40) } = ",
                "#{ 40 *  30 * (20  * 10) } = ",
                "#{ 30 * (20 *  40) * 10  }"
            ])

            IO.puts([
                "\tDistributive: ",
                "#{  10 * (20 + 30) } = ",
                "#{ (10 * 20) + (10 * 30) }"
            ])

            IO.puts([
                "\tNeutral element: ",
                "#{ 10 *  1 }, ",
                "#{ 20 *  1 }, ",
                "#{ 1  * 10 }, ",
                "#{ 1  * 20 }"
            ])

            IO.puts([
                "\tAnnulment: ",
                "#{ 10 *  0 }, ",
                "#{ 20 *  0 }, ",
                "#{ 0  * 10 }, ",
                "#{ 0  * 20 }\n"
            ])

            mult = fn x, y -> IO.puts("\t#{x} * #{y} = #{x * y}") end

            mult.(0.2, 0.3)
            mult.(1.2, 0.4)
            mult.(-12.0, 5.0)
            mult.(5.0, -12.0)
            mult.(15.0, 0.02)
            mult.(15.0, 0.2)
            mult.(0.2, 0.3)
            mult.(2.0 / 10.0, 3.0 / 10.0)
            mult.(1.2, 0.4)
            mult.(12.0 / 10.0, 4.0 / 10.0)
            mult.(29.01, 4.0)
            mult.(2901.0 / 100.0, 40.0 / 10.0)
            
            separator()
        end

        def division do
            quotient = 4.0
            divider  = 2.0
            rest     = 1.0
            dividend = divider * quotient + rest

            IO.puts("\t#{ dividend } = #{ divider } x #{ quotient } + #{ rest }")
            IO.puts("\t#{ dividend } / #{ divider } = #{ dividend / divider }")

            div = fn x, y -> IO.puts("\t#{x} / #{y} = #{x / y}") end

            div.(433.0, 6.0)
            div.(8.0, 1230.0)
            
            separator()
        end

        def separator() do IO.puts("\n") end
    end
end

IHS.Fiat.lux()


