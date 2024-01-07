structure Fib = struct
fun fib (n : int) : int = (if n < 2 then n else fib (n-1) + fib (n-2))
val _ = print (Int.toString (fib (10)))
end
