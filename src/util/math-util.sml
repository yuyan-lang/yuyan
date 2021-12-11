
structure MathUtil = struct

    fun power (base: int) (e : int) = if e = 0 then 1 else base * (power base (e-1))
end