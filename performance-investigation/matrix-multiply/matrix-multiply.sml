

    fun main() =
    let
        val args = CommandLine.arguments()
        val _ = if length args < 1
            then raise Fail "must have argument" else ()
        val nop = Int.fromString (hd args)
        val n = case nop of SOME n => n | NONE => raise Fail "arg must be int"
        val r = Random.rand (100,200)
        val ma= List.tabulate(n, (fn _ => 
            List.tabulate(n, fn _ => Random.randReal r)))
        val mb= List.tabulate(n, (fn _ => 
            List.tabulate(n, fn _ => Random.randReal r)))
        val sTime =  Time.now()
        val mc = List.tabulate(n, (fn i =>
            List.tabulate(n, fn j =>
                foldr (Real.+) 0.0
                (List.tabulate(n, fn k => 
                    List.nth(List.nth(ma, i), k) *
                    List.nth(List.nth(mb, k), j)
                ))
            )
        ))
        val fTime = Time.now()
        val runDuration : Time.time = Time.-(fTime,sTime)
        val seconds : real = Real.fromLargeInt (Time.toNanoseconds runDuration) * 0.000000001
        val _ = print (Real.fmt (StringCvt.FIX (SOME 10)) seconds)
        val _ = print "\n"
        val _ = TextIO.flushOut TextIO.stdOut

    in
        ()
    end;


main();