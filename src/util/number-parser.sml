
structure NumberParser = struct


    datatype parseResult = NPInt of int | NPReal of real

    exception NumberParserMalformed of UTF8String.t * string

    val ~=  = UTF8Char.~=
    infix 4 ~=


  fun parseInteger (s : UTF8String.t) : int = 
    let open UTF8Char
    in
    foldl (fn (c, acc) => 
        if c ~= (UTF8Char.fromString "零" NONE)
        then acc * 10 + 0
        else 
        if c ~= (UTF8Char.fromString "一" NONE)
        then acc * 10 + 1
        else 
        if c ~= (UTF8Char.fromString "二" NONE)
        then acc * 10 + 2
        else 
        if c ~= (UTF8Char.fromString "三" NONE)
        then acc * 10 + 3
        else 
        if c ~= (UTF8Char.fromString "四" NONE)
        then acc * 10 + 4
        else 
        if c ~= (UTF8Char.fromString "五" NONE)
        then acc * 10 + 5
        else 
        if c ~= (UTF8Char.fromString "六" NONE)
        then acc * 10 + 6
        else 
        if c ~= (UTF8Char.fromString "七" NONE)
        then acc * 10 + 7
        else 
        if c ~= (UTF8Char.fromString "八" NONE)
        then acc * 10 + 8
        else 
        if c ~= (UTF8Char.fromString "九" NONE)
        then acc * 10 + 9
        (* else raise PreprocessMalformedPrecedence s *)
        else raise NumberParserMalformed (s, ("can only contain number literals"))
    ) 0 s
    end

    fun parseNumber ( s : UTF8String.t) : parseResult = 
    if UTF8String.isSubstring (UTF8String.fromString "点") s
    then let 
        val parts = UTF8String.fields (fn c =>  c ~= (UTF8Char.fromString "点" NONE)) s
        in if length parts <> 2
        then raise NumberParserMalformed (s, "cannot have more than two 点's")
        else let val integralPart = parseInteger (List.nth(parts,0))
                 val decimalPart = parseInteger (List.nth(parts,1))
                 val finalValue = Real.fromInt integralPart + Math.pow((0.1), 
                            Real.fromInt ((length (List.nth(parts,1))))) * Real.fromInt decimalPart
                val _ = DebugPrint.p 
               ( " Integral part is " ^ Int.toString integralPart ^
                 " decimal part is " ^ Int.toString decimalPart  ^ 
                 " finalValue is " ^ Real.toString finalValue ^ "\n")

                in NPReal finalValue end
        end
    else NPInt (parseInteger s)


    fun isNumber(s : UTF8String.t) : bool = 
        List.all (fn c => UTF8String.containsChar (UTF8String.fromString("零一二三四五六七八九十点")) (c)) s
    
end