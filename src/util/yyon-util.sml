
structure YYONUtil = struct


    exception ParseFailure of MixedStr.t * string

    (* returns rest of the string if mathces, NONE if not *)
    fun matchChar (c : UTF8Char.t) (input : MixedStr.t) : MixedStr.t option = 
        case input of 
        [] => NONE
        | (h :: t) =>  if MixedStr.isChar h c then SOME t else NONE
        
    type 'a parseResult = 'a * MixedStr.t
    type 'a parser = MixedStr.t -> 'a parseResult
    fun fmapP (f : 'a -> 'b) (pa : 'a parser) : 'b parser = fn input =>
        case pa input of (r, resStr) => (f r, resStr)
    fun fmapPr (f : 'a -> 'b) (res : 'a parseResult) : 'b parseResult = 
        case res of (r, resStr) => (f r, resStr)
            
    fun alternatives (options : (UTF8Char.t * (MixedStr.t -> 'a parseResult)) list) 
           : 'a parser=  fn input => 
    let val res = 
        foldl (fn ((option, k), acc) => 
        case acc of 
            SOME j => acc
            | NONE => (case matchChar option input of 
                    SOME rest => SOME(k rest)
                    | NONE => NONE
                    )

            ) NONE options
    in case res of 
        SOME YYON => YYON
        | NONE => raise ParseFailure (input, "Expected one of " ^ UTF8String.toString (map (fn x => #1 x) options))
    end


    val allValidNumberStrs = UTF8String.fromString "零一二三四五六七八九十点"
    fun scanNumbers (input: MixedStr.t) : UTF8String.t * MixedStr.t = 
        case input of 
            [] => ([], input)
            | (h :: t) => if List.exists (fn c => MixedStr.isChar h c) allValidNumberStrs
                          then case scanNumbers t of (n,  remaining) => (MixedStr.getChar h :: n, remaining)
                          else ([], input)

    fun parseNumber():  NumberParser.parseResult parser = 
        fn input =>
            let val (number, rest) = scanNumbers input
            in (NumberParser.parseNumber number, rest)
            end

     fun parseLiteral():  UTF8String.t parser = fn input =>
        case input of 
                (MixedStr.Literal s :: rest) => (s, rest)
                | _ => raise ParseFailure (input, "Expected Literal following 之")


     fun parseBool():  bool parser = 
    alternatives [
        (UTF8Char.fromString "阴" NONE, (fn res => (false, res))),
        (UTF8Char.fromString "阳" NONE, (fn res => (true, res)))
    ]

    fun parseObject (): (UTF8String.t * YYON.value) list parser = 
        fn input => 
        case input of 
            [] => raise ParseFailure ([], "Unexpected EOF")
            | (h :: t) => if MixedStr.isChar h (UTF8Char.fromString "也" NONE)
                          then ([], t)
                          else if MixedStr.isChar h (UTF8Char.fromString "之" NONE)
                               then let val (name, objRest) = parseLiteral() t
                                        val (nextObject, rest) = parseYYON() objRest
                                            val (l, remaining) = parseObject() rest
                                            in ((name, nextObject)::l, remaining ) end
                               else raise ParseFailure (input, "Expected 也 or 之")
    and parseList (): (YYON.value) list parser = 
        fn input => 
        case input of 
            [] => raise ParseFailure ([], "Unexpected EOF")
            | (h :: t) => if MixedStr.isChar h (UTF8Char.fromString "也" NONE)
                          then ([], t)
                          else let val (nextObject, rest) = parseYYON() input
                                   val (l, remaining) = parseList() rest
                                   in (nextObject :: l , remaining)
                                   end


    and parseYYON() : YYON.value parser = 
            alternatives [
                (UTF8Char.fromString "物" NONE, 
                     fmapP YYON.OBJECT (parseObject ())),
                (UTF8Char.fromString "爻" NONE, 
                    fmapP YYON.BOOL (parseBool ())),
                (UTF8Char.fromString "数" NONE, 
                    fmapP (fn x => case x of 
                    NumberParser.NPInt i => YYON.INT i 
                    | NumberParser.NPReal r => YYON.FLOAT r) (parseNumber ())),
                (UTF8Char.fromString "列" NONE, 
                    fmapP YYON.ARRAY (parseList ())),
                (UTF8Char.fromString "言" NONE, 
                    fmapP YYON.STRING (parseLiteral ()))
            ] 
        

    fun parseYYONWithEOF(content : UTF8String.t): YYON.value = 
        let val (obj, rest) = parseYYON() (StaticErrorStructure.valOf (MixedStr.make content))
        (* TODO : fix valOf *)
        in case rest of 
            [] => obj
            | _ => raise ParseFailure(rest, "Expected EOF, extraneous content")
        end
    
    fun loadYYONFromFile(filepath : string) = 
        parseYYONWithEOF (UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn filepath)) filepath) 


    exception YYONAccessError 
    fun asObject v = case v of 
        YYON.OBJECT v => v
        | _ => raise YYONAccessError

    fun asString v = case v of 
        YYON.STRING v => v
        | _ => raise YYONAccessError



end