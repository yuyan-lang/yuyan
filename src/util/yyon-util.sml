
structure YYONUtil = struct

    exception ParseFailure of MixedStr.t

    (* returns rest of the string if mathces, NONE if not *)
    fun matchChar (c : UTF8Char.t) (input : MixedStr.t) : MixedStr.t option = 
        case input of 
        [] => false
        | (h :: t) =>  if MixedStr.isChar h c then SOME t else NONE
            
    fun alternatives (options : (UTF8Char.t * (MixedStr.t -> JSON.value)) list) (input : MixedStr.t) : JSON.value = 
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
        SOME json => json
        | NONE => raise ParseFailure input
    end
    fun parseObject (input : MixedStr.t) : (string * JSON.value) list = 
    raise Fail ""

    fun parseMixedStr (input : MixedStr.t) : JSON.value = 
    raise Fail ""
        (* alternatives
            [
                UTF8Char.fromString "物", (fn rest => OBJECT (parseObject rest))
            ] *)
        

    fun parseYYON(content : UTF8String.t): JSON.value = 
        let val s =  MixedStr.make content
        in parseMixedStr s end
    
    fun loadYYONFromFile(filepath : string) = 
        parseYYON (UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn filepath)) filepath) 

end