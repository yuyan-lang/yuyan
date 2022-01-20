structure URIUtil = struct 
    fun cutOffProtocol (uri : string) : string = 
        if String.isPrefix "file://" uri
        then String.extract (uri, String.size "file://", NONE)
        else uri

    fun parseHex (c : char) : int = 
    case c of
        #"0" => 0
        | #"1" => 1
        | #"2" => 2
        | #"3" => 3
        | #"4" => 4
        | #"5" => 5
        | #"6" => 6
        | #"7" => 7
        | #"8" => 8
        | #"9" => 9
        | #"A" => 10
        | #"B" => 11
        | #"C" => 12
        | #"D" => 13
        | #"E" => 14
        | #"F" => 15
        | _ => raise Fail "uu25"

    fun decodePercent (uri : char list) :char list = 
        case uri of 
        (#"%":: a :: b :: rest) => Char.chr (16 * (parseHex a) + parseHex b) :: decodePercent rest
        | h :: t => h  :: decodePercent t
        | [] => []

    fun decodePercentStr (s : string ) : string = 
        String.implode (decodePercent (String.explode s))

    fun uriToPath (uri : string) : string = 
        decodePercentStr (cutOffProtocol uri)


    fun pathToUri (path : string) : string = 
        "file://" ^ path

end
