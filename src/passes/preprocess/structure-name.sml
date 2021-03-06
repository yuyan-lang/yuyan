structure StructureName =
struct
type structureName = UTF8String.t list
type t = structureName
val separatorChar = [UTF8Char.fromString "之" NONE]
val separatorCharDebug = UTF8String.fromString "->"
    fun toString x = case x of
        [x] => x
        | (x :: xs) => x@separatorChar@toString xs
        | [] => UTF8String.fromString "【错误：空结构名】"

    fun toStringDebug x = case x of
        [x] => x
        | (x :: xs) => x@separatorCharDebug@toStringDebug xs
        | [] => UTF8String.fromString "【错误：空结构名】"

    fun toStringPlainDebug x = UTF8String.toString (toStringDebug x)
    fun toStringPlain x = UTF8String.toString (toString x)
    val topLevelName = [
        UTF8String.fromString "《《顶层结构》》"
        ]
    fun localName () = [UTF8String.fromString ("《《临时结构" ^ Int.toString(UID.next()) ^ "》》")]
    fun searchPathName() = [UTF8String.fromString ("《《搜索路径" ^ Int.toString(UID.next()) ^ "》》")]
    fun metaVarName() = [UTF8String.fromString ("《《元变量" ^ Int.toString(UID.next()) ^ "》》")]
    fun binderName () =  UTF8String.fromString ("绑定" ^Int.toString (UID.next()))

    fun getDeclaringScope (s : structureName) = 
        case s of 
        [] => raise Fail "sn28: cannot get declaring scope name of empty structure"
        | _ => List.take(s, length s - 1)
    fun last (s : structureName) = 
        case s of 
        [] => raise Fail "sn28: cannot get last name of empty structure"
        | _ => List.last(s)

    fun semanticEqual (s1 : structureName) (s2 : structureName) = 
    case (s1, s2) of
         ([], []) => true
        | (c1::s1tl, c2::s2tl) => if UTF8String.semanticEqual c1 c2 andalso semanticEqual s1tl s2tl then true else false
        | _ => false

    fun isPrefix(s1 : structureName) (s2 : structureName) = 

        if List.length s1 > List.length s2 then false else
        case (s1, s2) of
            (c1::s1tl, c2::s2tl) => if UTF8String.semanticEqual c1 c2 andalso isPrefix s1tl s2tl
                                    then true
                                    else false
            | ([], _) => true
            | _ => false


    fun stripPrefix(s1 : structureName) (s2 : structureName) = 

        if List.length s1 > List.length s2 then 
        raise Fail "Cannot strip prefix" else
        List.drop(s2, length s1)

    (* strips the prefix of s2 on agreed parts of s1 *)
    fun stripPrefixOnAgreedParts(s1 : structureName) (s2 : structureName) = 
        case (s1, s2) of
            (c1::cs1, c2::cs2) => if UTF8String.semanticEqual c1 c2 then stripPrefixOnAgreedParts cs1 cs2
                                  else s2
            | _ => s2
        
    fun getAgreedPrefixParts(s1 : structureName) (s2 : structureName) = 
        case (s1, s2) of
            (c1::cs1, c2::cs2) => if UTF8String.semanticEqual c1 c2 then c2::(getAgreedPrefixParts cs1 cs2)
                                  else []
            | _ => []

(* determine if curSName |-> toCheck refers to referred through the following basic rules:
1. A refers to A (reflexive) return A
2.  A->B->E->F |-> C->D refers to A->B->C->D returns A->B->C->D
3. A->B->E->F |-> D does not refer to A->B->C->D 

returns the canonical name (adding curSName if ommitted)
*)
 
    fun checkRefersTo(referred : structureName) (toCheck : structureName) (curSName : structureName) : structureName option = 
    (
        let 
        (* val _ = DebugPrint.p ("checking refers to referred=" ^ toStringPlainDebug referred ^
            " toCheck=" ^  toStringPlainDebug toCheck ^
            " curSName=" ^ toStringPlainDebug curSName) *)
            val res = 
      if semanticEqual referred toCheck then SOME(toCheck) else 
            if semanticEqual (stripPrefixOnAgreedParts curSName referred) toCheck
            then SOME((getAgreedPrefixParts curSName referred)@(stripPrefixOnAgreedParts curSName referred))
            else NONE
            (* val _ = DebugPrint.p ("result is " ^ (case res of 
                SOME _ => "true" 
                | NONE => "false")) *)


        in 
        res
        end
    )

(* determine if the scope reference curSName |-> toCheck includes referred through the following basic rules:
1. A |-> B  refers to B->C return  C
1. A |-> B  refers to A->B->C return  C
1. A |-> B does not refer to A->B  returns NONE
2.  A->B->E->F |-> C->D refers to A->B->C->D->E returns E
3. A->B->E->F |-> D does not refer to A->B->C->D 
returns the name removed of all scopes
*)
 
    fun checkRefersToScope(referred : structureName) (openScopeName : structureName) (curSName : structureName) : structureName option = 
    (
        let 
        (* val _ = DebugPrint.p ("checking refers (scope) to referred=" ^ Int.toString (length referred) ^ toStringPlainDebug referred  ^
            " toCheck=" ^ Int.toString(length openScopeName) ^  toStringPlainDebug openScopeName ^
            " curSName=" ^ Int.toString (length curSName) ^ toStringPlainDebug curSName) *)
            val curSNameReferred = stripPrefixOnAgreedParts curSName referred
            val res = 
            if isPrefix openScopeName curSNameReferred 
            then let val remainingName = stripPrefix (openScopeName) curSNameReferred
                 in  if length remainingName = 0 then NONE else SOME(remainingName)
                 end
            else NONE
            (* val _ = DebugPrint.p ("result is " ^ (case res of 
                SOME _ => "true\n" 
                | NONE => "false\n")) *)


        in 
        res
        end
    )
end
