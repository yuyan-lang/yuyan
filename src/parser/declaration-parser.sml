
structure DeclarationParser =
struct
    open ParseAST
    type parser = UTF8String.t -> (operator * UTF8String.t list) list 
    (* declarations are top level constructs that ignore internal expression structure, 
    but they contain arbitrary unknown names and structured parenthesis *)

    fun parseDeclaration(ops : operator list) = fn exp => raise Fail " Still considering"
        
end