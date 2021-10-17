
structure BuiltinOperators =
struct


    val allOps : Operators.operator list = List.concat [
             GroupOps.groupOps
            (* , NatOps.natOps
            , FunctionOps.functionOps *)
            ,DeclarationOps.declarationOps
            ]
end