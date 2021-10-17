
structure BuiltinOperators =
struct


    val allOps : Operators.operator list = List.concat [
            NatOps.natOps
            , GroupOps.groupOps
            , 
            FunctionOps.functionOps
            ]
end