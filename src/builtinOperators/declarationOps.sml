structure DeclarationOps =
struct
    val declJudgmentOp = Operators.parseOperatorStr "〇者〇也" false false 500 []
    val declarationOps = [
        declJudgmentOp
    ]
end
