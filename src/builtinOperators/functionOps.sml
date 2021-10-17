

structure FunctionOps =
struct
  val lambdaOp = Operators.parseOperator "遇〇而〇" true false 500 [1]
  val applicationOp = Operators.parseOperator "〇于〇" true true 800 []
  val functionOps = [ 
      lambdaOp
      , applicationOp
  ]
end