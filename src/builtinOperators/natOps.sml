
structure NatOps =
struct
  val oneOp = Operators.parseOperator "一" false false 1000
  val plusOp = Operators.parseOperator "〇加〇" true true 900
  val minusOp = Operators.parseOperator "〇减〇" true true 900
  val natOps = [oneOp, plusOp, minusOp ]
end